#!/usr/bin/perl
package ButtonGesture::Recognizer;

use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(min max);
use YAML::XS qw(LoadFile);

our $VERSION = '0.21';

# -------------------------------------------------------------------
# Overview
# -------------------------------------------------------------------
# Stream edges via feed_event('press'|'release', t).
# Quantize into '.' (press/hold) and '~' (pause) using quantum_ms.
#
# Matching (dot/tilde only):
#   • One global time-stretch s ∈ [stretch_min,stretch_max] for a candidate.
#   • Per-symbol elasticity (quanta) forgives small deviations.
#   • Pattern weight biases ties (“prefer when in doubt”).
#
# Key behavior for single→double ambiguity:
#   • We add a *virtual trailing run* at evaluation time that grows with real time
#     ('.' if last edge was press; '~' if last edge was release). This makes both
#     full scores and longer-pattern upper bounds evolve as you wait.
#   • A pattern written with a trailing ')' means “terminal: ignore penalty on
#     the last run if it is the (growing) virtual run”. This preserves a single
#     click’s score as the pause grows, while the longer pattern’s UB changes.
#
# Decision:
#   • Evaluate on releases and idle ticks.
#   • If best FULL candidate beats the best upper bound (UB) of any longer
#     pattern by an effective margin, COMMIT immediately.
#   • Otherwise wait—up to a dynamic window (based on remaining time a longer
#     supersequence could still take, using stretch_max) capped by decision_timeout_s.
#
# YAML schema:
#   gestures: [ { name, pattern, weight, elasticity:{'.', '~'} }, ... ]
#   or a top-level array of the same entries.
#   Suffix markers (optional):
#     ')'  → terminal: do not penalize last run if it is the virtual run
#     '>'  → advisory “often a prefix” (used only for debug; matching unaffected)
#
# Tunables (constructor args) — behavior & practical ranges:
#   quantum_ms        : ms per quantum (default 20). Typical 15–25.
#   idle_timeout_s    : idle gap before evaluating in tick() (default 0.25).
#   commit_threshold  : min FULL score to be eligible (default 0.75).
#   commit_margin     : margin vs best UB to commit now (default 0.10).
#   wait_preference   : 0..1; inflates effective margin & dynamic wait (default 0.60).
#   decision_timeout_s: max patience once a FULL exists (default 0.30; use 0.30–0.40 for double-click).
#   stretch_min/max   : global time-scale bounds (default 0.5..2.0).
#   verbose           : 0 silent; 1 compact lines; 2 +tables+viz; 3 deep; timestamps always shown.
#   gestures|config_file|callback
#
# Visualization (verbose >= 2):
#   • Prints the live observed string (with virtual tail) and a candidate table.
#   • Colorizes per-run matching: '.' shades of green, '~' shades of blue.
#   • Mapping and scale can be overridden via viz_* args.
#
# KEY:
#   DEBUG:verbose_*   — debug/logging controls and call sites
#   VIS:viz_*         — visualization controls and helpers
#
# -------------------------------------------------------------------

sub new {
    my ($class, %a) = @_;

    my $self = bless {
        # config / I-O
        config_file        => $a{config_file} // 'gestures.yaml',
        gestures_raw       => $a{gestures} // [],
        callback           => $a{callback} // sub { print "Action: $_[0]\n" },

        # DEBUG:verbose_level (kept backward compatible with 'verbose')
        verbose            => $a{verbose} // 0,   # 0 silent; 1 compact; 2 table+viz; 3 deep

        # timing
        quantum_ms         => (defined $a{quantum_ms} ? 0 + $a{quantum_ms} : 20),
        quantum_s          => undef,
        idle_timeout_s     => (defined $a{idle_timeout_s} ? 0.0 + $a{idle_timeout_s} : 0.25),

        # scoring knobs
        commit_threshold   => (defined $a{commit_threshold} ? 0.0 + $a{commit_threshold} : 0.75),
        commit_margin      => (defined $a{commit_margin} ? 0.0 + $a{commit_margin} : 0.10),
        wait_preference    => (defined $a{wait_preference} ? 0.0 + $a{wait_preference} : 0.60),
        decision_timeout_s => (defined $a{decision_timeout_s} ? 0.0 + $a{decision_timeout_s} : 0.30),
        stretch_min        => (defined $a{stretch_min} ? 0.0 + $a{stretch_min} : 0.5),
        stretch_max        => (defined $a{stretch_max} ? 0.0 + $a{stretch_max} : 2.0),

        # VIS: top-level toggles (independent from 'verbose')
        viz_enable         => exists $a{viz_enable} ? !!$a{viz_enable} : 1,
        viz_scale          => (defined $a{viz_scale} ? 0.0 + $a{viz_scale} : 1.0),
        viz_symbols        => $a{viz_symbols} // { '.' => '▉', '~' => '▂' },  # overridable glyphs
        viz_show_colors    => (defined $a{viz_show_colors} ? !!$a{viz_show_colors} : 1),
        viz_abbrev_max     => (defined $a{viz_abbrev_max} ? 0 + $a{viz_abbrev_max} : 60),
        viz_pats_full      => exists $a{viz_pats_full} ? !!$a{viz_pats_full} : 1,
        viz_user_bg_rgb    => $a{viz_user_bg_rgb} // [30,45,50],   # {ubg}  a24bg(30,45,50)
        viz_pat_bg_rgb     => $a{viz_pat_bg_rgb}  // [40,40,40],   # {pbg}  a24bg(40,40,40)

        # scoring emphasis — give presses more influence than pauses; prefer multi-press patterns
        run_weight_dot     => (defined $a{run_weight_dot}   ? 0.0 + $a{run_weight_dot}   : 1.5),
        run_weight_tilde   => (defined $a{run_weight_tilde} ? 0.0 + $a{run_weight_tilde} : 1.0),
        press_bias         => (defined $a{press_bias}       ? 0.0 + $a{press_bias}       : 0.15), # prior for patterns with more '.' runs
        tie_epsilon        => (defined $a{tie_epsilon}      ? 0.0 + $a{tie_epsilon}      : 0.02), # treat scores within ε as a tie

        # runtime state
        _events            => [],     # [ ['press'|'release', t], ... ]
        _last_event_t      => undef,  # time of last edge
        _first_event_t     => undef,  # time of first edge in current sequence
        _last_debug_line   => undef,  # throttle

        # compiled patterns
        patterns           => [],     # [{ name, weight, runs:[{sym,len}], len_total, elasticity{.,~}, terminal, continues }]
        _have_longers      => 0,      # any strict supersequence pair exists?
    }, $class;

    $self->{quantum_s} = $self->{quantum_ms} / 1000.0;

    # VIS: precompute ANSI backgrounds
    $self->{_viz_user_bg} = _ansi_bg(@{$self->{viz_user_bg_rgb}});
    $self->{_viz_pat_bg}  = _ansi_bg(@{$self->{viz_pat_bg_rgb}});

    $self->_load_and_compile();
    return $self;
}

# -------------------------------------------------------------------
# Public API
# -------------------------------------------------------------------

sub feed_event {
    my ($self, $kind, $t) = @_;
    $t //= time();
    die "feed_event kind must be 'press' or 'release'" unless $kind eq 'press' || $kind eq 'release';

    push @{$self->{_events}}, [$kind, $t];
    $self->{_last_event_t} = $t;
    $self->{_first_event_t} = $self->{_first_event_t} // $t;

    # DEBUG:verbose_flow — evaluate right after releases to minimize latency
    $self->_evaluate_if_ready($t) if $kind eq 'release';
}

sub tick {
    my ($self, $t) = @_;
    $t //= time();
    return unless @{$self->{_events}};
    my $since_last = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0;
    if ($since_last >= $self->{idle_timeout_s}) {
        # DEBUG:verbose_flow — idle evaluation (with virtual trailing run)
        $self->_evaluate_if_ready($t);
    }
}

sub periodic_update {    # alias for older callers
    my ($self, $t) = @_;
    $self->tick($t);
}

sub reset {
    my $self = shift;
    $self->{_events}         = [];
    $self->{_last_event_t}   = undef;
    $self->{_first_event_t}  = undef;
    $self->{_last_debug_line}= undef;
}

# -------------------------------------------------------------------
# Internal: Configuration / Compilation
# -------------------------------------------------------------------

sub _load_and_compile {
    my $self = shift;

    my $gest = $self->{gestures_raw};
    if (!@$gest && -f $self->{config_file}) {
        my $raw = eval { LoadFile($self->{config_file}) };
        warn "Config load error: $@" if $@ && $self->{verbose};
        if (!$@ && defined $raw) {
            if (ref($raw) eq 'HASH') {
                $gest = $raw->{gestures} // $raw->{patterns} // [];
            } elsif (ref($raw) eq 'ARRAY') {
                $gest = $raw;
            }
        }
    }

    my @compiled;
    for my $g (@$gest) {
        next unless ref($g) eq 'HASH' && defined $g->{pattern};
        my $name   = defined $g->{name} ? $g->{name} : 'unnamed';
        my $weight = defined $g->{weight} ? 0.0 + $g->{weight} : 1.0;
        my $elas   = $g->{elasticity} // {};
        my ($canon, $terminal, $continues) = _canonicalize_pattern($g->{pattern});
        my $runs = _runs_from_pattern($canon);
        next unless @$runs;
        my $len_total = 0; $len_total += $_->{len} for @$runs;
        my $n_press   = scalar grep { $_->{sym} eq '.' } @$runs;
        my $w_eff     = $weight * (1.0 + ($self->{press_bias} // 0) * ($n_press > 0 ? $n_press - 1 : 0));

        push @compiled, {
            name       => $name,
            weight     => $weight,
            w_eff      => $w_eff,      # effective weight used in scoring (with press_bias prior)
            n_press    => $n_press,    # number of '.' runs (used for tie-breaks)
            runs       => $runs,
            len_total  => $len_total,
            elasticity => { '.' => ($elas->{'.'} // 0), '~' => ($elas->{'~'} // 0) },
            terminal   => $terminal ? 1 : 0, # ignore penalty on last run if it's virtual tail
            continues  => $continues ? 1 : 0,
        };
    }

    @compiled = sort { $a->{len_total} <=> $b->{len_total} } @compiled;

    my $have_longers = 0;
    for my $i (0..$#compiled) {
        for my $j ($i+1..$#compiled) {
            if (_runs_prefix($compiled[$i]{runs}, $compiled[$j]{runs})) {
                $have_longers = 1; last;
            }
        }
        last if $have_longers;
    }

    $self->{patterns}      = \@compiled;
    $self->{_have_longers} = $have_longers;
}

# -------------------------------------------------------------------
# Internal: Evaluation / Decision
# -------------------------------------------------------------------

sub _evaluate_if_ready {
    my ($self, $t) = @_;

    my ($obs_runs, $virtual_sym) = _obs_runs_with_virtual($self->{_events}, $self->{quantum_s}, $t);
    return unless @$obs_runs;

    my @full = ();         # full matches we could commit now
    my $best_upperbound = 0.0;
    my $best_ub_idx     = -1;

    for my $idx (0..$#{$self->{patterns}}) {
        my $p = $self->{patterns}[$idx];

        if (@$obs_runs < @{$p->{runs}}) {
            my $ub = _prefix_upperbound(
                $self, $obs_runs, $p,
                $self->{stretch_min}, $self->{stretch_max}
            );
            $ub *= $p->{w_eff};

            if ($ub > $best_upperbound) {
                $best_upperbound = $ub;
                $best_ub_idx     = $idx;
            }
            next;
        }

        my ($score, $s, $perrun) = $self->_full_score(
            $obs_runs, $obs_runs, $p, $virtual_sym,
            $self->{stretch_min}, $self->{stretch_max}
        );
        next if $score <= 0;
        my $final = $score * $p->{w_eff};
        push @full, { name => $p->{name}, score => $final, raw => $score, s => $s, idx => $idx,
                      perrun => $perrun, press_count => $p->{n_press}, runs_count => scalar(@{$p->{runs}}) };
 
    }

    return unless @full;

    @full = sort { $b->{score} <=> $a->{score} } @full;
    my $best = $full[0];
    # tie-break within ε: prefer patterns with more press runs (more informative), then more total runs
    my @close = grep { $best->{score} - $_->{score} <= $self->{tie_epsilon} } @full;
    if (@close > 1) {
        @close = sort {
            $b->{press_count} <=> $a->{press_count} ||
            $b->{runs_count}  <=> $a->{runs_count}
        } @close;
        $best = $close[0];
    }


    # DEBUG:verbose_dump — timestamps relative to first edge
    my $t0   = $self->{_first_event_t} // $t;
    my $dt   = $t - $t0;
    _debug_dump_observation($self, $obs_runs, $dt) if $self->{verbose} >= 2 && $self->{viz_enable};
    _debug_dump_candidates($self, $obs_runs, \@full, $best_upperbound, $dt) if $self->{verbose} >= 2;

    # DEBUG:verbose_compact — one-liner (throttled)
    if ($self->{verbose} >= 1) {
        my $line = sprintf("[t=+%.03fs] best full: %s score=%.3f, best UB=%.3f\n",
                           $dt, $best->{name}, $best->{score}, $best_upperbound);
        my $emit = 1;
        if ($self->{_last_debug_line} && $self->{_last_debug_line} eq $line) {
            $emit = 0; # identical line; avoid spam
        }
        if ($emit) {
            print $line;
            $self->{_last_debug_line} = $line;
        }
    }

    my $threshold  = $self->{commit_threshold};
    my $eff_margin = $self->{commit_margin} * (1.0 + $self->{wait_preference});

    # Decision: commit immediately if full beats any continuation by margin
    if ($best->{score} >= $threshold && ($best->{score} - $best_upperbound) >= $eff_margin) {
        $self->_commit($best->{name}, $best->{score}, $t);
        return;
    }

    # Otherwise, wait but never beyond hard cap
    my $idle_s = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0;
    my $remain_s_max = $self->_max_remaining_time_for_any_supersequence($best->{idx});  # use stretch_max for patience
    my $dynamic_window = $remain_s_max * (1.0 + $self->{wait_preference});
    my $decision_cap   = min($self->{decision_timeout_s}, $dynamic_window);

    if ($best->{score} >= $threshold && $idle_s >= $decision_cap) {
        $self->_commit($best->{name}, $best->{score}, $t);
        return;
    }
}

sub _commit {
    my ($self, $name, $score, $t) = @_;
    # DEBUG:verbose_commit
    print sprintf("[t=+%.03fs] TRIGGERED: %s (score: %.3f)\n",
                  ($self->{_first_event_t} ? $t - $self->{_first_event_t} : 0.0),
                  $name, $score) if $self->{verbose};
    $self->{callback}->($name, $score, { t_end => $t }) if $self->{callback};
    $self->reset();
}

# Longest additional time any longer pattern would still need, using stretch_max (be patient)
sub _max_remaining_time_for_any_supersequence {
    my ($self, $best_idx) = @_;
    return 0 unless $self->{_have_longers};
    my $best = $self->{patterns}[$best_idx];
    my $k    = @{$best->{runs}};
    my $worst_rem_q = 0;

    for my $j (0..$#{$self->{patterns}}) {
        next if $j == $best_idx;
        my $p = $self->{patterns}[$j];
        next unless _runs_prefix($best->{runs}, $p->{runs});
        my $rem_q = 0; $rem_q += $p->{runs}[$_] {len} for ($k..$#{$p->{runs}});
        $worst_rem_q = max($worst_rem_q, $rem_q);
    }

    return 0 unless $worst_rem_q > 0;
    my $rem_q_max = max(1, int($worst_rem_q * $self->{stretch_max} + 0.5));
    return $rem_q_max * $self->{quantum_s};
}

# -------------------------------------------------------------------
# Matching primitives
# -------------------------------------------------------------------

# Convert events to runs and append a *virtual* trailing run to "now"
sub _obs_runs_with_virtual {
    my ($events, $q, $t_now) = @_;
    return ([], undef) unless @$events >= 1;

    my @runs;
    # Build completed spans between edges
    for my $i (0..$#$events-1) {
        my ($pk, $pt) = @{$events->[$i]};
        my ($ck, $ct) = @{$events->[$i+1]};
        my $dur = $ct - $pt;
        my $quanta = max(1, int($dur / $q + 0.5));
        my $sym = ($pk eq 'press') ? '.' : '~';
        if (@runs && $runs[-1]{sym} eq $sym) {
            $runs[-1]{len} += $quanta;
        } else {
            push @runs, { sym => $sym, len => $quanta };
        }
    }

    # Add virtual tail from last edge → now
    my ($lk, $lt) = @{$events->[-1]};
    my $v_dur = $t_now - $lt;
    my $v_q   = max(1, int($v_dur / $q + 0.5));
    my $v_sym = ($lk eq 'press') ? '.' : '~';

    if (@runs && $runs[-1]{sym} eq $v_sym) {
        $runs[-1]{len} += $v_q;
    } else {
        push @runs, { sym => $v_sym, len => $v_q };
    }

    return (\@runs, $v_sym);
}

# Score a FULL candidate using first |p| runs of observation.
# If pattern is 'terminal' and the last compared run is the *virtual* run,
# ignore penalty on that last run (indeterminate trailing pause/hold).
sub _full_score {
    my ($self, $obs_runs, $p, $virtual_sym, $smin, $smax) = @_;
    my $k = @{$p->{runs}};
    return (0, 1.0, undef) if @$obs_runs < $k;

    my @o = @$obs_runs[0..$k-1];

    # strict symbol agreement
    for my $i (0..$#o) {
        return (0, 1.0, undef) if $o[$i]{sym} ne $p->{runs}[$i]{sym};
    }

    my $sum_o = 0; $sum_o += $_->{len} for @o;
    my $sum_r = 0; $sum_r += $_->{len} for @{$p->{runs}};
    return (0, 1.0, undef) if $sum_r == 0;

    my $s = $sum_o / $sum_r;
    $s = ($s < $smin) ? $smin : ($s > $smax ? $smax : $s);

    my $err = 0;
    my $den = 0;
    my @perrun; # VIS:viz_perrun — [{sym, rlen_scaled, olen, tol, pen}]
    for my $i (0..$#o) {
        my $sym = $p->{runs}[$i]{sym};
        my $rlen_scaled = max(1, int($p->{runs}[$i]{len} * $s + 0.5));
        my $olen = $o[$i]{len};
        my $tol = $p->{elasticity}{$sym} // 0;

        my $pen;
        if ($p->{terminal} && $i == $#o && $sym eq $virtual_sym) {
            # Terminal semantics: do not penalize last run if it's the virtual tail
            $pen = 0;
        } else {
            my $diff = abs($olen - $rlen_scaled);
            $pen = ($diff > $tol) ? ($diff - $tol) : 0;
        }

        my $rw = ($sym eq '.') ? $self->{run_weight_dot} : $self->{run_weight_tilde};
        $err += $rw * $pen;              # weighted penalty
        $den += $rw * $rlen_scaled;      # weighted “ideal mass”

        push @perrun, { sym => $sym, r => $rlen_scaled, o => $olen, tol => $tol, pen => $pen };
    }

    my $score = _score_from_err_den($err, $den);
    return ($score, $s, \@perrun);
}

# Optimistic upper bound for a longer pattern given current obs prefix
# (prefix compared with virtual tail included; future unmatched runs assumed perfect)
sub _prefix_upperbound {
    my ($self, $obs_runs, $p, $smin, $smax) = @_;
    my $k = @$obs_runs;
    return 0 if $k == 0;
    return 0 if $k > @{$p->{runs}};

    my @pref = @{$p->{runs}}[0..$k-1];

    # compute s on prefix
    my $sum_o = 0; $sum_o += $_->{len} for @$obs_runs;
    my $sum_r = 0; $sum_r += $_->{len} for @pref;
    return 0 if $sum_r == 0;

    my $s = $sum_o / $sum_r;
    $s = ($s < $smin) ? $smin : ($s > $smax ? $smax : $s);

    my $err = 0;
    my $den = 0;
    for my $i (0..$#pref) {
        my $sym = $pref[$i]{sym};
        my $rlen_scaled = max(1, int($pref[$i]{len} * $s + 0.5));
        my $olen = $obs_runs->[$i]{len};
        my $tol = $p->{elasticity}{$sym} // 0;

        my $diff = abs($olen - $rlen_scaled);
        my $pen  = ($diff > $tol) ? ($diff - $tol) : 0;

        my $rw = ($sym eq '.') ? $self->{run_weight_dot} : $self->{run_weight_tilde};
        $err += $rw * $pen;              # weighted penalty
        $den += $rw * $rlen_scaled;      # weighted ideal
    }

    # optimistic: remaining runs perfect
    for my $i ($k..$#{$p->{runs}}) {
        my $sym = $p->{runs}[$i]{sym};
        my $rlen_scaled = max(1, int($p->{runs}[$i]{len} * $s + 0.5));
        my $rw = ($sym eq '.') ? $self->{run_weight_dot} : $self->{run_weight_tilde};
        $den += $rw * $rlen_scaled;
    }

    return _score_from_err_den($err, $den);
}

# Compute per-run alignment for the longest matching prefix (including virtual-tail semantics).
# Returns: (\@perrun, $k, $s) where $k is number of aligned runs and $s is the chosen scale.
sub _perrun_prefix_alignment {
    my ($obs_runs, $p, $virtual_sym, $smin, $smax) = @_;
    return ([], 0, 1.0) unless @$obs_runs;

    my $kmax = (@$obs_runs < @{$p->{runs}}) ? @$obs_runs : scalar(@{$p->{runs}});
    my $k = 0;
    my $sum_o = 0;
    my $sum_r = 0;

    for my $i (0..$kmax-1) {
        last if $obs_runs->[$i]{sym} ne $p->{runs}[$i]{sym};
        $sum_o += $obs_runs->[$i]{len};
        $sum_r += $p->{runs}[$i]{len};
        $k++;
    }
    return ([], 0, 1.0) if $k == 0 || $sum_r == 0;

    my $s = $sum_o / $sum_r;
    $s = ($s < $smin) ? $smin : ($s > $smax ? $smax : $s);

    my @perrun;
    for my $i (0..$k-1) {
        my $sym = $p->{runs}[$i]{sym};
        my $rlen_scaled = max(1, int($p->{runs}[$i]{len} * $s + 0.5));
        my $olen = $obs_runs->[$i]{len};
        my $tol = $p->{elasticity}{$sym} // 0;

        my $pen;
        if ($p->{terminal} && $i == $k-1 && $sym eq $virtual_sym) {
            $pen = 0;
        } else {
            my $diff = abs($olen - $rlen_scaled);
            $pen = ($diff > $tol) ? ($diff - $tol) : 0;
        }

        push @perrun, { sym => $sym, r => $rlen_scaled, o => $olen, tol => $tol, pen => $pen };
    }

    return (\@perrun, $k, $s);
}

sub _score_from_err_den {
    my ($err, $den) = @_;
    $den = max(1, $den);
    my $score = $den / ($den + $err);   # in (0,1], higher is better
    return $score;
}

# -------------------------------------------------------------------
# Pattern utilities
# -------------------------------------------------------------------




sub _score_from_err_den {
    my ($err, $den) = @_;
    $den = max(1, $den);
    my $score = $den / ($den + $err);   # in (0,1], higher is better
    return $score;
}

# -------------------------------------------------------------------
# Pattern utilities
# -------------------------------------------------------------------

# Normalize pattern & extract flags.
# Returns ($canonical_string, $terminal_flag, $continues_flag)
sub _canonicalize_pattern {
    my ($p) = @_;
    $p //= '';
    $p =~ s/\s+//g;

    my $continues = ($p =~ s/>$//) ? 1 : 0;   # advisory hint
    my $terminal  = ($p =~ s/\)$//) ? 1 : 0;  # ignore penalty on last run if it is virtual tail

    $p =~ s/\+//g;   # drop long markers
    $p =~ s/-/./g;   # medium press → dot
    $p =~ s/[^.~]//g;

    return ($p, $terminal, $continues);
}

sub _runs_from_pattern {
    my ($p) = @_;
    return [] unless length $p;
    my @runs;
    while ($p =~ /([.~])\1*/g) {
        my $tok = $&;
        push @runs, { sym => substr($tok,0,1), len => length($tok) };
    }
    return \@runs;
}

sub _runs_prefix {
    my ($A, $B) = @_;
    return 0 unless @$A < @$B;
    for my $i (0..$#$A) {
        return 0 if $A->[$i]{sym} ne $B->[$i]{sym};
    }
    return 1;
}

# -------------------------------------------------------------------
# VIS helpers (colors, backgrounds, abbreviated renderings)
# -------------------------------------------------------------------

sub _ansi_rgb   { return sprintf("\e[38;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_bg    { return sprintf("\e[48;2;%d;%d;%dm", $_[0], $_[1], $_[2]); } # a24bg(R,G,B)
sub _ansi_reset { return "\e[0m"; }

# pen==0 → near-white; higher pen → darker base color
sub _fg_for_pen {
    my ($sym, $pen) = @_;
    my $alpha = 1.0 / (1.0 + $pen);   # in (0,1]
    my ($r1,$g1,$b1,$r2,$g2,$b2);
    if ($sym eq '.') {        # green-ish
        ($r1,$g1,$b1) = (220,255,220); # good
        ($r2,$g2,$b2) = (0,80,0);      # bad
    } else {                   # '~' blue-ish
        ($r1,$g1,$b1) = (220,220,255);
        ($r2,$g2,$b2) = (0,0,120);
    }
    my $r = int($r2 + ($r1 - $r2) * $alpha + 0.5);
    my $g = int($g2 + ($g1 - $g2) * $alpha + 0.5);
    my $b = int($b2 + ($b1 - $b2) * $alpha + 0.5);
    return _ansi_rgb($r,$g,$b);
}

# VIS:viz_runs_abbrev — render runs using base bg + symbol fg, truncated
sub _viz_runs_abbrev {
    my ($runs, $scale, $symbols, $bg, $max_chars) = @_;
    my $s = $bg;
    my $count = 0;
    for my $r (@$runs) {
        my $ch = $symbols->{$r->{sym}} // $r->{sym};
        my $n  = max(1, int($r->{len} * $scale + 0.5));
        for (my $i=0; $i<$n; $i++) {
            last if $count >= $max_chars;
            $s .= _fg_for_pen($r->{sym}, 0) . $ch;
            $count++;
        }
        last if $count >= $max_chars;
    }
    $s .= "…" if _render_len_chars($runs, $scale) > $max_chars;
    return $s . _ansi_reset();
}

sub _render_len_chars {
    my ($runs, $scale) = @_;
    my $tot = 0;
    for my $r (@$runs) {
        $tot += max(1, int($r->{len} * $scale + 0.5));
    }
    return $tot;
}


# VIS:viz_full_candidate_line — per-run colored by penalty; scaled to viz_scale, on pattern bg
sub _viz_full_candidate_line {
    my ($self, $perrun) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $colored = $self->{_viz_pat_bg};

    for my $i (0..$#$perrun) {
        my ($sym, $r, $pen) = @{$perrun->[$i]}{qw(sym r pen)};
        my $nchars = max(1, int($r * $scale + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $seg = ($ch x $nchars);
        my $fg  = $self->{viz_show_colors} ? _fg_for_pen($sym, $pen) : '';
        $colored .= $fg . $seg;
    }
    return $colored . _ansi_reset();
}

# VIS:viz_pattern_str — render full pattern (no penalty info), colored by symbol type
sub _viz_pattern_str {
    my ($self, $runs, $is_user) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $bg      = $is_user ? $self->{_viz_user_bg} : $self->{_viz_pat_bg};
    my $s = $bg;
    for my $r (@$runs) {
        my $n  = max(1, int($r->{len} * $scale + 0.5));
        my $ch = $symbols->{$r->{sym}} // $r->{sym};
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($r->{sym}, 0) : '';
        $s .= $fg . ($ch x $n);
    }
    return $s . _ansi_reset();
}

# Abbreviated pattern string (no penalty), for one-line 'pat="..."'
sub _viz_pattern_abbrev {
    my ($self, $runs) = @_;
    return _viz_runs_abbrev($runs, $self->{viz_scale}, $self->{viz_symbols}, $self->{_viz_pat_bg}, $self->{viz_abbrev_max});
}

# VIS: per-run colored abbreviated 'pat="..."' using measured penalties for prefix, dim remainder.
sub _viz_pattern_abbrev_perrun {
    my ($self, $perrun, $p_runs, $s, $k) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $maxc    = $self->{viz_abbrev_max};
    my $out     = $self->{_viz_pat_bg};
    my $count   = 0;

    # prefix: perrun colored
    for my $i (0..$k-1) {
        my ($sym, $r, $pen) = @{$perrun->[$i]}{qw(sym r pen)};
        my $nchars = max(1, int($r * $scale + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($sym, $pen) : '';
        for (my $j=0; $j<$nchars && $count<$maxc; $j++) {
            $out .= $fg . $ch;
            $count++;
        }
        last if $count >= $maxc;
    }

    # remainder: dim (no observation yet)
    for my $i ($k..$#$p_runs) {
        my $sym = $p_runs->[$i]{sym};
        my $rlen_scaled = max(1, int($p_runs->[$i]{len} * $s + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($sym, 999) : '';  # very dark
        for (my $j=0; $j<$rlen_scaled && $count<$maxc; $j++) {
            $out .= $fg . $ch;
            $count++;
        }
        last if $count >= $maxc;
    }

    $out .= "…" if $count >= $maxc;
    return $out . _ansi_reset();
}

# VIS: full pattern line with per-run penalties for prefix, dim remainder.
sub _viz_full_from_perrun {
    my ($self, $perrun, $p_runs, $s, $k) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $out     = $self->{_viz_pat_bg};

    for my $i (0..$k-1) {
        my ($sym, $r, $pen) = @{$perrun->[$i]}{qw(sym r pen)};
        my $nchars = max(1, int($r * $scale + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($sym, $pen) : '';
        $out .= $fg . ($ch x $nchars);
    }
    for my $i ($k..$#$p_runs) {
        my $sym = $p_runs->[$i]{sym};
        my $rlen_scaled = max(1, int($p_runs->[$i]{len} * $s + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($sym, 999) : '';
        $out .= $fg . ($ch x max(1, int($rlen_scaled * $scale + 0.5)));
    }
    return $out . _ansi_reset();
}

# -------------------------------------------------------------------
# DEBUG helpers (verbose >= 2)
# -------------------------------------------------------------------




# -------------------------------------------------------------------
# DEBUG helpers (verbose >= 2)
# -------------------------------------------------------------------

sub _runs_to_string_scaled {
    my ($runs, $scale, $symbols) = @_;
    my $s = '';
    for my $r (@$runs) {
        my $ch = $symbols->{$r->{sym}} // $r->{sym};
        my $n  = max(1, int($r->{len} * $scale + 0.5));
        $n = 120 if $n > 120; # cap
        $s .= ($ch x $n);
    }
    return $s;
}

sub _debug_dump_observation {
    my ($self, $obs_runs, $dt) = @_;
    return unless $self->{viz_enable};
    my $qms = int($self->{quantum_s} * 1000 + 0.5);
    my $len_q = 0; $len_q += $_->{len} for @$obs_runs;
    my $len_ms = $len_q * $qms;
    my $sym = _runs_to_string_scaled($obs_runs, $self->{viz_scale}, $self->{viz_symbols});
    printf "[t=+%.03fs] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
        $dt, $qms, $len_q, $len_ms, $sym;
}

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_list, $best_ub, $dt) = @_;

    printf "[t=+%.03fs] CAND full_best=%.3f ub_best=%.3f\n",
        $dt, $full_list->[0]{score}, $best_ub;

    # VIS: one-line user abbreviated line (usr="...") with user bg
    if ($self->{viz_enable}) {
        my $usr_abbrev = _viz_runs_abbrev($obs_runs, $self->{viz_scale}, $self->{viz_symbols}, $self->{_viz_user_bg}, $self->{viz_abbrev_max});
        printf "                                              usr=\"%s\"\n", $usr_abbrev;
    }

    my $smin = $self->{stretch_min};
    my $smax = $self->{stretch_max};

    # Show ALL patterns: FULL (scored), UB (upper bound), or X (impossible)
    for my $idx (0..$#{$self->{patterns}}) {
        my $p = $self->{patterns}[$idx];
        my $name = $p->{name};
        my $status;
        my $val = 0.0;
        my $viz  = '';
        my $pat_abbrev = '';

        if (@$obs_runs < @{$p->{runs}}) {
            $status = 'UB';
            $val = _prefix_upperbound($self, $obs_runs, $p, $smin, $smax) * $p->{w_eff};

            if ($self->{viz_enable}) {
                my ($perrun_pref, $k, $s) = _perrun_prefix_alignment($obs_runs, $p, $obs_runs->[-1]{sym}, $smin, $smax);
                $pat_abbrev = $self->_viz_pattern_abbrev_perrun($perrun_pref, $p->{runs}, $s, $k);
                if ($self->{viz_pats_full}) {
                    my $pf = _viz_full_from_perrun($self, $perrun_pref, $p->{runs}, $s, $k);
                    $viz = $pf;
                }
            }
        } else {
            my ($sc, $s, $perrun) = $self->_full_score($obs_runs, $p, $obs_runs->[-1]{sym}, $smin, $smax);

            if ($sc > 0) {
                $status = 'FULL';
                $val = $sc * $p->{w_eff};
                if ($self->{viz_enable}) {
                    $pat_abbrev = $self->_viz_pattern_abbrev_perrun($perrun, $p->{runs}, $s, scalar(@$perrun));
                    $viz = $self->{viz_pats_full} ? $self->_viz_full_candidate_line($perrun) : '';
                }
            } else {
                $status = 'X';
                $val = 0;
                if ($self->{viz_enable}) {
                    # attempt prefix coloring if any prefix aligns; else neutral
                    my ($perrun_pref, $k, $s) = _perrun_prefix_alignment($obs_runs, $p, $obs_runs->[-1]{sym}, $smin, $smax);
                    if ($k > 0) {
                        $pat_abbrev = $self->_viz_pattern_abbrev_perrun($perrun_pref, $p->{runs}, $s, $k);
                        $viz = $self->{viz_pats_full} ? _viz_full_from_perrun($self, $perrun_pref, $p->{runs}, $s, $k) : '';
                    } else {
                        $pat_abbrev = $self->_viz_pattern_abbrev($p->{runs});
                        $viz = $self->{viz_pats_full} ? $self->_viz_pattern_str($p->{runs}, 0) : '';
                    }
                }
            }
        }

        if ($self->{viz_enable}) {
            my $line = sprintf("  %-14s  %-5s  score=%.3f  w=%.2f eff=%.2f  pat=\"%s\"",
                               $name, $status, $val, $p->{weight}, $p->{w_eff}, $pat_abbrev);
            print $line, "\n";
            if ($viz) {
                print "    ", $viz, "\n";
            }
        } else {
            my $pat_txt = _runs_to_string_scaled($p->{runs}, 1.0, { '.' => '.', '~' => '~' });
            my $line = sprintf("  %-14s  %-5s  score=%.3f  w=%.2f eff=%.2f  pat=\"%s\"",
                               $name, $status, $val, $p->{weight}, $p->{w_eff}, $pat_txt);
 
            print $line, "\n";
        }
    }
}


1;

