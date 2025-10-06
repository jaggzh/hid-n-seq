#!/usr/bin/perl
package ButtonGesture::Recognizer;

use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(min max);
use YAML::XS qw(LoadFile);

our $VERSION = '0.20';

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
# -------------------------------------------------------------------

sub new {
    my ($class, %a) = @_;

    my $self = bless {
        # config / I-O
        config_file        => $a{config_file} // 'gestures.yaml',
        gestures_raw       => $a{gestures} // [],
        callback           => $a{callback} // sub { print "Action: $_[0]\n" },
        verbose            => $a{verbose} // 0,

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

        # visualization
        viz_scale          => (defined $a{viz_scale} ? 0.0 + $a{viz_scale} : 1.0),
        viz_symbols        => $a{viz_symbols} // { '.' => '▉', '~' => '▂' },  # can be UTF-8 e.g. • and ∿
        viz_show_colors    => (defined $a{viz_show_colors} ? !!$a{viz_show_colors} : 1),

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

    # Evaluate right after releases to minimize latency
    $self->_evaluate_if_ready($t) if $kind eq 'release';
}

sub tick {
    my ($self, $t) = @_;
    $t //= time();
    return unless @{$self->{_events}};
    my $since_last = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0;
    if ($since_last >= $self->{idle_timeout_s}) {
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
        push @compiled, {
            name       => $name,
            weight     => $weight,
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
                $obs_runs, $p,
                $self->{stretch_min}, $self->{stretch_max}
            );
            $ub *= $p->{weight};
            if ($ub > $best_upperbound) {
                $best_upperbound = $ub;
                $best_ub_idx     = $idx;
            }
            next;
        }

        my ($score, $s, $perrun) = _full_score(
            $obs_runs, $p, $virtual_sym,
            $self->{stretch_min}, $self->{stretch_max}
        );
        next if $score <= 0;
        my $final = $score * $p->{weight};
        push @full, { name => $p->{name}, score => $final, raw => $score, s => $s, idx => $idx, perrun => $perrun };
    }

    return unless @full;

    @full = sort { $b->{score} <=> $a->{score} } @full;
    my $best = $full[0];

    # Verbose dumps (with timestamps relative to first edge)
    my $t0   = $self->{_first_event_t} // $t;
    my $dt   = $t - $t0;
    _debug_dump_observation($self, $obs_runs, $dt) if $self->{verbose} >= 2;
    _debug_dump_candidates($self, $obs_runs, \@full, $best_upperbound, $dt) if $self->{verbose} >= 2;

    # Compact line (throttled)
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

    # Commit immediately if we clearly beat any continuation
    if ($best->{score} >= $threshold && ($best->{score} - $best_upperbound) >= $eff_margin) {
        $self->_commit($best->{name}, $best->{score}, $t);
        return;
    }

    # Otherwise, allow waiting—but only for a finite window.
    my $idle_s = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0;
    my $remain_s_max = $self->_max_remaining_time_for_any_supersequence($best->{idx});  # use stretch_max for patience
    my $dynamic_window = $remain_s_max * (1.0 + $self->{wait_preference});
+   my $decision_cap   = min($self->{decision_timeout_s}, $dynamic_window);


    if ($best->{score} >= $threshold && $idle_s >= $decision_cap) {
        # Time's up: accept best-so-far full candidate
        $self->_commit($best->{name}, $best->{score}, $t);
        return;
    }
}

sub _commit {
    my ($self, $name, $score, $t) = @_;
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
    my ($obs_runs, $p, $virtual_sym, $smin, $smax) = @_;
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
    my @perrun; # for viz: [{sym, rlen_scaled, olen, tol, pen}]
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

        $err += $pen;
        $den += $rlen_scaled;
        push @perrun, { sym => $sym, r => $rlen_scaled, o => $olen, tol => $tol, pen => $pen };
    }

    my $score = _score_from_err_den($err, $den);
    return ($score, $s, \@perrun);
}

# Optimistic upper bound for a longer pattern given current obs prefix
# (prefix compared with virtual tail included; future unmatched runs assumed perfect)
sub _prefix_upperbound {
    my ($obs_runs, $p, $smin, $smax) = @_;
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

        $err += $pen;
        $den += $rlen_scaled;
    }

    # optimistic: remaining runs perfect
    for my $i ($k..$#{$p->{runs}}) {
        my $rlen_scaled = max(1, int($p->{runs}[$i]{len} * $s + 0.5));
        $den += $rlen_scaled;
    }

    return _score_from_err_den($err, $den);
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
# Debug helpers (verbose >= 2)
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

sub _ansi_rgb     { return sprintf("\e[38;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_reset   { return "\e[0m"; }

sub _color_for {
    my ($sym, $pen) = @_;
    # pen==0 → near-white; higher pen → darker base color
    my $alpha = 1.0 / (1.0 + $pen);   # 1 -> 0..1
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

sub _viz_full_candidate_line {
    my ($self, $p, $perrun) = @_;
    # Render per-run with color by penalty; scaled to viz_scale
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $colored = '';

    for my $i (0..$#$perrun) {
        my ($sym, $r, $o, $tol, $pen) = @{$perrun->[$i]}{qw(sym r o tol pen)};
        my $nchars = max(1, int($r * $scale + 0.5));
        my $ch = $symbols->{$sym} // $sym;
        my $seg = ($ch x $nchars);
        if ($self->{viz_show_colors}) {
            $colored .= _color_for($sym, $pen) . $seg . _ansi_reset();
        } else {
            $colored .= $seg;
        }
    }
    return $colored;
}

sub _debug_dump_observation {
    my ($self, $obs_runs, $dt) = @_;
    my $qms = int($self->{quantum_s} * 1000 + 0.5);
    my $sym = _runs_to_string_scaled($obs_runs, $self->{viz_scale}, $self->{viz_symbols});
    my $len_q = 0; $len_q += $_->{len} for @$obs_runs;
    my $len_ms = $len_q * $qms;
    printf "[t=+%.03fs] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
        $dt, $qms, $len_q, $len_ms, $sym;
}

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_list, $best_ub, $dt) = @_;

    printf "[t=+%.03fs] CAND full_best=%.3f ub_best=%.3f\n",
        $dt, $full_list->[0]{score}, $best_ub;

    # Show ALL patterns: FULL (scored), UB (upper bound), or X (impossible)
    my $smin = $self->{stretch_min};
    my $smax = $self->{stretch_max};
    my @rows;

    # Build a map from name to full info
    my %fullmap = map { $_->{name} => $_ } @$full_list;

    for my $idx (0..$#{$self->{patterns}}) {
        my $p = $self->{patterns}[$idx];
        my $name = $p->{name};
        my $status;
        my $val = 0.0;
        my $viz  = '';

        if (@$obs_runs < @{$p->{runs}}) {
            $status = 'UB';
            $val = _prefix_upperbound($obs_runs, $p, $smin, $smax) * $p->{weight};
        } else {
            my ($sc, $s, $perrun) = _full_score($obs_runs, $p, $obs_runs->[-1]{sym}, $smin, $smax);
            if ($sc > 0) {
                $status = 'FULL';
                $val = $sc * $p->{weight};
                $viz = _viz_full_candidate_line($self, $p, $perrun);
            } else {
                $status = 'X';
                $val = 0;
            }
        }

        my $pat = _runs_to_string_scaled($p->{runs}, $self->{viz_scale}, $self->{viz_symbols});
        my $line = sprintf("  %-14s  %-5s  score=%.3f  w=%.2f  pat=\"%s\"",
                           $name, $status, $val, $p->{weight}, $pat);
        print $line, "\n";
        if ($viz && $self->{viz_show_colors}) {
            print "    ", $viz, "\n";
        }
    }
}

1;
