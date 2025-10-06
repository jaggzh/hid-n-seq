#!/usr/bin/perl
package ButtonGesture::Recognizer;

use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(min max);
use YAML::XS qw(LoadFile);

our $VERSION = '0.12';

# -------------------------------------------------------------------
# Overview
# -------------------------------------------------------------------
# Stream edges via feed_event('press'|'release', t).
# Quantize into '.' (press/hold) and '~' (pause) using quantum_ms.
#
# Matching:
#   • Dot/Tilde only. Legacy '-', '+', ')' are normalized away.
#   • Dynamic time-stretching: a single global scale s ∈ [stretch_min,stretch_max]
#     aligns observed runs to reference runs; per-symbol elasticity (quanta) forgives
#     small deviations before adding penalty.
#   • Pattern weight biases ties (“prefer when in doubt”).
#
# Decision (click-click vs click-click-click):
#   • Evaluate on releases and idle tick.
#   • If a FULL candidate (obs covers all of its runs) beats the best *upper bound*
#     of any longer supersequence by commit_margin (inflated by wait_preference),
#     COMMIT immediately.
#   • Otherwise, we wait—but only up to a finite decision window:
#       decision_timeout_s OR (min remaining time any plausible supersequence would
#       still need at stretch_max) × (1 + wait_preference). After that, COMMIT
#       the best full candidate.  (NOTE: previous builds used stretch_min and
#       (1 - wait_preference), which biased toward singles; fixed here.)
#
# YAML schema:
#   gestures: [ { name, pattern, weight, elasticity:{'.', '~'} }, ... ]
#   or a top-level array of the same entries.
#   Optional trailing '>' in a pattern is a “often-a-prefix” hint (advisory only).
#
# Tunables (constructor args) — detailed behavior and practical ranges:
#   quantum_ms        : Milliseconds per quantum used to convert time spans into run lengths.
#                       Smaller values = finer timing resolution but more noise; larger values = coarser timing and “snappier”
#                       matching but less nuance. Typical range: 15–25 ms. Default: 20.
#                       Implications: Elasticity values (in quanta) scale inversely with this; halving quantum_ms roughly doubles
#                       the number of quanta in a given real-world gap/press.
#
#   idle_timeout_s    : If no new edge has arrived for this many seconds, the engine runs an evaluation pass from tick().
#                       Lower => lower perceived latency but more frequent “consider committing now?” checks. Default: 0.25.
#
#   commit_threshold  : Minimum score a FULL candidate must reach to be eligible to commit.
#                       Raise to avoid weak matches; lower to make the system more eager. Range: 0.70–0.90. Default: 0.75.
#                       Note: score already includes pattern weight, time-stretch, and elasticity penalties.
#
#   commit_margin     : The minimum lead (best_full_score − best_upper_bound_of_any_longer_pattern) required to commit immediately.
#                       Larger margin => more “patience” when a plausible continuation (e.g., double-click) exists.
#                       Effective margin = commit_margin × (1 + wait_preference). Typical: 0.10–0.25. Default: 0.10.
#
#   wait_preference   : 0..1 scalar that increases the effective commit_margin AND the dynamic wait window when a longer pattern is plausible.
#                       0.0 = neutral (no extra patience). 0.5 => 1.5× margin/window. 1.0 => 2× margin/window. Default: 0.20.
#
#   decision_timeout_s: Upper bound on how long to wait once a FULL candidate exists but doesn’t yet beat continuations by margin.
#                       If the second click hasn’t arrived by this window, we commit the best full candidate (keeps UX responsive).
#                       Typical: 0.25–0.40. Default: 0.30 (slightly higher than before to be double-click friendly).
#
#   stretch_min/max   : Global time-stretch limits (s) for aligning observed vs reference run lengths.
#                       The engine finds a single scale s ∈ [stretch_min, stretch_max] to best fit total timing before applying
#                       per-symbol elasticity. Defaults: 0.5 .. 2.0. If users’ double-click gaps are longer than learned, raise
#                       stretch_max (e.g., to 2.2). If very fast users exist, lower stretch_min (e.g., 0.6).
#
#   verbose           : Integer. 0 = silent; 1 = compact (“best full vs UB”); 2 = candidate table + observed symbol string;
#                       3 = deep per-run breakdown. Output is throttled (~0.2 s) to avoid flooding identical lines.
#
#   gestures          : Array of gesture maps passed directly (preferred). Each: { name, pattern, weight, elasticity:{'.','~'} }.
#   config_file       : YAML file loaded when gestures aren’t provided. Accepts top-level array or {gestures:[...]} / {patterns:[...]}.
#   callback          : sub { my ($name, $score, $meta) = @_; ... } called on commit; $meta->{t_end} is the commit timestamp.
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

        # runtime state
        _events            => [],     # [ ['press'|'release', t], ... ]
        _last_event_t      => undef,  # time of last edge
        _last_debug        => undef,  # { line, t } for throttling

        # compiled patterns
        patterns           => [],     # [{ name, weight, runs:[{sym,len}], len_total, elasticity{.,~}, continues }]
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
    $self->{_events}       = [];
    $self->{_last_event_t} = undef;
    $self->{_last_debug}   = undef;
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
        my ($canon, $continues) = _canonicalize_pattern($g->{pattern});
        my $runs = _runs_from_pattern($canon);
        next unless @$runs;
        my $len_total = 0; $len_total += $_->{len} for @$runs;
        push @compiled, {
            name       => $name,
            weight     => $weight,
            runs       => $runs,
            len_total  => $len_total,
            elasticity => { '.' => ($elas->{'.'} // 0), '~' => ($elas->{'~'} // 0) },
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

    my $obs_runs = _obs_runs_from_events($self->{_events}, $self->{quantum_s});
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

        my ($score, $s) = _full_score(
            $obs_runs, $p,
            $self->{stretch_min}, $self->{stretch_max}
        );
        next if $score <= 0;
        my $final = $score * $p->{weight};
        push @full, { name => $p->{name}, score => $final, raw => $score, s => $s, idx => $idx };
    }

    return unless @full;

    @full = sort { $b->{score} <=> $a->{score} } @full;
    my $best = $full[0];

    # Verbose dumps
    _debug_dump_observation($self, $obs_runs, $t) if $self->{verbose} && $self->{verbose} >= 2;
    _debug_dump_candidates($self, $obs_runs, \@full, $best_upperbound) if $self->{verbose} && $self->{verbose} >= 2;

    # Compact line (throttled)
    if ($self->{verbose} && $self->{verbose} >= 1) {
        my $now  = $t;
        my $line = sprintf("[recognizer] best full: %s score=%.3f, best UB=%.3f\n",
                           $best->{name}, $best->{score}, $best_upperbound);
        my $emit = 1;
        if ($self->{_last_debug}) {
            my $ld = $self->{_last_debug};
            $emit = 0 if $line eq $ld->{line} && ($now - $ld->{t} < 0.20);
        }
        if ($emit) {
            print $line;
            $self->{_last_debug} = { line => $line, t => $now };
        }
    }

    my $threshold = $self->{commit_threshold};
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
    my $decision_cap   = max($self->{decision_timeout_s}, $dynamic_window);

    if ($best->{score} >= $threshold && $idle_s >= $decision_cap) {
        # Time's up: accept best-so-far full candidate
        $self->_commit($best->{name}, $best->{score}, $t);
        return;
    }
}

sub _commit {
    my ($self, $name, $score, $t) = @_;
    print "TRIGGERED: $name (score: $score)\n" if $self->{verbose};
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

sub _obs_runs_from_events {
    my ($events, $q) = @_;
    return [] unless @$events >= 2; # need at least a span

    my @runs;
    my $prev = $events->[0];
    for my $i (1..$#$events) {
        my ($pk, $pt) = @{$prev};
        my ($ck, $ct) = @{$events->[$i]};
        my $dur = $ct - $pt;
        my $quanta = max(1, int($dur / $q + 0.5));
        my $sym = ($pk eq 'press') ? '.' : '~';
        if (@runs && $runs[-1]{sym} eq $sym) {
            $runs[-1]{len} += $quanta;
        } else {
            push @runs, { sym => $sym, len => $quanta };
        }
        $prev = $events->[$i];
    }
    return \@runs;
}

# Score a fully-complete candidate: use first |p| runs from obs
sub _full_score {
    my ($obs_runs, $p, $smin, $smax) = @_;
    my $k = @{$p->{runs}};
    return (0, 1.0) if @$obs_runs < $k;

    my @o = @$obs_runs[0..$k-1];

    # strict symbol agreement
    for my $i (0..$#o) {
        return (0, 1.0) if $o[$i]{sym} ne $p->{runs}[$i]{sym};
    }

    my $sum_o = 0; $sum_o += $_->{len} for @o;
    my $sum_r = 0; $sum_r += $_->{len} for @{$p->{runs}};
    return (0, 1.0) if $sum_r == 0;

    my $s = $sum_o / $sum_r;
    $s = ($s < $smin) ? $smin : ($s > $smax ? $smax : $s);

    my $err = 0;
    my $den = 0;
    for my $i (0..$#o) {
        my $sym = $p->{runs}[$i]{sym};
        my $rlen_scaled = max(1, int($p->{runs}[$i]{len} * $s + 0.5));
        my $olen = $o[$i]{len};
        my $tol = $p->{elasticity}{$sym} // 0;

        my $diff = abs($olen - $rlen_scaled);
        my $pen  = ($diff > $tol) ? ($diff - $tol) : 0;
        $err += $pen;
        $den += $rlen_scaled;
    }

    my $score = _score_from_err_den($err, $den);
    return ($score, $s);
}

# Optimistic upper bound for a longer pattern given current obs prefix
# Assume future unmatched runs will be perfect; score the aligned prefix only.
sub _prefix_upperbound {
    my ($obs_runs, $p, $smin, $smax) = @_;
    my $k = @$obs_runs;
    return 0 if $k == 0;
    return 0 if $k > @{$p->{runs}};

    my @pref = @{$p->{runs}}[0..$k-1];
    my ($err, $den_cur) = _prefix_error_and_den($obs_runs, \@pref, $p, $smin, $smax);
    return 0 if $err < 0;

    my $den_total = $den_cur;
    for my $i ($k..$#{$p->{runs}}) { $den_total += $p->{runs}[$i]{len} }

    return _score_from_err_den($err, $den_total);
}

sub _prefix_error_and_den {
    my ($o_pref, $r_pref, $p_full, $smin, $smax) = @_;

    for my $i (0..$#$r_pref) {
        return (-1, 0) if $o_pref->[$i]{sym} ne $r_pref->[$i]{sym};
    }

    my $sum_o = 0; $sum_o += $_->{len} for @$o_pref;
    my $sum_r = 0; $sum_r += $_->{len} for @$r_pref;
    return (0, 1) if $sum_r == 0;

    my $s = $sum_o / $sum_r;
    $s = ($s < $smin) ? $smin : ($s > $smax ? $smax : $s);

    my $err = 0;
    my $den = 0;
    for my $i (0..$#$r_pref) {
        my $sym = $r_pref->[$i]{sym};
        my $rlen_scaled = max(1, int($r_pref->[$i]{len} * $s + 0.5));
        my $olen = $o_pref->[$i]{len};
        my $tol = $p_full->{elasticity}{$sym} // 0;

        my $diff = abs($olen - $rlen_scaled);
        my $pen  = ($diff > $tol) ? ($diff - $tol) : 0;
        $err += $pen;
        $den += $rlen_scaled;
    }
    return ($err, $den);
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

sub _canonicalize_pattern {
    my ($p) = @_;
    $p //= '';
    $p =~ s/\s+//g;

    my $continues = ($p =~ s/>$//) ? 1 : 0;   # advisory hint
    $p =~ s/\)$//;                            # drop legacy terminal

    $p =~ s/\+//g;   # drop long markers
    $p =~ s/-/./g;   # medium press → dot
    $p =~ s/[^.~]//g;

    return ($p, $continues);
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

sub _runs_to_string {
    my ($runs) = @_;
    my $s = '';
    for my $r (@$runs) {
        my $ch = $r->{sym};
        my $n  = $r->{len};
        $n = 60 if $n > 60; # cap for readability
        $s .= ($ch x $n);
    }
    return $s;
}

sub _debug_dump_observation {
    my ($self, $obs_runs, $t) = @_;
    my $qms = int($self->{quantum_s} * 1000 + 0.5);
    my $sym = _runs_to_string($obs_runs);
    my $len_q = 0; $len_q += $_->{len} for @$obs_runs;
    my $len_ms = $len_q * $qms;
    printf "[recognizer/v2] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
        $qms, $len_q, $len_ms, $sym;
    if ($self->{verbose} >= 3) {
        my @parts = map { sprintf("%s×%d", $_->{sym}, $_->{len}) } @$obs_runs;
        print "  runs: ", join(' ', @parts), "\n";
    }
}

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_list, $best_ub) = @_;

    printf "[recognizer/v2] CAND full_best=%.3f ub_best=%.3f\n",
        $full_list->[0]{score}, $best_ub;

    if ($self->{verbose} >= 3) {
        # Deep table: recompute statuses for all patterns
        my $smin = $self->{stretch_min};
        my $smax = $self->{stretch_max};
        my @rows;
        for my $idx (0..$#{$self->{patterns}}) {
            my $p = $self->{patterns}[$idx];
            my $status;
            my $val;

            if (@$obs_runs < @{$p->{runs}}) {
                $status = 'UB';
                $val = _prefix_upperbound($obs_runs, $p, $smin, $smax) * $p->{weight};
            } else {
                my ($sc, $s) = _full_score($obs_runs, $p, $smin, $smax);
                if ($sc > 0) {
                    $status = 'FULL';
                    $val = $sc * $p->{weight};
                } else {
                    $status = 'X';
                    $val = 0;
                }
            }

            my $pat = _runs_to_string($p->{runs});
            push @rows, sprintf("  %-12s  %-5s  score=%.3f  w=%.2f  pat=\"%s\"",
                                $p->{name}, $status, $val, $p->{weight}, $pat);
        }
        print join("\n", @rows), "\n";
    }
}

1;
