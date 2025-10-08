package Recognizer;
use v5.36;
use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(sum min max);

# ----------------------------------------------------------------------------
# Recognizer.pm  — variance-driven scoring + patience-weighted UBs
# ----------------------------------------------------------------------------
# Key changes in this refactor:
#  • Per-run variance and per-run weight are first-class in scoring.
#  • Pattern-level weight multiplies the raw fit to produce a final score.
#  • Global stretch_min/max are removed. We match in QUANTA ("dots").
#  • UB (upper bound) is optimistic: prefix scored as-is; unseen suffix = perfect.
#  • New param: patience (default 1.0). We scale UB_raw of *non-best* candidates
#    by this factor before comparing vs the best FULL. patience>1 waits longer;
#    patience<1 commits sooner. (UB_raw is clamped to [0,1] after scaling.)
#  • Hard-stop safety: idle and/or span hard caps emit NO_MATCH and reset.
#  • Visualization hooks are preserved: _debug_dump_observation, _debug_dump_candidates.
# ----------------------------------------------------------------------------

sub new {
    my ($class, %a) = @_;

    my $self = bless({
        # time
        quantum_ms         => defined $a{quantum_ms} ? 0.0 + $a{quantum_ms} : 20.0, # ms per dot
        # scoring / decisions
        commit_threshold    => defined $a{commit_threshold}    ? 0.0 + $a{commit_threshold}    : 0.75,
        commit_margin       => defined $a{commit_margin}       ? 0.0 + $a{commit_margin}       : 0.10,
        prefer_longer_margin=> defined $a{prefer_longer_margin}? 0.0 + $a{prefer_longer_margin}: 0.12,
        tie_epsilon         => defined $a{tie_epsilon}         ? 0.0 + $a{tie_epsilon}         : 0.005,
        decision_timeout_s  => defined $a{decision_timeout_s}  ? 0.0 + $a{decision_timeout_s}  : 0.30,
        ignore_ub_on_timeout=> exists $a{ignore_ub_on_timeout} ? !!$a{ignore_ub_on_timeout}    : 0,
        require_release_for_dot_end => exists $a{require_release_for_dot_end} ? !!$a{require_release_for_dot_end} : 1,
        # safety
        hard_stop_idle_s    => defined $a{hard_stop_idle_s}    ? 0.0 + $a{hard_stop_idle_s}    : 1.00,
        hard_stop_span_s    => defined $a{hard_stop_span_s}    ? 0.0 + $a{hard_stop_span_s}    : 0.00,
        # UB patience tuning (global)
        patience            => defined $a{patience}            ? 0.0 + $a{patience}            : 1.00,
        # symbol config
        sym_press           => defined $a{sym_press}           ? $a{sym_press}                 : '.',
        sym_release         => defined $a{sym_release}         ? $a{sym_release}               : '~',
        # verbosity / viz
        verbose             => defined $a{verbose}             ? 0 + $a{verbose}               : 1,
        viz_enable          => exists $a{viz_enable}           ? !!$a{viz_enable}              : 1,
        # defaults if pattern-run variance/weight missing
        default_var_press   => defined $a{default_var_press}   ? 0.0 + $a{default_var_press}   : 1.0, # dots
        default_var_release => defined $a{default_var_release} ? 0.0 + $a{default_var_release} : 2.0, # dots
        default_run_weight  => defined $a{default_run_weight}  ? 0.0 + $a{default_run_weight}  : 1.0,
        # inputs
        patterns            => $a{patterns} || [],
        callback            => $a{callback},
        # runtime
        _events             => [],   # [ [press|release, t_abs], ... ]
        _first_event_t      => undef,
        _last_event_t       => undef,
        _last_debug_line    => undef,
    }, $class);

    $self->{quantum_s} = $self->{quantum_ms} / 1000.0;

    # Precompute pattern structures (len per run in dots, total len/weight)
    _precompute_patterns($self);

    return $self;
}

sub reset {
    my ($self) = @_;
    $self->{_events} = [];
    $self->{_first_event_t} = undef;
    $self->{_last_event_t}  = undef;
    $self->{_last_debug_line} = undef;
}

# -----------------------------------------------------------------------------
# External interface: feed edges and ticks
# -----------------------------------------------------------------------------
sub press   { my ($self, $t) = @_; $self->_edge('press',   defined $t ? $t : time()); }
sub release { my ($self, $t) = @_; $self->_edge('release', defined $t ? $t : time()); }
sub tick    { my ($self, $t) = @_; $self->_evaluate_if_ready(defined $t ? $t : time()); }

sub _edge {
    my ($self, $kind, $t) = @_;
    push @{$self->{_events}}, [$kind, $t];
    $self->{_first_event_t} //= $t;
    $self->{_last_event_t}    = $t;
    $self->_evaluate_if_ready($t);
}

# -----------------------------------------------------------------------------
# Pattern preprocessing
# -----------------------------------------------------------------------------
sub _precompute_patterns {
    my ($self) = @_;
    my $qs = $self->{quantum_s};
    my $sp = $self->{sym_press};
    my $sr = $self->{sym_release};

    for my $p (@{$self->{patterns}}) {
        # Expected fields per pattern:
        #   name, id (optional), weight (pattern-level),
        #   runs => [ { type: 'press'|'release', sym: '.'|'~', len: <dots>, variance: <dots>, weight: <w> }, ... ]
        # If sym missing, infer from type; if len missing, infer from visual string length if present.
        $p->{weight} = 1.0 unless defined $p->{weight};

        # Normalize runs:
        #  - if HASH (indexed map) -> ordered ARRAY
        #  - if missing but visual present -> parse visual (supports .{N}/~{N})
        if (exists $p->{runs} && ref($p->{runs}) eq 'HASH') {
            my @ord = map { $p->{runs}{$_} } sort { $a <=> $b } keys %{$p->{runs}};
            $p->{runs} = \@ord;
        }
        if (!exists $p->{runs} && exists $p->{visual}) {
            $p->{runs} = _runs_from_visual($p->{visual}, $sp, $sr);
        }
        next unless ref($p->{runs}) eq 'ARRAY' && @{$p->{runs}};

        my $len_total = 0.0;
        my $w_total   = 0.0;
        for my $r (@{$p->{runs}}) {
            $r->{type} //= ($r->{sym} && $r->{sym} eq $sr) ? 'release' : 'press';
            $r->{sym}  //= ($r->{type} eq 'release') ? $sr : $sp;
            $r->{len}  = 0.0 + ($r->{len} // 1);
            $r->{variance} //= ($r->{type} eq 'release') ? $self->{default_var_release} : $self->{default_var_press};
            $r->{weight}   //= $self->{default_run_weight};
            $len_total += $r->{len};
            $w_total   += $r->{weight};
        }
        $p->{len_total}  = $len_total;
        $p->{run_w_sum}  = $w_total;
        $p->{runs_count}    = scalar(@{$p->{runs}});
    }
}

sub _runs_from_visual {
    my ($visual, $sp, $sr) = @_;
    my @runs; my $i = 0;
    my @ch = split //, ($visual // '');
    while ($i <= $#ch) {
        my $c = $ch[$i];
        if ($c eq $sp || $c eq $sr) {
            my $type = ($c eq $sr) ? 'release' : 'press';
            # brace form: .{N} / ~{N}  (N in dots, may be float)
            if ($i+1 <= $#ch && $ch[$i+1] eq '{') {
                my $j = $i + 2; my $num = '';
                while ($j <= $#ch && $ch[$j] ne '}') { $num .= $ch[$j]; $j++ }
                if ($j <= $#ch && $ch[$j] eq '}') {
                    my $dots = 0.0 + $num;
                    push @runs, { sym=>$c, type=>$type, len=>$dots };
                    $i = $j + 1;
                    next;
            }
        } else {
            # ignore unknown for now (visual braces handled upstream, not here)
        }
            # plain glyph run
            my $k = $i + 1; my $cnt = 1;
            $cnt++, $k++ while ($k <= $#ch && $ch[$k] eq $c);
            push @runs, { sym=>$c, type=>$type, len=>$cnt };
            $i = $k; next;
    }
        $i++;
    }
    return \@runs;
}

# -----------------------------------------------------------------------------
# Core evaluation
# -----------------------------------------------------------------------------
sub _evaluate_if_ready {
    my ($self, $t) = @_;

    my ($obs_runs, $virtual_sym) = _obs_runs_with_virtual(
        $self->{_events}, $self->{quantum_s}, $t, $self->{sym_press}, $self->{sym_release}
    );
    return unless @$obs_runs;

    my @full = ();
    my $best_upperbound_w   = 0.0;
    my $best_upperbound_raw = 0.0;
    my $best_ub_idx         = -1;
    my @ub_idx = ();
    my ($last_kind, $last_t) = @{$self->{_events}[-1]};

    # Score FULL candidates; collect strict-longers for UB pass
    for my $idx (0..$#{$self->{patterns}}) {
        my $p = $self->{patterns}[$idx];
        my $pruns = $p->{runs};
        if (@$obs_runs < @$pruns) { push @ub_idx, $idx; next; }

        my ($raw, $perrun) = _full_score_variance($obs_runs, $p);
        next if $raw <= 0; # impossible
        my $final = $raw * ($p->{weight} // 1.0);
        push @full, {
            name       => ($p->{name} // $p->{id} // "pattern_$idx"),
            score      => $final,    # final = raw * pattern weight
            raw        => $raw,      # 0..1
            idx        => $idx,
            perrun     => $perrun,
            runs_count => $p->{runs_count},
            len_total  => $p->{len_total},
        };
    }

    return unless @full;

    # Rank by final score (higher better)
    @full = sort { $b->{score} <=> $a->{score} } @full;
    my $best = $full[0];

    # Prefer longer FULL only if it's a TRUE supersequence of current best AND close in score
    my $prom = $self->{prefer_longer_margin};
    my $best_runs_ref = $self->{patterns}[$best->{idx}]{runs};
    my @close_longers = grep {
        $_->{runs_count} > $best->{runs_count}
        && _runs_prefix($best_runs_ref, $self->{patterns}[$_->{idx}]{runs})
        && $_->{score} >= $best->{score} - $prom
    } @full;
    if (@close_longers) {
        @close_longers = sort { $b->{runs_count} <=> $a->{runs_count} || $b->{score} <=> $a->{score} } @close_longers;
        $best = $close_longers[0];
    }

    # Tie-break within ε: fewer runs, then shorter len, then higher score
    my @near = grep { $best->{score} - $_->{score} <= $self->{tie_epsilon} } @full;
    if (@near > 1) {
        @near = sort {
            $a->{runs_count}  <=> $b->{runs_count}  ||
            $a->{len_total}   <=> $b->{len_total}   ||
            $b->{score}       <=> $a->{score}
        } @near;
        $best = $near[0];
    }

    # Compute UB among strict supersequences of the chosen best FULL
    my $best_runs = $self->{patterns}[$best->{idx}]{runs};
    $best_upperbound_w   = 0.0;
    $best_upperbound_raw = 0.0;
    $best_ub_idx         = -1;

    for my $j (@ub_idx) {
        my $q = $self->{patterns}[$j];
        next unless _runs_prefix($best_runs, $q->{runs}); # only true supers of best
        my $ub_raw = _prefix_upperbound_variance($obs_runs, $q);
        # Apply patience scaling to non-best UB
        $ub_raw = max(0.0, min(1.0, $ub_raw * ($self->{patience} // 1.0)));
        my $ub_w   = $ub_raw * ($q->{weight} // 1.0);
        if ($ub_w > $best_upperbound_w)    { $best_upperbound_w   = $ub_w; }
        if ($ub_raw > $best_upperbound_raw){ $best_upperbound_raw = $ub_raw; $best_ub_idx = $j; }
    }

    # DEBUG: dumps — timestamps relative to first edge
    my $t0 = $self->{_first_event_t} // $t;
    my $dt = $t - $t0;
    $self->_debug_dump_observation($obs_runs, $dt) if $self->{verbose} >= 2 && $self->{viz_enable};
    $self->_debug_dump_candidates($obs_runs, \@full, $best_upperbound_w, $dt) if $self->{verbose} >= 2;

    # DEBUG: compact one-liner (throttled)
    if ($self->{verbose} >= 1) {
        my $line = sprintf("[t=+%.03fs] best full: %s score=%.3f, best UBw=%.3f UB=%.3f\n",
                           $dt, $best->{name}, $best->{score}, $best_upperbound_w, $best_upperbound_raw);
        my $emit = 1;
        if ($self->{_last_debug_line} && $self->{_last_debug_line} eq $line) { $emit = 0; }
        if ($emit) { print $line; $self->{_last_debug_line} = $line; }
    }

    # Decision thresholds and guards
    my $threshold     = $self->{commit_threshold};
    my $eff_margin    = $self->{commit_margin};
    my $best_last_sym = $self->{patterns}[$best->{idx}]{runs}[-1]{sym};
    # reuse earlier $last_kind (remove duplicate 'my')
    my $dot_end_guard = ($self->{require_release_for_dot_end}
                         && defined $last_kind && $last_kind eq 'press'
                         && $best_last_sym eq $self->{sym_press});

    # Immediate commit if best FULL clearly beats any continuation by margin
    if (!$dot_end_guard && $best->{raw} >= ($best_upperbound_raw + $eff_margin) && $best->{score} >= $threshold) {
        _commit($self, $best->{name}, $best->{score}, $t);
        return;
    }

    # Timed commit window (simple: fixed cap); prefer simplicity until deadline logic lands
    my $idle_s = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0.0;
    my $decision_cap = $self->{decision_timeout_s};

    if (!$dot_end_guard && $best->{score} >= $threshold && $idle_s >= $decision_cap &&
        ( $self->{ignore_ub_on_timeout} || $best->{raw} >= $best_upperbound_raw )) {
        _commit($self, $best->{name}, $best->{score}, $t);
        return;
    }

    # Hard-stop / reset if nothing matched and we've been idle too long or spanned too long
    my $span_s = defined($self->{_first_event_t}) ? ($t - $self->{_first_event_t}) : 0.0;
    if ($self->{hard_stop_idle_s} && $idle_s >= $self->{hard_stop_idle_s}) { _no_match($self, $t); return; }
    if ($self->{hard_stop_span_s} && $span_s >= $self->{hard_stop_span_s}) { _no_match($self, $t); return; }
}

# -----------------------------------------------------------------------------
# Scoring (variance-driven)
# -----------------------------------------------------------------------------
# _full_score_variance: raw in [0,1]; runs beyond pattern length ignored.
# raw = (sum_j w_j * per_run_score_j) / (sum_j w_j), where per_run_score_j = exp(-0.5 * z^2)
#   and z = (obs_len_dots - mean_len_dots) / max(variance_dots, eps)
# ----------------------------------------------------------------------------
sub _full_score_variance {
    my ($obs_runs, $p) = @_;
    my $pruns = $p->{runs};
    my $L = scalar(@$pruns);

    my $w_sum = 0.0; my $acc = 0.0; my @per;
    for my $i (0..$L-1) {
        my $r  = $pruns->[$i];
        my $or = $obs_runs->[$i];     # FULL guarantees exists
        my $mu = 0.0 + ($r->{len} // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight} // 1.0);
        my $x  = 0.0 + ($or->{len} // 0);
        my $eps = 0.25; # dots, stability
        my $z  = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s  = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
        push @per, $s;
    }
    return ( ($w_sum>0 ? $acc/$w_sum : 0.0), \@per );
}

# UB for a longer pattern q: prefix scored as-is; suffix assumed perfect (score=1)
sub _prefix_upperbound_variance {
    my ($obs_runs, $q) = @_;
    my $qruns = $q->{runs};
    my $K = scalar(@$obs_runs);

    my $w_sum = 0.0; my $acc = 0.0;
    # prefix runs (0..K-1)
    for my $i (0..$K-1) {
        my $r  = $qruns->[$i];
        last unless defined $r; # if obs longer than q (shouldn't be for UB)
        my $mu = 0.0 + ($r->{len} // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight} // 1.0);
        my $x  = 0.0 + ($obs_runs->[$i]{len} // 0);
        my $eps = 0.25;
        my $z  = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s  = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
    }
    # suffix runs (K..end) get perfect 1.0 each
    for my $i ($K..$#$qruns) {
        my $w = 0.0 + ($qruns->[$i]{weight} // 1.0);
        $acc  += $w * 1.0;
        $w_sum+= $w;
    }
    return ($w_sum>0 ? $acc/$w_sum : 0.0);
}

# -----------------------------------------------------------------------------
# Observation utilities
# -----------------------------------------------------------------------------
sub _obs_runs_with_virtual {
    my ($events, $quantum_s, $t, $sym_press, $sym_release) = @_;
    return ([], undef) unless @$events;

    my @runs;  # [{sym, len}], in dots (quantized)
    my ($cur_sym, $cur_start) = (undef, undef);

    # Events are edge timestamps relative to absolute time; we walk pairs
    for (my $i=0; $i<@$events; $i++) {
        my ($kind, $te) = @{$events->[$i]};
        if (!defined $cur_sym) {
            $cur_sym   = ($kind eq 'press') ? $sym_press : $sym_release;
            $cur_start = $te;
        } else {
            # edge toggles run
            my $len_s = $te - $cur_start;
            my $len_d = int(($len_s / $quantum_s) + 0.5);
            $len_d = 1 if $len_d < 1;  # ensure visible dot
            push @runs, { sym=>$cur_sym, len=>$len_d };
            $cur_sym   = ($kind eq 'press') ? $sym_press : $sym_release;
            $cur_start = $te;
        }
    }
    # virtual trailing run to "now"
    if (defined $cur_sym && defined $cur_start) {
        my $len_s = $t - $cur_start;
        my $len_d = int(($len_s / $quantum_s) + 0.5);
        $len_d = 1 if $len_d < 1;
        push @runs, { sym=>$cur_sym, len=>$len_d };
    }

    return (\@runs, ($cur_sym // undef));
}

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
sub _runs_prefix {
    my ($a_ref, $b_ref) = @_;
    my $n = scalar(@$a_ref);
    return 0 unless scalar(@$b_ref) > $n; # strict supersequence only
    for my $i (0..$n-1) {
        return 0 unless ($a_ref->[$i]{sym} // '') eq ($b_ref->[$i]{sym} // '');
    }
    return 1;
}

sub _commit {
    my ($self, $name, $score, $t) = @_;
    print sprintf("[t=+%.03fs] TRIGGERED: %s (score: %.3f)\n",
                  ($self->{_first_event_t} ? $t - $self->{_first_event_t} : 0.0),
                  $name, $score) if $self->{verbose};
    $self->{callback}->($name, $score, { t_end => $t }) if $self->{callback};
    $self->reset();
}

sub _no_match {
    my ($self, $t) = @_;
    print sprintf("[t=+%.03fs] NO_MATCH (reset)\n",
                  ($self->{_first_event_t} ? $t - $self->{_first_event_t} : 0.0)) if $self->{verbose};
    $self->{callback}->('NO_MATCH', undef, { t_end => $t }) if $self->{callback};
    $self->reset();
}

# -----------------------------------------------------------------------------
# Visualization (kept minimal; hooks preserved)
# -----------------------------------------------------------------------------
sub _debug_dump_observation {
    my ($self, $obs_runs, $dt) = @_;
    my $q = $self->{quantum_ms};
    my $s = '';
    for my $r (@$obs_runs) { $s .= ($r->{sym} x max(1, $r->{len})); }
    printf("[t=+%.03fs] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
           $dt, $q, scalar(@$obs_runs),
           int(sum(map { $_->{len} } @$obs_runs) * $self->{quantum_ms}), $s);
}

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_ref, $best_ub_w, $dt) = @_;
    my $usr = '';
    for my $r (@$obs_runs) { $usr .= ($r->{sym} x max(1, $r->{len})); }
    printf("%60s usr=\"%s\"\n", '', $usr);
    for my $c (@$full_ref) {
        my $p = $self->{patterns}[$c->{idx}];
        my $pat = '';
        for my $r (@{$p->{runs}}) { $pat .= ($r->{sym} x max(1, $r->{len})); }
        printf("  %-14s FULL   score=%.3f  w=%.2f  pat=\"%s\"\n",
               $c->{name}, $c->{score}, ($p->{weight}//1.0), $pat);
    }
}

1;
