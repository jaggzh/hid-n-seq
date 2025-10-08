package Recognizer;
use v5.36;
use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(sum min max);
use Term::ReadKey;
use Data::Dumper;

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
        # DONE detection (NEW)
        done_k_sigma        => defined $a{done_k_sigma}        ? 0.0 + $a{done_k_sigma}        : 1.00,
        # symbols
        sym_press           => defined $a{sym_press}           ? $a{sym_press}                 : '.',
        sym_release         => defined $a{sym_release}         ? $a{sym_release}               : '~',
        # visualization
        viz_enable          => exists $a{viz_enable}           ? !!$a{viz_enable}              : 1,
        verbose             => defined $a{verbose}             ? 0 + $a{verbose}               : 1,
        viz_symbols         => $a{viz_symbols} // { '.' => '▉', '~' => '▂' },
        viz_show_colors     => exists $a{viz_show_colors} ? !!$a{viz_show_colors} : 1,
        viz_scale           => defined $a{viz_scale} ? 0.0 + $a{viz_scale} : 1.0,
        viz_abbrev_max      => defined $a{viz_abbrev_max} ? 0 + $a{viz_abbrev_max} : 120,
        viz_user_bg_rgb     => $a{viz_user_bg_rgb} // [ 18, 18, 18 ],
        viz_pat_bg_rgb      => $a{viz_pat_bg_rgb}  // [ 10, 10, 10 ],
        # defaults if pattern-run variance/weight missing
        default_var_press   => defined $a{default_var_press}   ? 0.0 + $a{default_var_press}   : 1.0,
        default_var_release => defined $a{default_var_release} ? 0.0 + $a{default_var_release} : 2.0,
        default_run_weight  => defined $a{default_run_weight}  ? 0.0 + $a{default_run_weight}  : 1.0,
        # inputs/runtime
        patterns            => $a{patterns} || [],
        callback            => $a{callback},
        _events             => [],
        _first_event_t      => undef,
        _last_event_t       => undef,
        _last_debug_line    => undef,
    }, $class);

    $self->{quantum_s} = $self->{quantum_ms} / 1000.0;

    # VIS: precompute ANSI backgrounds
    $self->{_viz_user_bg} = _ansi_bg(@{$self->{viz_user_bg_rgb}});
    $self->{_viz_pat_bg}  = _ansi_bg(@{$self->{viz_pat_bg_rgb}});

    _precompute_patterns($self);

    # NEW: init per-pattern peak_done
    for my $p (@{$self->{patterns}}) { $p->{_peak_done} = 0.0; }

    return $self;
}

sub reset {
    my ($self) = @_;
    $self->{_events} = [];
    $self->{_first_event_t} = undef;
    $self->{_last_event_t}  = undef;
    $self->{_last_debug_line} = undef;
    # clear per-pattern peak_done each fresh observation
    for my $p (@{$self->{patterns}}) { $p->{_peak_done} = 0.0; }
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

# DONE detection — distinguishes PREFIX vs DONE for a pattern given current observation
sub _pattern_is_done {
    my ($self, $obs_runs, $p, $last_kind) = @_;
    my $Lobs = scalar(@$obs_runs);
    my $Lpat = scalar(@{$p->{runs}});
    return 0 if $Lobs < $Lpat;

    my $last_sym = $p->{runs}[-1]{sym};
    if ($last_sym eq $self->{sym_press}) {
        # dot-ended patterns are DONE only on release (press-end guard)
        return 1 if ($self->{require_release_for_dot_end} && defined $last_kind && $last_kind eq 'release');
        return 0;
    } else {
        # tilde-ended: DONE when the final gap reaches "completable" length (μ - k·σ), not growing forever
        my $r_last = $p->{runs}[-1];
        my $mu  = 0.0 + ($r_last->{len}      // 1);
        my $sd  = 0.0 + ($r_last->{variance} // 1.0);
        my $k   = 0.0 + ($self->{done_k_sigma} // 1.0);
        my $need = $mu - $k * $sd;  $need = 0 if $need < 0;
        my $x   = 0.0 + ($obs_runs->[$Lpat-1]{len} // 0);
        return ($x >= $need) ? 1 : 0;
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

    my @full = ();                      # now "prefix-covered" candidates
    my ($best_ub_w, $best_ub_raw, $best_ub_idx) = (0.0, 0.0, -1);
    my @ub_idx = ();                    # indices to consider for UB
    my ($last_kind, $last_t) = @{$self->{_events}[-1]};

    for my $idx (0..$#{$self->{patterns}}) {
        my $p = $self->{patterns}[$idx];

        if (@$obs_runs < @{$p->{runs}}) {
            push @ub_idx, $idx;         # strict supersequences
            next;
        }

        # Full prefix covered: compute score and DONE state
        my ($raw, $perrun_info) = _full_score_variance($obs_runs, $p);
        next if $raw <= 0;

        my $final = $raw * ($p->{weight} // 1.0);
        my $is_done = $self->_pattern_is_done($obs_runs, $p, $last_kind);
        if ($is_done) {
            # Peak-at-DONE: keep the best score reached at DONE moments only
            my $prev = $p->{_peak_done} // 0.0;
            $p->{_peak_done} = ($final > $prev) ? $final : $prev;
        }

        push @full, {
            name        => ($p->{name} // $p->{id} // "pattern_$idx"),
            score       => $final,             # instantaneous (for viz)
            raw         => $raw,
            idx         => $idx,
            perrun_info => $perrun_info,
            runs_count  => $p->{runs_count},
            len_total   => $p->{len_total},
            is_done     => $is_done,
            peak_done   => ($p->{_peak_done} // 0.0),
        };
    }

    return unless @full;

    # Pick a "display best" for the line (unchanged tie logic)
    @full = sort { $b->{score} <=> $a->{score} } @full;
    my $best_disp = $full[0];
    my $prom = $self->{prefer_longer_margin};
    my @close_longers = grep { $_->{runs_count} > $best_disp->{runs_count}
                               && $_->{score} >= $best_disp->{score} - $prom } @full;
    if (@close_longers) {
        @close_longers = sort { $b->{runs_count} <=> $a->{runs_count} || $b->{score} <=> $a->{score} } @close_longers;
        $best_disp = $close_longers[0];
    }
    my @near = grep { $best_disp->{score} - $_->{score} <= $self->{tie_epsilon} } @full;
    if (@near > 1) {
        @near = sort {
            $b->{runs_count}  <=> $a->{runs_count}  ||
            $b->{len_total}   <=> $a->{len_total}   ||
            $b->{score}       <=> $a->{score}
        } @near;
        $best_disp = $near[0];
    }

    # UBs: consider any pattern that is a plausible supersequence of the observation prefix
    $best_ub_w = 0.0; $best_ub_raw = 0.0; $best_ub_idx = -1;
    for my $j (0 .. $#{$self->{patterns}}) {
        my $q = $self->{patterns}[$j];
        next unless _runs_prefix($obs_runs, $q->{runs});            # symbol-prefix match
        next if @$obs_runs >= @{$q->{runs}} && $self->_pattern_is_done($obs_runs, $q, $last_kind);
        my ($ub_raw, undef) = _prefix_ub_with_perrun($obs_runs, $q);# optimistic suffix
        my $ub_w = $ub_raw * ($q->{weight} // 1.0);
        if ($ub_w > $best_ub_w)    { $best_ub_w   = $ub_w;  }
        if ($ub_raw > $best_ub_raw){ $best_ub_raw = $ub_raw; $best_ub_idx = $j; }
    }

    # DEBUG VIS
    my $t0   = $self->{_first_event_t} // $t;
    my $dt   = $t - $t0;
    $self->_debug_dump_observation($obs_runs, $dt) if $self->{verbose} >= 2 && $self->{viz_enable};
    $self->_debug_dump_candidates($obs_runs, \@full, $best_ub_w, $dt) if $self->{verbose} >= 2;

    if ($self->{verbose} >= 1) {
        my $line = sprintf("[t=+%.03fs] best full: %s score=%.3f, best UBw=%.3f UB=%.3f\n",
                           $dt, $best_disp->{name}, $best_disp->{score}, $best_ub_w, $best_ub_raw);
        if (!$self->{_last_debug_line} || $self->{_last_debug_line} ne $line) {
            print $line;
            $self->{_last_debug_line} = $line;
        }
    }

    # --------- NEW: Peak-DONE decision ---------------------------------------
    # Choose best DONE by peak; compare against max UB of others (with patience)
    my $threshold  = $self->{commit_threshold};
    my $margin     = $self->{commit_margin};
    my $patience   = $self->{patience};

    my $best_done_idx   = -1;
    my $best_done_score = 0.0;
    for my $c (@full) {
        if ($c->{peak_done} > $best_done_score) {
            $best_done_score = $c->{peak_done};
            $best_done_idx   = $c->{idx};
        }
    }

    if ($best_done_idx >= 0 && $best_done_score >= $threshold) {
        my $ub_competitors = 0.0;
        for my $j (0 .. $#{$self->{patterns}}) {
            next if $j == $best_done_idx;
            my $q = $self->{patterns}[$j];
            next unless _runs_prefix($obs_runs, $q->{runs});
            next if @$obs_runs >= @{$q->{runs}} && $self->_pattern_is_done($obs_runs, $q, $last_kind);
            my ($ub_raw_j, undef) = _prefix_ub_with_perrun($obs_runs, $q);
            my $ub_w_j = ($ub_raw_j * ($q->{weight} // 1.0)) * $patience;
            $ub_competitors = $ub_w_j if $ub_w_j > $ub_competitors;
        }

        if ($best_done_score >= $ub_competitors + $margin) {
            my $name = $self->{patterns}[$best_done_idx]{name} // $self->{patterns}[$best_done_idx]{id} // "pattern_$best_done_idx";
            $self->_commit($name, $best_done_score, $t);
            return;
        }
    }
    # -------------------------------------------------------------------------

    # Safety: hard-stops if nothing can be decided for too long
    my $idle_s = defined($self->{_last_event_t}) ? ($t - $self->{_last_event_t}) : 0;
    my $span_s = defined($self->{_first_event_t}) ? ($t - $self->{_first_event_t}) : 0;

    if ($self->{hard_stop_idle_s} && $idle_s >= $self->{hard_stop_idle_s}) {
        $self->_no_match($t);
        return;
    }
    if ($self->{hard_stop_span_s} && $span_s >= $self->{hard_stop_span_s}) {
        $self->_no_match($t);
        return;
    }
}


# -----------------------------------------------------------------------------
# Scoring (variance-driven)
# -----------------------------------------------------------------------------
# _full_score_variance: raw in [0,1]; runs beyond pattern length ignored.
# raw = (sum_j w_j * per_run_score_j) / (sum_j w_j), where per_run_score_j = exp(-0.5 * z^2)
#   and z = (obs_len_dots - mean_len_dots) / max(variance_dots, eps)
# ----------------------------------------------------------------------------

# _full_score_variance: return (raw, perrun_info)
sub _full_score_variance {
    my ($obs_runs, $p) = @_;
    my $pruns = $p->{runs};
    my $L = scalar(@$pruns);

    my $w_sum = 0.0; my $acc = 0.0; my @per_info;
    for my $i (0..$L-1) {
        my $r  = $pruns->[$i];
        my $or = $obs_runs->[$i];
        my $mu = 0.0 + ($r->{len}      // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight}   // 1.0);
        my $x  = 0.0 + ($or->{len}     // 0);
        my $eps = 0.25;
        my $z  = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s  = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
        push @per_info, { sym => $r->{sym}, pen => ($z*$z), sim => $s };
    }
    return ( ($w_sum>0 ? $acc/$w_sum : 0.0), \@per_info );
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

# NEW: UB with per-run similarities (for colored viz prefix)
sub _prefix_ub_with_perrun {
    my ($obs_runs, $q) = @_;
    my $qruns = $q->{runs};
    my $K = scalar(@$obs_runs);

    my $w_sum = 0.0; my $acc = 0.0;
    my @sim_prefix;

    # prefix: score observed runs against q's runs
    for my $i (0..$K-1) {
        my $r  = $qruns->[$i] or last;
        my $mu = 0.0 + ($r->{len}      // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight}   // 1.0);
        my $x  = 0.0 + ($obs_runs->[$i]{len} // 0);
        my $eps = 0.25;  # dots, stability floor
        my $z  = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s  = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
        $sim_prefix[$i] = $s;  # for colorization
    }

    # suffix: assume perfect (optimistic UB)
    for my $i ($K..$#$qruns) {
        my $w = 0.0 + ($qruns->[$i]{weight} // 1.0);
        $acc  += $w * 1.0;
        $w_sum+= $w;
    }

    my $ub = ($w_sum>0 ? $acc/$w_sum : 0.0);
    return ($ub, \@sim_prefix);
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
# Visualization helpers (colors, backgrounds, abbreviated renderings)  (NEW)
# -----------------------------------------------------------------------------
sub _ansi_rgb   { return sprintf("\e[38;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_bg    { return sprintf("\e[48;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_dim   { "\e[2m" }
sub _ansi_reset { return "\e[0m"; }

# pen==0 → near-white; higher pen → darker base color per symbol family
sub _fg_for_pen {
    my ($sym, $pen) = @_;
    my $alpha = 1.0 / (1.0 + ($pen // 0));         # (0,1], 0-pen ~ near-white
    my ($r1,$g1,$b1,$r2,$g2,$b2);
    if ($sym eq '~') { ($r1,$g1,$b1) = (220,220,255); ($r2,$g2,$b2) = (0,0,120);  }
    else             { ($r1,$g1,$b1) = (220,255,220); ($r2,$g2,$b2) = (0,80,0);   }
    my $r = int($r2 + ($r1 - $r2) * $alpha + 0.5);
    my $g = int($g2 + ($g1 - $g2) * $alpha + 0.5);
    my $b = int($b2 + ($b1 - $b2) * $alpha + 0.5);
    return _ansi_rgb($r,$g,$b);
}

sub _render_len_chars {
    my ($runs, $scale) = @_;
    my $tot = 0;
    for my $r (@$runs) { $tot += max(1, int(($r->{len}//1) * $scale + 0.5)); }
    return $tot;
}

# abbreviated runs with base bg; per-run no penalty coloring
sub _viz_runs_abbrev {
    my ($runs, $scale, $symbols, $bg, $max_chars) = @_;
    my $s = $bg; my $count = 0;
    for my $r (@$runs) {
        my $ch = $symbols->{$r->{sym}} // $r->{sym};
        my $n  = max(1, int(($r->{len}//1) * $scale + 0.5));
        my $fg = _fg_for_pen($r->{sym}, 0);
        for (my $i=0; $i<$n && $count<$max_chars; $i++) { $s .= $fg . $ch; $count++; }
        last if $count >= $max_chars;
    }
    $s .= "…" if _render_len_chars($runs, $scale) > $max_chars;
    return $s . _ansi_reset();
}

# full pattern with bg; flat color by symbol type
sub _viz_pattern_str {
    my ($self, $runs, $is_user) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $bg      = $is_user ? $self->{_viz_user_bg} : $self->{_viz_pat_bg};
    my $s = $bg;
    for my $r (@$runs) {
        my $n  = max(1, int(($r->{len}//1) * $scale + 0.5));
        my $ch = $symbols->{$r->{sym}} // $r->{sym};
        my $fg = $self->{viz_show_colors} ? _fg_for_pen($r->{sym}, 0) : '';
        $s .= $fg . ($ch x $n);
    }
    return $s . _ansi_reset();
}

# abbreviated pattern for pat="...": prefix colored by per-run penalty; remainder dim
sub _viz_pattern_abbrev_perrun {
    my ($self, $perrun, $p_runs, $k) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $maxc    = $self->{viz_abbrev_max};
    my $out     = $self->{_viz_pat_bg};
    my $count   = 0;

    # Defensive: synthesize empty per-run info if missing
    my @fallback;
    if (ref($perrun) ne 'ARRAY' || !@$perrun) {
        @fallback = map { +{ sym => $_->{sym}, pen => 0 } } @$p_runs;
        $perrun = \@fallback;
    }

    # prefix
    for my $i (0..($k-1)) {
        my $sym  = (defined $perrun->[$i] && exists $perrun->[$i]{sym})
                   ? $perrun->[$i]{sym} : ($p_runs->[$i]{sym} // '.');
        my $pen  = (defined $perrun->[$i] && exists $perrun->[$i]{pen})
                   ? $perrun->[$i]{pen} : 0;
        my $n    = max(1, int(($p_runs->[$i]{len}//1) * $scale + 0.5));
        my $ch   = $symbols->{$sym} // $sym;
        my $fg   = $self->{viz_show_colors} ? _fg_for_pen($sym, $pen) : '';
        for (my $j=0; $j<$n && $count<$maxc; $j++) { $out .= $fg . $ch; $count++; }
        last if $count >= $maxc;
    }

    # remainder (dim)
    for my $i ($k..$#$p_runs) {
        my $sym = $p_runs->[$i]{sym};
        my $n   = max(1, int(($p_runs->[$i]{len}//1) * $scale + 0.5));
        my $ch  = $symbols->{$sym} // $sym;
        my $fg  = $self->{viz_show_colors} ? _fg_for_pen($sym, 999) : '';
        for (my $j=0; $j<$n && $count<$maxc; $j++) { $out .= $fg . $ch; $count++; }
        last if $count >= $maxc;
    }

    $out .= "…" if $count >= $maxc;
    return $out . _ansi_reset();
}

sub _debug_dump_observation {
    my ($self, $obs_runs, $dt) = @_;
    my $q = $self->{quantum_ms};
    my $s = $self->_viz_pattern_str($obs_runs, 1);
    printf("[t=+%.03fs] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
           $dt, $q, scalar(@$obs_runs),
           int(sum(map { $_->{len} } @$obs_runs) * $self->{quantum_ms}), $s);
}


# sub _debug_dump_observation {
#     my ($self, $obs_runs, $dt) = @_;
#     my $q = $self->{quantum_ms};
#     my $symmap = $self->{viz_symbols} || {};
#     my $s = '';
#     for my $r (@$obs_runs) {
#         my $glyph = $symmap->{$r->{sym}} // $r->{sym};
#         $s .= ($glyph x max(1, int($r->{len} + 0.5)));
#     }
#     printf("[t=+%.03fs] OBS q=%dms len=%dq (~%dms)  \"%s\"\n",
#            $dt, $q, scalar(@$obs_runs),
#            int(sum(map { $_->{len} } @$obs_runs) * $self->{quantum_ms}), $s);
# }

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_ref, $best_ub_w, $dt) = @_;
    my $usr = $self->_viz_pattern_str($obs_runs, 1);
    printf("%44s usr=\"%s\"\n", '', $usr);

    my %seen_full = map { $self->{patterns}[ $_->{idx} ]{name} // $_->{idx} => 1 } @$full_ref;

    # FULL rows: per-run colored, no suffix
    for my $c (@$full_ref) {
        my $p = $self->{patterns}[$c->{idx}];
        my $k = $p->{runs_count};                              # FULL: obs >= pat
        my $per = $c->{perrun_info} // $c->{perrun};   # backward compatibility
        my $pat = $self->_viz_pattern_abbrev_perrun($per, $p->{runs}, $k);
        printf("  %-14s FULL   score=%.3f  w=%.2f  pat=\"%s\"\n",
               $c->{name}, $c->{score}, ($p->{weight}//1.0), $pat);
    }

    # UB rows: strict supersequences of best prefix (prefix colored, suffix dim)
    for my $j (0 .. $#{$self->{patterns}}) {
        my $p = $self->{patterns}[$j];
        my $name = $p->{name} // $p->{id} // "pattern_$j";
        next if $seen_full{$name};
        next unless @$obs_runs < @{$p->{runs}};
        next unless _runs_prefix($obs_runs, $p->{runs});
        my ($ub_raw, $sim_prefix) = _prefix_ub_with_perrun($obs_runs, $p);
        my $ub_w = $ub_raw * ($p->{weight} // 1.0);
        my $k    = scalar(@$obs_runs);
        my $per  = _sim_prefix_to_perrun($p, $sim_prefix);
        my $pat  = $self->_viz_pattern_abbrev_perrun($per, $p->{runs}, $k);
        printf("  %-14s UB     score=%.3f  w=%.2f  pat=\"%s\"\n",
               $name, $ub_w, ($p->{weight}//1.0), $pat);
    }
}

# --- similarity color ramp (per run) -----------------------------------------
sub _ansi_for_sim {
    my ($s) = @_;  # s in [0,1]
    return '' unless defined $s;
    # Bright green → green → yellow → orange → red
    return "\e[38;5;46m"  if $s >= 0.97;   # bright green
    return "\e[32m"       if $s >= 0.90;   # green
    return "\e[33m"       if $s >= 0.80;   # yellow
    return "\e[38;5;208m" if $s >= 0.65;   # orange
    return "\e[31m";                          # red
}

# Render full pattern with per-run colorization; for UB, prefix colored, suffix dimmed
sub _render_pattern_colored {
    my ($self, $p, $obs_runs, $kind, $simlist) = @_;
    my $symmap = $self->{viz_symbols} || {};
    my $s = '';
    my $K = scalar(@$obs_runs);
    for my $i (0 .. $#{$p->{runs}}) {
        my $r = $p->{runs}[$i];
        my $glyph = $symmap->{$r->{sym}} // $r->{sym};
        my $len   = max(1, int(($r->{len}//1) + 0.5));
        my $seg   = ($glyph x $len);
        if ($self->{viz_colorize}) {
            if ($kind eq 'UB' && $i >= $K) {
                $s .= _ansi_dim() . $seg . _ansi_reset();
            } else {
                my $sim = (defined $simlist && defined $simlist->[$i]) ? $simlist->[$i] : undef;
                my $col = _ansi_for_sim($sim);
                $s .= $col . $seg . _ansi_reset();
            }
        } else {
            $s .= $seg;
        }
    }
    return $s;
}

# Convert similarity s∈[0,1] to a penalty 'pen' compatible with _fg_for_pen (NEW)
sub _sim_to_pen {
    my ($s) = @_;
    return 999 if !defined $s || $s <= 0;
    return 0   if $s >= 0.999999;
    # s = exp(-0.5*z^2)  ⇒  z^2 = -2 ln s
    my $z2 = -2.0 * log($s);
    return $z2;
}

# Build per-run info array (sym, pen) for UB prefix (NEW)
sub _sim_prefix_to_perrun {
    my ($p, $sim_prefix) = @_;
    my @out;
    for my $i (0..$#$sim_prefix) {
        my $sym = $p->{runs}[$i]{sym};
        push @out, { sym => $sym, pen => _sim_to_pen($sim_prefix->[$i]) };
    }
    return \@out;
}



1;
