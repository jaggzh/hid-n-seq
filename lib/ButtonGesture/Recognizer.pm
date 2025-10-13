package Recognizer;
use v5.36;
use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(sum min max);
use Term::ReadKey;
use Data::Dumper;
use Digest::MD5 qw(md5);

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
        # Commit guard: block commit if any open UB ≥ threshold
        open_guard          => exists $a{open_guard}           ? !!$a{open_guard}              : 1,
        open_guard_threshold=> defined $a{open_guard_threshold}? 0.0 + $a{open_guard_threshold} : undef,
        # DONE detection
        done_k_sigma        => defined $a{done_k_sigma}        ? 0.0 + $a{done_k_sigma}        : 1.00,
        edge_bonus_weight   => defined $a{edge_bonus_weight}   ? 0.0 + $a{edge_bonus_weight}   : 0.40,
        hold_ub_threshold   => defined $a{hold_ub_threshold}   ? 0.0 + $a{hold_ub_threshold}   : (defined $a{commit_threshold} ? 0.0 + $a{commit_threshold} : 0.75),
        # Initial release handling
        ignore_initial_release => exists $a{ignore_initial_release} ? !!$a{ignore_initial_release} : 1,
        # symbols
        sym_press           => defined $a{sym_press}           ? $a{sym_press}                 : '.',
        sym_release         => defined $a{sym_release}         ? $a{sym_release}               : '~',
        # visualization
        viz_enable          => exists $a{viz_enable}           ? !!$a{viz_enable}              : 1,
        verbose             => defined $a{verbose}             ? 0 + $a{verbose}               : 1,
        viz_symbols         => $a{viz_symbols} // { '.' => '▉', '~' => '▂' },
        viz_symbol_release  => { '.' => '▊' },
        viz_show_colors     => exists $a{viz_show_colors} ? !!$a{viz_show_colors} : 1,
        viz_scale           => defined $a{viz_scale} ? 0.0 + $a{viz_scale} : 1.0,
        viz_abbrev_max      => defined $a{viz_abbrev_max} ? 0 + $a{viz_abbrev_max} : 120,
        viz_user_bg_rgb     => $a{viz_user_bg_rgb} // [ 18, 18, 18 ],
        viz_pat_bg_rgb      => $a{viz_pat_bg_rgb}  // [ 10, 10, 10 ],
        # Color gradient thresholds for run accuracy visualization
        # viz_gradient_thresholds => $a{viz_gradient_thresholds} // [
        #     { min => 0.97, color => [100, 255, 100] },  # bright green
        #     { min => 0.90, color => [50, 220, 50] },    # green
        #     { min => 0.80, color => [255, 255, 0] },    # yellow
        #     { min => 0.65, color => [255, 165, 0] },    # orange
        #     { min => 0.00, color => [255, 50, 50] },    # red
        # ],
        viz_gradient_thresholds => $a{viz_gradient_thresholds} // [
            { min => 0.97, color => [255, 255, 200] },  # faded yellow
            { min => 0.80, color => [225, 255, 225] },  # green
            { min => 0.00, color => [0, 80, 0] },    # dark green
        ],
        # Pattern background color settings
        viz_pattern_bg_saturation_range => $a{viz_pattern_bg_saturation_range} // [0.80, 1.00],
        viz_pattern_bg_brightness_range => $a{viz_pattern_bg_brightness_range} // [0.20, 0.30],
        # Display toggles
        viz_display => {
            name => 1,
            rel_label => 1,
            usr_label => 0,  # Now shown on OBS line by default
            score => 1,
            weight => 1,
            type => 1,
            quotes => 1,
            %{$a{viz_display} // {}},
        },
        # Default colors (explicit, no \e[0m)
        viz_default_fg => $a{viz_default_fg} // [200, 200, 200],
        viz_default_bg => $a{viz_default_bg} // [0, 0, 0],
        # Release edge highlight color (magenta)
        viz_release_edge_bg => $a{viz_release_edge_bg} // [255, 80, 255],
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
        _last_obs_key       => undef,
        _expecting_press    => 1,  # Start expecting a press (ignore initial release)
    }, $class);

    $self->{quantum_s} = $self->{quantum_ms} / 1000.0;

    # VIS: precompute default colors
    $self->{_viz_default_fg_ansi} = _ansi_rgb(@{$self->{viz_default_fg}});
    $self->{_viz_default_bg_ansi} = _ansi_bg(@{$self->{viz_default_bg}});
    $self->{_viz_reset} = $self->{_viz_default_fg_ansi} . $self->{_viz_default_bg_ansi};

    # VIS: precompute release edge bg
    $self->{_viz_release_edge_bg_ansi} = _ansi_bg(@{$self->{viz_release_edge_bg}});

    # VIS: precompute user bg (kept for compatibility)
    $self->{_viz_user_bg} = _ansi_bg(@{$self->{viz_user_bg_rgb}});

    _precompute_patterns($self);

    # init per-pattern peak_done
    for my $p (@{$self->{patterns}}) { $p->{_peak_done} = 0.0; $p->{_ever_done} = 0; }

    return $self;
}

sub reset {
    my ($self) = @_;
    $self->{_events} = [];
    $self->{_first_event_t} = undef;
    $self->{_last_event_t}  = undef;
    $self->{_last_debug_line} = undef;
    $self->{_last_obs_key} = undef;
    $self->{_expecting_press} = 1;  # Reset to expecting press
    # clear per-pattern peak_done each fresh observation
    for my $p (@{$self->{patterns}}) { $p->{_peak_done} = 0.0; $p->{_ever_done} = 0; }
}

# Runtime display toggle
sub viz_set_display {
    my ($self, %opts) = @_;
    if (exists $opts{text}) {
        my $v = !!$opts{text};
        $self->{viz_display}{$_} = $v for qw(name rel_label usr_label score weight type);
    }
    for my $k (keys %opts) {
        next if $k eq 'text';
        $self->{viz_display}{$k} = !!$opts{$k} if exists $self->{viz_display}{$k};
    }
}

# -----------------------------------------------------------------------------
# External interface: feed edges and ticks
# -----------------------------------------------------------------------------
sub press   { my ($self, $t) = @_; $self->_edge('press',   defined $t ? $t : time()); }
sub release { my ($self, $t) = @_; $self->_edge('release', defined $t ? $t : time()); }
sub tick    { my ($self, $t) = @_; $self->_evaluate_if_ready(defined $t ? $t : time()); }

sub _edge {
    my ($self, $kind, $t) = @_;

    # Ignore initial release if configured
    if ($self->{ignore_initial_release} && $self->{_expecting_press} && $kind eq 'release') {
        # Silently ignore this release - we're waiting for a press to start
        if ($self->{verbose} >= 2) {
            print "[Ignoring initial release - waiting for press]\n";
        }
        return;
    }

    # First press marks the start of valid gesture tracking
    if ($kind eq 'press') {
        $self->{_expecting_press} = 0;  # Now accepting all events
    }

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
        # per-pattern cached transition bonus weight (allows per-pattern override later)
        $p->{_edge_bonus_weight} = (defined $p->{edge_bonus_weight} ? 0.0 + $p->{edge_bonus_weight} : $self->{edge_bonus_weight});

        # Generate unique background color for this pattern based on name hash
        $p->{_viz_bg_ansi} = $self->_pattern_bg_color($p->{name} // $p->{id} // '');
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

    my $obs_key = join(',', map { $_->{sym} . $_->{len} } @$obs_runs);
    if (defined $self->{_last_obs_key} && $self->{_last_obs_key} eq $obs_key) {
        return;  # No change, skip evaluation
    }
    $self->{_last_obs_key} = $obs_key;


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

        # INVALIDATION: obs exceeds pattern length
        # BUT: if pattern ends with press (dot), and obs is pattern+1 with final release, that's OK (it's the DONE signal)
        my $pattern_ends_with_press = ($p->{runs}[-1]{sym} eq $self->{sym_press});
        my $obs_is_just_release_after = (@$obs_runs == @{$p->{runs}} + 1 &&
                                          $obs_runs->[-1]{sym} eq $self->{sym_release});

        if (@$obs_runs > @{$p->{runs}} && !($pattern_ends_with_press && $obs_is_just_release_after)) {

            # Add to @full for viz, but mark as invalidated
            push @full, {
                name        => ($p->{name} // $p->{id} // "pattern_$idx"),
                score       => 0.0,
                raw         => 0.0,
                idx         => $idx,
                perrun_info => [],
                runs_count  => $p->{runs_count},
                len_total   => $p->{len_total},
                is_done     => 0,
                peak_done   => 0.0,
                invalidated => 1,
            };
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
            $p->{_ever_done} = 1;
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
            invalidated => 0,
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
        # RELAXED: allow equal-length (LAST) patterns too; skip only if that pattern is already DONE
        next unless _runs_prefix_relaxed($obs_runs, $q->{runs});    # symbol-prefix (<= length) match
        next if (@$obs_runs == @{$q->{runs}} && $self->_pattern_is_done($obs_runs, $q, $last_kind));
        my ($ub_raw, undef) = _prefix_ub_with_perrun($obs_runs, $q); # optimistic suffix gated by current run
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

    # --------- Peak-DONE decision ---------------------------------------
    # Choose best DONE by peak; compare against max UB of others (with patience)
    my $threshold  = $self->{commit_threshold};
    my $margin     = $self->{commit_margin};
    my $patience   = $self->{patience};

    my $best_done_idx   = -1;
    my $best_done_score = 0.0;
    for my $c (@full) {
        my $p = $self->{patterns}[$c->{idx}];

        if ($self->{verbose} >= 2 && $c->{name} eq 'click') {
            printf("[DEBUG] Checking click: invalidated=%s obs_runs=%d pat_runs=%d is_done=%s peak_done=%.3f\n",
                   ($c->{invalidated} ? 'YES' : 'NO'),
                   scalar(@$obs_runs), scalar(@{$p->{runs}}),
                   ($c->{is_done} ? 'YES' : 'NO'),
                   $c->{peak_done});
        }

        # Skip invalidated patterns
        next if $c->{invalidated};

        # INVALIDATION: Skip patterns where observation has exceeded pattern length
        # BUT: allow dot-ended patterns with one extra release run (DONE signal)
        my $pattern_ends_with_press = ($p->{runs}[-1]{sym} eq $self->{sym_press});
        my $obs_is_just_release_after = (@$obs_runs == @{$p->{runs}} + 1 &&
                                          $obs_runs->[-1]{sym} eq $self->{sym_release});
        next if @$obs_runs > @{$p->{runs}} && !($pattern_ends_with_press && $obs_is_just_release_after);

        # Only consider patterns that are actually DONE and meet threshold
        next unless $c->{is_done};
        next unless $c->{peak_done} >= $threshold;

        if ($c->{peak_done} > $best_done_score) {
            $best_done_score = $c->{peak_done};
            $best_done_idx   = $c->{idx};
        }
    }

    if ($self->{verbose} >= 2) {
        printf("[DEBUG] best_done_idx=%d best_done_score=%.3f threshold=%.3f\n",
               $best_done_idx, $best_done_score, $threshold);
    }

    if ($best_done_idx >= 0 && $best_done_score >= $threshold) {

        # (A) max UB among all plausible competitors (existing logic)
        my $ub_competitors = 0.0;
        for my $j (0 .. $#{$self->{patterns}}) {
            next if $j == $best_done_idx;
            my $q = $self->{patterns}[$j];

            # INVALIDATION: Skip patterns we've already exceeded
            next if @$obs_runs > @{$q->{runs}};

            next unless _runs_prefix($obs_runs, $q->{runs});
            next if @$obs_runs >= @{$q->{runs}} && $self->_pattern_is_done($obs_runs, $q, $last_kind);
            my ($ub_raw_j, undef) = _prefix_ub_with_perrun($obs_runs, $q);

            # INVALIDATION by UB: If UB is below threshold, pattern has failed
            next if $ub_raw_j < $threshold;

            my $ub_w_j = ($ub_raw_j * ($q->{weight} // 1.0)) * $patience;
            $ub_competitors = $ub_w_j if $ub_w_j > $ub_competitors;
        }

        if ($self->{verbose} >= 2) {
            printf("[DEBUG] ub_competitors=%.3f patience=%.3f\n",
                   $ub_competitors, $patience);
        }

        if (1) {
            # (B) NEW: Superset hold — if any strict superset of best_done is still viable and high, defer commit
            my $hold_th   = defined($self->{hold_ub_threshold}) ? $self->{hold_ub_threshold} : $threshold;
            my $hold_max  = 0.0;
            my $p_best    = $self->{patterns}[$best_done_idx];

            for my $j (0 .. $#{$self->{patterns}}) {
                next if $j == $best_done_idx;
                my $q = $self->{patterns}[$j];

                # INVALIDATION: Skip if obs exceeds this pattern
                next if @$obs_runs > @{$q->{runs}};

                next unless _is_strict_superset($q, $p_best);

                my $val = 0.0;
                if (@$obs_runs < @{$q->{runs}}) {
                    my ($ub_raw_j, undef) = _prefix_ub_with_perrun($obs_runs, $q);

                    # INVALIDATION by UB: If UB below threshold, pattern has failed
                    next if $ub_raw_j < $threshold;

                    $val = $ub_raw_j * ($q->{weight} // 1.0);
                } else {
                    # If already FULL for q, use its current weighted score (or recompute) as a conservative competitor
                    my ($full_q) = grep { $_->{idx} == $j } @full;
                    if ($full_q && !$full_q->{invalidated}) {
                        $val = $full_q->{score};
                    } else {
                        my ($raw_q, undef) = _full_score_variance($obs_runs, $q);
                        $val = $raw_q * ($q->{weight} // 1.0);
                    }
                }
                $val *= $patience;
                $hold_max = $val if $val > $hold_max;
            }

            if ($self->{verbose} >= 2) {
                printf("[DEBUG] superset hold_max=%.3f hold_th=%.3f (blocking=%s)\n",
                       $hold_max, $hold_th, ($hold_max >= $hold_th ? "YES" : "NO"));
            }

            # If a strict superset still has enough headroom, wait.
            return if $hold_max >= $hold_th;
        }

        if ($self->{verbose} >= 2) {
            printf("[DEBUG] Final check: %.3f >= %.3f + %.3f? (%s)\n",
                   $best_done_score, $ub_competitors, $margin,
                   ($best_done_score >= $ub_competitors + $margin ? "YES->COMMIT" : "NO->WAIT"));
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

    # Smart idle timeout: don't timeout during an active press if a dot-ended pattern is viable
    my $has_viable_dot_pattern = 0;
    if ($self->{hard_stop_idle_s} && $idle_s >= $self->{hard_stop_idle_s}) {
        # Check if we're currently in a press and any dot-ended pattern is still viable
        if (@{$self->{_events}} > 0) {
            my ($last_kind, $last_t) = @{$self->{_events}[-1]};
            if ($last_kind eq 'press') {
                # We're actively pressing - check if any dot-ended patterns are viable
                for my $c (@full) {
                    next if $c->{invalidated};
                    my $p = $self->{patterns}[$c->{idx}];
                    next if @$obs_runs > @{$p->{runs}};  # Skip exceeded patterns

                    # Check if pattern ends with press and has viable score/UB
                    if (($p->{runs}[-1]{sym} // '') eq $self->{sym_press}) {
                        if ($c->{score} >= $threshold || $c->{is_done}) {
                            $has_viable_dot_pattern = 1;
                            last;
                        }
                    }
                }
            }
        }

        # Only timeout if no viable dot patterns during active press
        if (!$has_viable_dot_pattern) {
            $self->_no_match($t, 'idle_timeout');
            return;
        }
    }

    if ($self->{hard_stop_span_s} && $span_s >= $self->{hard_stop_span_s}) {
        $self->_no_match($t, 'span_timeout');
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

        # CRITICAL: Symbol mismatch means zero score
        if (($r->{sym} // '') ne ($or->{sym} // '')) {
            return (0.0, []);
        }

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
    my $raw = ($w_sum>0 ? $acc/$w_sum : 0.0);
    # my $edges_total = ($L > 1) ? ($L - 1) : 1;
    # say "DEBUG: Pattern=$p->{name} raw_before_edge=$raw edges_total=$edges_total" if $p->{name} eq 'click';


    # transition (edge) bonus — more observed edges ⇒ slightly higher raw
    my $ew = $p->{_edge_bonus_weight} // 0.0;
    if ($ew > 0) {
        my $edges_total = ($L > 1) ? ($L - 1) : 1;
        my $edges_have  = max(0, min($edges_total, scalar(@$obs_runs) - 1));
        my $edge_frac   = ($edges_total > 0) ? ($edges_have / $edges_total) : 1.0;
        $raw *= (1.0 + $ew * $edge_frac);
        $raw  = 1.0 if $raw > 1.0;  # keep raw ≤ 1 before pattern weight
    }

    return ($raw, \@per_info);
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

# UB with per-run similarities (for colored viz prefix)
sub _prefix_ub_with_perrun {
    my ($obs_runs, $q) = @_;
    my $qruns = $q->{runs};
    my $K = scalar(@$obs_runs);

    my $w_sum = 0.0; my $acc = 0.0;
    my @sim_prefix;

    # prefix: earlier closed runs scored as observed
    for my $i (0..$K-2) {
        my $r  = $qruns->[$i] or last;
        my $mu = 0.0 + ($r->{len}      // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight}   // 1.0);
        my $x  = 0.0 + ($obs_runs->[$i]{len} // 0);
        my $eps = 0.25;
        my $z   = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s   = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
        $sim_prefix[$i] = $s;
    }

    # current open run (salvage-aware) - gates the suffix
    my $g = 1.0;  # Salvageability gate for suffix
    if ($K >= 1) {
        my $i  = $K-1;
        if (my $r = $qruns->[$i]) {
            my $mu = 0.0 + ($r->{len}      // 1);
            my $sd = 0.0 + ($r->{variance} // 1.0);
            my $w  = 0.0 + ($r->{weight}   // 1.0);
            my $x  = 0.0 + ($obs_runs->[$i]{len} // 0);
            my $eps = 0.25;
            my $z_now = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
            my $s_now = exp(-0.5 * $z_now * $z_now);
            # salvage: if we can still reach μ by ending soon, UB=1 for this run; otherwise best is current
            my $s_feasible = ($x <= $mu) ? 1.0 : $s_now;
            $acc  += $w * $s_feasible;
            $w_sum+= $w;
            $sim_prefix[$i] = $s_feasible;

            # Gate suffix by current run's salvageability
            $g = $s_feasible;
        }
    }

    # suffix: gated by current run salvageability (not perfect 1.0!)
    for my $i ($K..$#$qruns) {
        my $w = 0.0 + ($qruns->[$i]{weight} // 1.0);
        $acc  += $w * $g;  # <-- Changed from 1.0 to $g
        $w_sum+= $w;
    }

    my $ub = ($w_sum>0 ? $acc/$w_sum : 0.0);
    return ($ub, \@sim_prefix);
}

# -----------------------------------------------------------------------------
# Scoring (variance-driven)
# -----------------------------------------------------------------------------

# best-case from "now" to completion for FULL-but-not-done patterns
sub _bestcase_continue_score {
    my ($obs_runs, $p) = @_;
    my $pruns = $p->{runs};
    my $L = scalar(@$pruns);

    my $w_sum = 0.0; my $acc = 0.0;
    for my $i (0..$L-2) {                               # all fixed earlier runs
        my $r  = $pruns->[$i];
        my $mu = 0.0 + ($r->{len}      // 1);
        my $sd = 0.0 + ($r->{variance} // 1.0);
        my $w  = 0.0 + ($r->{weight}   // 1.0);
        my $x  = 0.0 + ($obs_runs->[$i]{len} // 0);
        my $eps= 0.25;
        my $z  = ($sd > $eps) ? (($x - $mu)/$sd) : (($x - $mu)/$eps);
        my $s  = exp(-0.5 * $z * $z);
        $acc  += $w * $s;
        $w_sum+= $w;
    }

    # last run: salvage-aware (can we still hit μ?)
    my $rL  = $pruns->[$L-1];
    my $muL = 0.0 + ($rL->{len}      // 1);
    my $sdL = 0.0 + ($rL->{variance} // 1.0);
    my $wL  = 0.0 + ($rL->{weight}   // 1.0);
    my $xL  = 0.0 + ($obs_runs->[$L-1]{len} // 0);
    my $eps = 0.25;
    my $zL  = ($sdL > $eps) ? (($xL - $muL)/$sdL) : (($xL - $muL)/$eps);
    my $s_now = exp(-0.5 * $zL * $zL);
    my $s_feasible = ($xL <= $muL) ? 1.0 : $s_now;

    $acc  += $wL * $s_feasible;
    $w_sum+= $wL;

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

# strict superset check for patterns (by symbol prefix only)
sub _is_strict_superset {
    my ($super_p, $sub_p) = @_;
    my $A = $sub_p->{runs} // [];
    my $B = $super_p->{runs} // [];
    return 0 unless @$B > @$A;
    for my $i (0..$#$A) {
        return 0 unless ($A->[$i]{sym} // '') eq ($B->[$i]{sym} // '');
    }
    return 1;
}

# RELAXED prefix: allow equal-length (used for LAST patterns still in play)
sub _runs_prefix_relaxed {
    my ($a_ref, $b_ref) = @_;
    my $n = scalar(@$a_ref);
    return 0 unless scalar(@$b_ref) >= $n;  # allow equal length
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
    my ($self, $t, $reason) = @_;
    $reason //= 'timeout';  # Default reason

    print sprintf("[t=+%.03fs] NO_MATCH (reset: %s)\n",
                  ($self->{_first_event_t} ? $t - $self->{_first_event_t} : 0.0),
                  $reason) if $self->{verbose};

    $self->{callback}->('NO_MATCH', undef, {
        t_end => $t,
        reason => $reason
    }) if $self->{callback};

    $self->reset();
}

# -----------------------------------------------------------------------------
# Visualization helpers (colors, backgrounds, abbreviated renderings)
# -----------------------------------------------------------------------------
sub _ansi_rgb   { return sprintf("\e[38;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_bg    { return sprintf("\e[48;2;%d;%d;%dm", $_[0], $_[1], $_[2]); }
sub _ansi_dim   { "\e[2m" }

# Generate pattern background color from name hash
sub _pattern_bg_color {
    my ($self, $name) = @_;

    # Hash the name to get consistent color
    my $hash = unpack('N', md5($name));

    # Extract values from hash
    my $hue = ($hash & 0xFFFF) / 0xFFFF;  # 0-1

    # Get configured ranges
    my ($sat_min, $sat_max) = @{$self->{viz_pattern_bg_saturation_range}};
    my ($bri_min, $bri_max) = @{$self->{viz_pattern_bg_brightness_range}};

    # Map to configured saturation and brightness ranges
    my $sat = $sat_min + (($hash >> 16) & 0xFF) / 255.0 * ($sat_max - $sat_min);
    my $bri = $bri_min + (($hash >> 8) & 0xFF) / 255.0 * ($bri_max - $bri_min);

    # Convert HSV to RGB
    my ($r, $g, $b) = _hsv_to_rgb($hue, $sat, $bri);

    return _ansi_bg($r, $g, $b);
}

# HSV to RGB conversion
sub _hsv_to_rgb {
    my ($h, $s, $v) = @_;

    my $c = $v * $s;
    my $x = $c * (1 - abs(($h * 6) % 2 - 1));
    my $m = $v - $c;

    my ($r1, $g1, $b1);
    if ($h < 1/6) {
        ($r1, $g1, $b1) = ($c, $x, 0);
    } elsif ($h < 2/6) {
        ($r1, $g1, $b1) = ($x, $c, 0);
    } elsif ($h < 3/6) {
        ($r1, $g1, $b1) = (0, $c, $x);
    } elsif ($h < 4/6) {
        ($r1, $g1, $b1) = (0, $x, $c);
    } elsif ($h < 5/6) {
        ($r1, $g1, $b1) = ($x, 0, $c);
    } else {
        ($r1, $g1, $b1) = ($c, 0, $x);
    }

    my $r = int(($r1 + $m) * 255 + 0.5);
    my $g = int(($g1 + $m) * 255 + 0.5);
    my $b = int(($b1 + $m) * 255 + 0.5);

    return ($r, $g, $b);
}

sub _render_len_chars {
    my ($runs, $scale) = @_;
    my $tot = 0;
    for my $r (@$runs) { $tot += max(1, int(($r->{len}//1) * $scale + 0.5)); }
    return $tot;
}

# Convert similarity s∈[0,1] to a penalty 'pen' compatible with coloring
sub _sim_to_pen {
    my ($s) = @_;
    return 999 if !defined $s || $s <= 0;
    return 0   if $s >= 0.999999;
    # s = exp(-0.5*z^2)  ⇒  z^2 = -2 ln s
    my $z2 = -2.0 * log($s);
    return $z2;
}

# Build per-run info array (sym, pen, sim) for UB prefix
sub _sim_prefix_to_perrun {
    my ($p, $sim_prefix) = @_;
    my @out;
    for my $i (0..$#$sim_prefix) {
        my $sym = $p->{runs}[$i]{sym};
        my $sim = $sim_prefix->[$i];
        push @out, { sym => $sym, pen => _sim_to_pen($sim), sim => $sim };
    }
    return \@out;
}

# Facet helpers for labeling: USR and REL
sub _facet_usr_label {
    my ($self) = @_;
    return 'USR:UNKNOWN' unless @{$self->{_events}};
    my ($k, $t) = @{$self->{_events}[-1]};
    return ($k && $k eq 'press') ? 'USR:PRESS' : 'USR:RELEASE';
}

sub _facet_rel_label {
    my ($self, $obs_runs, $p, $is_done) = @_;
    my $Lobs = scalar(@$obs_runs);
    my $Lpat = $p->{runs_count} // scalar(@{$p->{runs}});

    # Check if this is the special case: dot-ended pattern with release after
    my $pattern_ends_with_press = ($p->{runs}[-1]{sym} eq $self->{sym_press});
    my $obs_is_just_release_after = ($Lobs == $Lpat + 1 &&
                                      $obs_runs->[-1]{sym} eq $self->{sym_release});

    # INVALIDATED by run count exceeding pattern (except for dot+release case)
    if ($Lobs > $Lpat && !($pattern_ends_with_press && $obs_is_just_release_after)) {
        return 'REL:INVALID';
    }

    # If this pattern was ever done earlier, reflect that stably in the facet.
    if ($p->{_ever_done}) {
        return 'REL:DONE';
    }
    return 'REL:LAST'  if $Lobs >= $Lpat;
    return sprintf('REL:PART %d/%d', $Lobs, $Lpat);
}

sub _debug_dump_observation {
    my ($self, $obs_runs, $dt) = @_;
    my $q = $self->{quantum_ms};
    my $usr_label = $self->_facet_usr_label();

    printf("[t=+%.03fs] OBS q=%dms len=%dq (~%dms) %s\n",
           $dt, $q, scalar(@$obs_runs),
           int(sum(map { $_->{len} } @$obs_runs) * $self->{quantum_ms}),
           $usr_label);
}

sub _debug_dump_candidates {
    my ($self, $obs_runs, $full_ref, $best_ub_w, $dt) = @_;

    # Build user observation viz
    my $usr = $self->_viz_pattern_str($obs_runs, 1);

    # Pre-generate all pattern lines and track max prefix width
    my @rendered;
    my $max_prefix_width = 0;

    my %seen_full = map { $self->{patterns}[ $_->{idx} ]{name} // $_->{idx} => 1 } @$full_ref;

    # FULL-ish rows (prefix-covered or invalidated)
    for my $c (@$full_ref) {
        my $p = $self->{patterns}[$c->{idx}];
        my ($prefix, $viz, $width) = $self->_build_pattern_line($c, $p, $obs_runs, 'FULL');
        $max_prefix_width = $width if $width > $max_prefix_width;
        push @rendered, { prefix => $prefix, viz => $viz, bg => $p->{_viz_bg_ansi}, type => 'pat' };
    }

    # UB rows: strict supersequences of observation prefix
    for my $j (0 .. $#{$self->{patterns}}) {
        my $p = $self->{patterns}[$j];
        my $name = $p->{name} // $p->{id} // "pattern_$j";
        next if $seen_full{$name};
        next unless @$obs_runs < @{$p->{runs}};
        next unless _runs_prefix($obs_runs, $p->{runs});

        my ($ub_raw, $sim_prefix) = _prefix_ub_with_perrun($obs_runs, $p);
        my $ub_w = $ub_raw * ($p->{weight} // 1.0);

        # Build candidate structure for UB
        my $k = scalar(@$obs_runs);
        my $per = _sim_prefix_to_perrun($p, $sim_prefix);
        my $rel = sprintf('REL:PART %d/%d', $k, scalar(@{$p->{runs}}));

        my $c_ub = {
            name => $name,
            score => $ub_w,
            raw => $ub_raw,
            idx => $j,
            perrun_info => $per,
            runs_count => scalar(@{$p->{runs}}),
            len_total => $p->{len_total},
            is_done => 0,
            peak_done => 0,
            invalidated => 0,
            is_ub => 1,
            ub_prefix_len => $k,
        };

        my ($prefix, $viz, $width) = $self->_build_pattern_line($c_ub, $p, $obs_runs, 'UB');
        $max_prefix_width = $width if $width > $max_prefix_width;
        push @rendered, { prefix => $prefix, viz => $viz, bg => $p->{_viz_bg_ansi}, type => 'pat' };
    }

    # Print usr line with proper alignment
    my $usr_type = $self->{viz_display}{type} ? 'usr=' : '';
    my $q_open = $self->{viz_display}{quotes} ? '"' : '';
    my $q_close = $self->{viz_display}{quotes} ? '"' : '';

    printf("%*s%s%s%s%s\n", $max_prefix_width, '', $usr_type, $q_open, $usr, $q_close);

    # Print all pattern lines
    for my $line (@rendered) {
        print $line->{bg} . $line->{prefix};
        my $pat_type = $self->{viz_display}{type} ? ' pat=' : ' ';
        print $pat_type . $q_open . $line->{viz} . $q_close;
        print $self->{_viz_reset} . "\n";
    }
}

# Build a single pattern line with prefix and viz
sub _build_pattern_line {
    my ($self, $c, $p, $obs_runs, $kind) = @_;  # kind = 'FULL' or 'UB'

    my @parts;
    my $prefix_width = 2;  # Leading "  "

    # Name field
    if ($self->{viz_display}{name}) {
        my $name_str = sprintf("%-14s", $c->{name});
        push @parts, $name_str;
        $prefix_width += length($name_str) + 1;
    }

    # REL label field
    if ($self->{viz_display}{rel_label}) {
        my $rel = $self->_facet_rel_label($obs_runs, $p, $c->{is_done});
        my $rel_str = sprintf("%-14s", $rel);
        push @parts, $rel_str;
        $prefix_width += length($rel_str) + 1;
    }

    # Score field
    if ($self->{viz_display}{score}) {
        my $score_str;
        if ($kind eq 'UB') {
            $score_str = sprintf("UB=%.3f", $c->{score});
        } elsif ($c->{invalidated}) {
            $score_str = "score=0.000 INV";
        } else {
            $score_str = sprintf("score=%.3f", $c->{score});
        }
        push @parts, $score_str;
        $prefix_width += length($score_str) + 1;
    }

    # Weight field
    if ($self->{viz_display}{weight}) {
        my $w_str = sprintf("w=%.2f", $p->{weight} // 1.0);
        push @parts, $w_str;
        $prefix_width += length($w_str) + 1;
    }

    my $prefix = "  " . join(' ', @parts);

    # Build visualization
    my $viz;
    if ($kind eq 'UB') {
        # UB: prefix colored, suffix dimmed
        $viz = $self->_viz_pattern_abbrev_perrun($c->{perrun_info}, $p->{runs}, $c->{ub_prefix_len} // 0);
    } else {
        # FULL: all runs colored by per-run info
        my $k = $p->{runs_count};
        $viz = $self->_viz_pattern_abbrev_perrun($c->{perrun_info}, $p->{runs}, $k);
    }

    return ($prefix, $viz, $prefix_width);
}

# Full pattern with bg; colored by per-run similarity (user observation)
sub _viz_pattern_str {
    my ($self, $runs, $is_user) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $bg      = $is_user ? $self->{_viz_user_bg} : $self->{_viz_default_bg_ansi};
    my $s = $bg;

    for my $r (@$runs) {
        my $n  = max(1, int(($r->{len}//1) * $scale + 0.5));
        my $ch = $symbols->{$r->{sym}} // $r->{sym};

        # User observation gets perfect similarity (pen=0, sim=1.0)
        my $fg = $self->_fg_for_pen($r->{sym}, 0, 1.0);

        if ($is_user) {
			if ($r->{sym} eq $self->{sym_release}) {
				$s .= $fg . ($ch x $n);
			} else {
				$s .= $fg . ($ch x ($n-1));
				$ch = $self->{viz_symbol_release}{$r->{sym}} // $ch;
				$s .= $self->{_viz_release_edge_bg_ansi} . $fg . $ch . $bg;
			}
		} else { # Pattern just gets the similarity color on each run
            $s .= $fg . ($ch x $n);
        }
    }
    return $s . $self->{_viz_reset};
}

# Enhanced gradient for run accuracy visualization
# pen==0 → near-white; higher pen → darker base color per symbol family
# At very high similarity (>90%), transition to yellow
sub _fg_for_pen {
    my ($self, $sym, $pen, $sim) = @_;
    return '' unless $self->{viz_show_colors};

    # If similarity provided and very high (>0.90), transition to yellow
    # Standard gradient based on penalty (0-0.90 similarity range)
    my $alpha = 1.0 / (1.0 + ($pen // 0));  # (0,1], 0-pen ~ near-white
    my ($r1, $g1, $b1, $r2, $g2, $b2);
    if ($sym eq '~') {
        ($r1, $g1, $b1) = (120, 120, 205);  # near-white blue
        ($r2, $g2, $b2) = (0, 0, 120);      # dark blue
    } else {
        ($r1, $g1, $b1) = (120, 255, 120);  # near-white green
        ($r2, $g2, $b2) = (0, 80, 0);       # dark green
    }
    my $r = int($r2 + ($r1 - $r2) * $alpha + 0.0);
    my $g = int($g2 + ($g1 - $g2) * $alpha + 0.0);
    my $b = int($b2 + ($b1 - $b2) * $alpha + 0.0);
    return _ansi_rgb($r, $g, $b);
}


# Abbreviated pattern for pat="...": prefix colored by per-run penalty; remainder dim
sub _viz_pattern_abbrev_perrun {
    my ($self, $perrun, $p_runs, $k) = @_;
    my $scale   = $self->{viz_scale};
    my $symbols = $self->{viz_symbols};
    my $maxc    = $self->{viz_abbrev_max};
    my $out     = '';
    my $count   = 0;

    # Defensive: synthesize empty per-run info if missing
    my @fallback;
    if (ref($perrun) ne 'ARRAY' || !@$perrun) {
        @fallback = map { +{ sym => $_->{sym}, pen => 999, sim => 0.1 } } @$p_runs;
        $perrun = \@fallback;
    }

    # prefix (colored by per-run similarity/penalty)
    for my $i (0..($k-1)) {
        my $sym  = (defined $perrun->[$i] && exists $perrun->[$i]{sym})
                   ? $perrun->[$i]{sym} : ($p_runs->[$i]{sym} // '.');
        my $pen  = (defined $perrun->[$i] && exists $perrun->[$i]{pen})
                   ? $perrun->[$i]{pen} : 0;
        my $sim  = (defined $perrun->[$i] && exists $perrun->[$i]{sim})
                   ? $perrun->[$i]{sim} : 1.0;
        my $n    = max(1, int(($p_runs->[$i]{len}//1) * $scale + 0.5));
        my $ch   = $symbols->{$sym} // $sym;

        my $fg = $self->_fg_for_pen($sym, $pen, $sim);

		my $chars_to_output = min($n, $maxc - $count);
		if ($chars_to_output > 0) {
			$out .= $fg . ($ch x $chars_to_output);
			$count += $chars_to_output;
		}

		last if $count >= $maxc;
    }

    # remainder (dimmed)
    my $dim_fg = _ansi_rgb(80, 80, 80);
    for my $i ($k..$#$p_runs) {
        my $sym = $p_runs->[$i]{sym};
        my $n   = max(1, int(($p_runs->[$i]{len}//1) * $scale + 0.5));
        my $ch  = $symbols->{$sym} // $sym;

		my $chars_to_output = min($n, $maxc);
		if ($chars_to_output > 0) {
			$out .= $dim_fg . ($ch x $chars_to_output);
			$count += $chars_to_output;
		}
        last if $count >= $maxc;
    }

    $out .= "…" if $count >= $maxc;
    return $out;
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


1;
