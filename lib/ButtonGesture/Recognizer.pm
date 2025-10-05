#!/usr/bin/perl
package ButtonGesture::Recognizer;

use strict;
use warnings;
use Time::HiRes qw(time);
use List::Util qw(max);
use YAML::XS qw(LoadFile);

our $VERSION = '0.01';

# Constants
use constant QUANTUM_MS => 50;  # 50ms per quantum
use constant QUANTUM_S  => 0.05;

sub new {
    my ($class, %args) = @_;
    
    my $self = {
        config_file    => $args{config_file} || 'gestures.yaml',
        patterns       => [],
        candidates     => [],
        current_seq    => [],
        quantum_clock  => 0,
        last_tick      => time(),
        button_down    => 0,
        press_start    => 0,
        last_input     => 0,
        last_release   => 0,
        verbose        => $args{verbose} || 0,
        callback       => $args{callback} || sub { print "Action: $_[0]\n" },
        max_wait       => 30,  # max quanta to wait (1.5s default)
    };
    
    bless $self, $class;
    $self->load_config();
    $self->reset();
    return $self;
}

sub load_config {
    my $self = shift;
    
    # For now, hardcode some patterns for testing
    # Later we'll load from YAML
    $self->{patterns} = [
        {
            name => 'tap',
            pattern => '.~)',
            weight => 1.0,
            elasticity => { '.' => 2, '~' => 10 },
        },
        {
            name => 'double_tap',
            pattern => '.~.~)',
            weight => 1.5,
            elasticity => { '.' => 2, '~' => 8 },
        },
        {
            name => 'long_press',
            pattern => '------)',
            weight => 0.8,
            elasticity => { '-' => 5 },
        },
        {
            name => 'triple_tap',
            pattern => '.~.~.~)',
            weight => 1.8,
            elasticity => { '.' => 2, '~' => 6 },
        },
        {
            name => 'press_tap',
            pattern => '----~.~)',
            weight => 1.3,
            elasticity => { '-' => 4, '.' => 2, '~' => 8 },
        },
    ];
    
    # Load from file if it exists
    if (-f $self->{config_file}) {
        eval {
            my $config = LoadFile($self->{config_file});
            if ($config->{patterns}) {
                $self->{patterns} = $config->{patterns};
            }
            $self->{max_wait} = $config->{max_wait} if $config->{max_wait};
        };
        warn "Config load error: $@" if $@ && $self->{verbose};
    }
}

sub reset {
    my $self = shift;
    $self->{current_seq} = [];
    $self->{candidates} = [ @{$self->{patterns}} ];
    $self->{quantum_clock} = 0;
    $self->{last_input} = $self->{quantum_clock};
    
    # Give each candidate a current score
    for my $c (@{$self->{candidates}}) {
        $c->{score} = 0;
        $c->{possible} = 1;
    }
}

sub button_press {
    my $self = shift;
    
    return if $self->{button_down};  # Already pressed
    
    $self->{button_down} = 1;
    $self->{press_start} = time();
    $self->{last_input} = $self->{quantum_clock};
    
    print "PRESS at quantum $self->{quantum_clock}\n" if $self->{verbose};
    $self->tick();  # Immediate tick to update state
}

sub button_release {
    my $self = shift;
    
    return unless $self->{button_down};  # Not pressed
    
    $self->{button_down} = 0;
    my $duration = time() - $self->{press_start};
    $self->{last_release} = time();
    $self->{last_input} = $self->{quantum_clock};
    
    print "RELEASE at quantum $self->{quantum_clock} (held for ${duration}s)\n" if $self->{verbose};
    $self->tick();  # Immediate tick to process release
}

sub tick {
    my $self = shift;
    my $now = time();
    
    # Update quantum clock
    my $elapsed = $now - $self->{last_tick};
    my $quanta_passed = int($elapsed / QUANTUM_S);
    
    if ($quanta_passed > 0) {
        $self->{quantum_clock} += $quanta_passed;
        $self->{last_tick} = $now;
    }
    
    # Update current sequence
    if ($self->{button_down}) {
        push @{$self->{current_seq}}, '*';  # Button pressed
    } else {
        push @{$self->{current_seq}}, '_';  # Button released
    }
    
    # Normalize and match
    my $normalized = $self->normalize_sequence($self->{current_seq});
    $self->update_candidates($normalized);
    
    # Check if we should trigger
    if ($self->should_trigger()) {
        $self->execute_best_match();
    }
}

sub normalize_sequence {
    my ($self, $seq) = @_;
    return '' unless @$seq;
    
    my $str = join('', @$seq);
    my $normalized = '';
    
    # Convert runs to symbols
    while ($str =~ /(\*+|_+)/g) {
        my $run = $1;
        my $len = length($run);
        my $quanta = $len;
        
        if (substr($run, 0, 1) eq '*') {
            # Press duration
            if ($quanta <= 2) {
                $normalized .= '.';  # Short press
            } elsif ($quanta <= 10) {
                $normalized .= '-' x int($quanta / 2);  # Medium press
            } else {
                $normalized .= '-' x 5 . '+';  # Long press
            }
        } else {
            # Pause duration
            if ($quanta <= 3) {
                $normalized .= '~';  # Short pause
            } elsif ($quanta <= 10) {
                $normalized .= '~' x int($quanta / 3);  # Medium pause
            } else {
                $normalized .= '~~~+';  # Long pause
            }
        }
    }
    
    return $normalized;
}

sub update_candidates {
    my ($self, $user_seq) = @_;
    
    for my $candidate (@{$self->{candidates}}) {
        $candidate->{score} = $self->elastic_match($user_seq, $candidate);
        $candidate->{possible} = $candidate->{score} > 0;
    }
    
    # Filter out impossible candidates
    @{$self->{candidates}} = grep { $_->{possible} } @{$self->{candidates}};
    
    if ($self->{verbose}) {
        print "User sequence: '$user_seq'\n";
        print "Active candidates:\n";
        for my $c (@{$self->{candidates}}) {
            printf "  %s (pattern: %s) score: %.3f\n", 
                $c->{name}, $c->{pattern}, $c->{score};
        }
    }
}

sub elastic_match {
    my ($self, $user_seq, $pattern) = @_;
    
    my $patt_seq = $pattern->{pattern};
    
    # Remove terminal marker for matching
    $patt_seq =~ s/\)$//;
    
    # Check if user sequence could be prefix of pattern
    return 0 if length($user_seq) > length($patt_seq);
    
    # Basic prefix matching with elasticity
    my $score = 1.0;
    my $patt_pos = 0;
    my $user_pos = 0;
    
    while ($user_pos < length($user_seq) && $patt_pos < length($patt_seq)) {
        my $user_char = substr($user_seq, $user_pos, 1);
        my $patt_char = substr($patt_seq, $patt_pos, 1);
        
        if ($user_char eq $patt_char) {
            # Exact match
            $score *= 1.0;
            $user_pos++;
            $patt_pos++;
        } elsif ($self->chars_compatible($user_char, $patt_char)) {
            # Compatible but not exact (elastic match)
            $score *= 0.8;
            $user_pos++;
            $patt_pos++;
        } else {
            # Check if we can skip pattern chars (elasticity)
            my $can_skip = 0;
            if ($patt_char eq '~' || $patt_char eq '-') {
                # These can be elastic
                $patt_pos++;
                $can_skip = 1;
                $score *= 0.9;
            }
            
            return 0 unless $can_skip;
        }
    }
    
    # Penalize incomplete patterns
    my $completion = $user_pos / length($patt_seq);
    $score *= $completion;
    
    # Apply weight
    $score *= $pattern->{weight};
    
    return $score;
}

sub chars_compatible {
    my ($self, $a, $b) = @_;
    
    # Define compatibility rules
    return 1 if $a eq $b;
    return 1 if ($a eq '.' && $b eq '-');  # Short can match medium
    return 1 if ($a eq '-' && $b eq '.');  # Medium can match short
    return 1 if ($a eq '~' && $b =~ /~/);   # Pauses are flexible
    
    return 0;
}

sub should_trigger {
    my $self = shift;
    
    return 0 unless @{$self->{candidates}};
    
    # Sort by score
    my @sorted = sort { $b->{score} <=> $a->{score} } @{$self->{candidates}};
    my $best = $sorted[0];
    
    # Don't trigger if score too low
    return 0 if $best->{score} < 0.3;
    
    # Check if pattern is complete (ends with terminal)
    if ($best->{pattern} =~ /\)$/) {
        # Check if we've matched enough of it
        my $patt_len = length($best->{pattern}) - 1;  # Minus terminal
        my $user_len = length($self->normalize_sequence($self->{current_seq}));
        
        if ($user_len >= $patt_len * 0.8) {  # 80% complete
            return 1 if $best->{score} > 0.6;
        }
    }
    
    # Check timeout
    my $idle_quanta = $self->{quantum_clock} - $self->{last_input};
    if ($idle_quanta > $self->{max_wait}) {
        return 1 if $best->{score} > 0.5;
    }
    
    # If only one candidate left with good score
    if (@{$self->{candidates}} == 1 && $best->{score} > 0.7) {
        return 1;
    }
    
    # If clear winner emerged
    if (@sorted > 1) {
        my $second = $sorted[1];
        if ($best->{score} > $second->{score} * 1.5) {  # 50% better
            return 1 if $best->{score} > 0.6;
        }
    }
    
    return 0;
}

sub execute_best_match {
    my $self = shift;
    
    return unless @{$self->{candidates}};
    
    my @sorted = sort { $b->{score} <=> $a->{score} } @{$self->{candidates}};
    my $best = $sorted[0];
    
    print "TRIGGERED: $best->{name} (score: $best->{score})\n" if $self->{verbose};
    
    # Execute callback
    $self->{callback}->($best->{name}, $best);
    
    # Reset for next gesture
    $self->reset();
}

sub periodic_update {
    my $self = shift;
    
    # Call this periodically to update timeouts
    $self->tick() if !$self->{button_down};
}

1;