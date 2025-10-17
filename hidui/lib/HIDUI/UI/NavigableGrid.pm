package HIDUI::UI::NavigableGrid;

use strict;
use warnings;
use Tk;

# Constructor
# Takes: parent (Tk widget), cells (arrayref of cell data), on_activate (callback)
sub new {
    my ($class, %args) = @_;
    
    my $parent = $args{parent} or die "NavigableGrid requires parent widget";
    my $cells = $args{cells} // [];
    my $on_activate = $args{on_activate};
    my $initial_index = $args{initial_index} // 0;
    
    my $self = {
        parent => $parent,
        cells => $cells,           # Array of cell data hashes
        buttons => [],             # Array of Tk button widgets
        current_index => $initial_index,
        on_activate => $on_activate,
        frame => undef,            # Container frame
    };
    
    bless $self, $class;
    
    $self->_build_ui();
    $self->_highlight_current();
    
    return $self;
}

# Navigate to next cell
sub navigate_next {
    my ($self) = @_;
    
    return unless @{$self->{cells}};
    
    $self->{current_index}++;
    
    # Wrap around to beginning
    if ($self->{current_index} >= @{$self->{cells}}) {
        $self->{current_index} = 0;
    }
    
    $self->_highlight_current();
}

# Navigate to previous cell
sub navigate_prev {
    my ($self) = @_;
    
    return unless @{$self->{cells}};
    
    $self->{current_index}--;
    
    # Wrap around to end
    if ($self->{current_index} < 0) {
        $self->{current_index} = @{$self->{cells}} - 1;
    }
    
    $self->_highlight_current();
}

# Activate current cell
sub activate_current {
    my ($self) = @_;
    
    return unless @{$self->{cells}};
    
    my $cell = $self->{cells}[$self->{current_index}];
    
    if ($self->{on_activate}) {
        $self->{on_activate}->($cell);
    }
}

# Get current cell data
sub get_current_cell {
    my ($self) = @_;
    
    return undef unless @{$self->{cells}};
    return $self->{cells}[$self->{current_index}];
}

# Get current index
sub get_current_index {
    my ($self) = @_;
    return $self->{current_index};
}

# Set current index (useful for restoring position)
sub set_current_index {
    my ($self, $index) = @_;
    
    return unless @{$self->{cells}};
    
    $index = 0 if $index < 0;
    $index = $#{$self->{cells}} if $index >= @{$self->{cells}};
    
    $self->{current_index} = $index;
    $self->_highlight_current();
}

# Get the Tk frame widget
sub frame {
    my ($self) = @_;
    return $self->{frame};
}

# Internal: Build the UI
sub _build_ui {
    my ($self) = @_;
    
    # Create container frame
    $self->{frame} = $self->{parent}->Frame(
        -borderwidth => 2,
        -relief => 'groove'
    );
    
    # Create buttons for each cell
    my $row = 0;
    my $col = 0;
    my $max_cols = 4;  # Layout in grid, 4 columns max
    
    for my $i (0 .. $#{$self->{cells}}) {
        my $cell = $self->{cells}[$i];
        
        my $button = $self->{frame}->Button(
            -text => $cell->{label} // $cell->{id},
            -width => 15,
            -command => sub {
                # Allow mouse clicks on buttons too
                $self->{current_index} = $i;
                $self->activate_current();
            }
        );
        
        $button->grid(-row => $row, -column => $col, -padx => 2, -pady => 2);
        
        push @{$self->{buttons}}, $button;
        
        # Move to next grid position
        $col++;
        if ($col >= $max_cols) {
            $col = 0;
            $row++;
        }
    }
}

# Internal: Highlight the current cell
sub _highlight_current {
    my ($self) = @_;
    
    return unless @{$self->{buttons}};
    
    # Remove highlight from all buttons
    for my $button (@{$self->{buttons}}) {
        $button->configure(-background => 'grey90');
    }
    
    # Highlight current button
    my $current_button = $self->{buttons}[$self->{current_index}];
    $current_button->configure(-background => 'yellow');
}

1;

__END__

=head1 NAME

HIDUI::UI::NavigableGrid - Navigable grid of buttons

=head1 SYNOPSIS

    my $grid = HIDUI::UI::NavigableGrid->new(
        parent => $parent_frame,
        cells => [
            { id => 'cell1', label => 'Button 1', type => 'action', ... },
            { id => 'cell2', label => 'Button 2', type => 'preset', ... },
        ],
        on_activate => sub {
            my $cell = shift;
            # Handle activation
        },
        initial_index => 0
    );
    
    $grid->navigate_next();
    $grid->activate_current();

=head1 DESCRIPTION

Provides a navigable grid of buttons that can be controlled programmatically
via navigate_next/prev and activate_current methods.

=cut
# vim: et ts=4 sts=4 sw=4
