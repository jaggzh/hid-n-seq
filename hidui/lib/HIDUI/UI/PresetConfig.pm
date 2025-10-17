package HIDUI::UI::PresetConfig;

use strict;
use warnings;
use Tk;
use HIDUI::UI::NavigableGrid;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "PresetConfig requires core object";
    
    my $self = {
        core => $core,
        toplevel => undef,
        current_preset_id => undef,
        event_grid => undef,        # Grid of event slots
        button_grid => undef,       # Grid of Save/Cancel buttons
        current_section => 'events', # 'events' or 'buttons'
    };
    
    bless $self, $class;
    
    $self->_build_window();
    
    return $self;
}

# Show the window
sub show {
    my ($self) = @_;
    
    # Load active preset
    $self->{current_preset_id} = $self->{core}->config->get('active_preset');
    
    $self->_load_preset();
    $self->{toplevel}->deiconify();
    $self->{toplevel}->raise();
    
    # Start in events section
    $self->{current_section} = 'events';
}

# Hide the window
sub hide {
    my ($self) = @_;
    $self->{toplevel}->withdraw();
}

# Navigate to next event slot
sub next_event {
    my ($self) = @_;
    
    if ($self->{current_section} eq 'events') {
        $self->{event_grid}->navigate_next() if $self->{event_grid};
    } elsif ($self->{current_section} eq 'buttons') {
        $self->{button_grid}->navigate_next() if $self->{button_grid};
    }
}

# Navigate to previous event slot
sub prev_event {
    my ($self) = @_;
    
    if ($self->{current_section} eq 'events') {
        $self->{event_grid}->navigate_prev() if $self->{event_grid};
    } elsif ($self->{current_section} eq 'buttons') {
        $self->{button_grid}->navigate_prev() if $self->{button_grid};
    }
}

# Switch between sections (events <-> buttons)
sub switch_section {
    my ($self) = @_;
    
    if ($self->{current_section} eq 'events') {
        $self->{current_section} = 'buttons';
    } else {
        $self->{current_section} = 'events';
    }
}

# Select action for current event (placeholder for now)
sub select_action {
    my ($self) = @_;
    
    if ($self->{current_section} eq 'buttons') {
        # Activate current button
        $self->{button_grid}->activate_current() if $self->{button_grid};
    } else {
        # For MVP, just show a message
        my $cell = $self->{event_grid}->get_current_cell() if $self->{event_grid};
        my $event_name = $cell->{event_name} if $cell;
        
        $self->{toplevel}->messageBox(
            -title => 'Select Action',
            -message => "Action selection UI not yet implemented.\nEvent: $event_name",
            -type => 'OK'
        );
    }
}

# Internal: Build the window
sub _build_window {
    my ($self) = @_;
    
    # Create toplevel window
    $self->{toplevel} = MainWindow->new();
    $self->{toplevel}->title('HIDUI - Preset Configuration');
    $self->{toplevel}->geometry('600x500');
    
    # Hide initially
    $self->{toplevel}->withdraw();
    
    # Handle close
    $self->{toplevel}->protocol('WM_DELETE_WINDOW', sub {
        $self->hide();
    });
    
    # Create main frame
    my $main_frame = $self->{toplevel}->Frame()->pack(
        -fill => 'both',
        -expand => 1,
        -padx => 10,
        -pady => 10
    );
    
    # Title
    $main_frame->Label(
        -text => 'Configure Preset Events',
        -font => ['helvetica', 14, 'bold']
    )->pack(-pady => 5);
    
    # Preset selector (simplified for MVP)
    my $preset_frame = $main_frame->Frame()->pack(-fill => 'x', -pady => 5);
    $preset_frame->Label(-text => 'Editing Preset: ')->pack(-side => 'left');
    
    $self->{preset_label} = $preset_frame->Label(
        -text => '',
        -font => ['helvetica', 10, 'bold']
    )->pack(-side => 'left');
    
    # Event mappings frame (will hold the grid)
    $self->{events_container} = $main_frame->Frame(
        -borderwidth => 2,
        -relief => 'sunken'
    )->pack(-fill => 'both', -expand => 1, -pady => 10);
    
    # Buttons frame (will hold Save/Cancel grid)
    $self->{buttons_container} = $main_frame->Frame()->pack(-fill => 'x');
}

# Internal: Load current preset and display events
sub _load_preset {
    my ($self) = @_;
    
    my $preset = $self->{core}->config->get('presets', $self->{current_preset_id});
    
    return unless $preset;
    
    # Update preset label
    $self->{preset_label}->configure(-text => $preset->{label});
    
    # Clear existing grids
    if ($self->{event_grid}) {
        $self->{event_grid}->frame->destroy();
        $self->{event_grid} = undef;
    }
    if ($self->{button_grid}) {
        $self->{button_grid}->frame->destroy();
        $self->{button_grid} = undef;
    }
    
    # Build event cells
    my $events = $preset->{events} // {};
    my @event_cells;
    
    for my $event_name (sort keys %$events) {
        my $action_id = $events->{$event_name};
        push @event_cells, {
            id => $event_name,
            label => "$event_name → $action_id",
            type => 'event_slot',
            event_name => $event_name,
        };
    }
    
    # Create event grid
    $self->{event_grid} = HIDUI::UI::NavigableGrid->new(
        parent => $self->{events_container},
        cells => \@event_cells,
        on_activate => sub {
            my $cell = shift;
            # Placeholder - will be action selector later
        }
    );
    $self->{event_grid}->frame->pack(-fill => 'both', -expand => 1);
    
    # Build button cells
    my @button_cells = (
        { id => 'save', label => 'Save', type => 'button' },
        { id => 'cancel', label => 'Cancel', type => 'button' },
    );
    
    # Create button grid
    $self->{button_grid} = HIDUI::UI::NavigableGrid->new(
        parent => $self->{buttons_container},
        cells => \@button_cells,
        on_activate => sub {
            my $cell = shift;
            if ($cell->{id} eq 'save') {
                $self->_save_and_close();
            } elsif ($cell->{id} eq 'cancel') {
                $self->hide();
            }
        }
    );
    $self->{button_grid}->frame->pack(-fill => 'x');
}

# Internal: Save and close
sub _save_and_close {
    my ($self) = @_;
    
    $self->{core}->config->save();
    $self->hide();
}

1;

__END__

=head1 NAME

HIDUI::UI::PresetConfig - Preset configuration window

=head1 DESCRIPTION

Window for configuring event-to-action mappings for presets.
MVP version allows navigation through events; action selection to be enhanced.

=cut

# vim: et ts=4 sts=4 sw=4
