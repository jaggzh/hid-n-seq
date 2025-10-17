package HIDUI::UI::PresetConfig;

use strict;
use warnings;
use Tk;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "PresetConfig requires core object";
    
    my $self = {
        core => $core,
        toplevel => undef,
        current_preset_id => undef,
        event_list => [],           # List of event names
        current_event_index => 0,
        event_labels => {},         # event_name => label widget
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
}

# Hide the window
sub hide {
    my ($self) = @_;
    $self->{toplevel}->withdraw();
}

# Navigate to next event slot
sub next_event {
    my ($self) = @_;
    
    return unless @{$self->{event_list}};
    
    $self->{current_event_index}++;
    
    if ($self->{current_event_index} >= @{$self->{event_list}}) {
        $self->{current_event_index} = 0;
    }
    
    $self->_highlight_current_event();
}

# Navigate to previous event slot
sub prev_event {
    my ($self) = @_;
    
    return unless @{$self->{event_list}};
    
    $self->{current_event_index}--;
    
    if ($self->{current_event_index} < 0) {
        $self->{current_event_index} = $#{$self->{event_list}};
    }
    
    $self->_highlight_current_event();
}

# Select action for current event (placeholder for now)
sub select_action {
    my ($self) = @_;
    
    # For MVP, just show a message
    # Later: open action selector dialog
    my $event_name = $self->{event_list}[$self->{current_event_index}];
    
    $self->{toplevel}->messageBox(
        -title => 'Select Action',
        -message => "Action selection UI not yet implemented.\nEvent: $event_name",
        -type => 'OK'
    );
}

# Internal: Build the window
sub _build_window {
    my ($self) = @_;
    
    # Create toplevel window
    $self->{toplevel} = MainWindow->new();
    $self->{toplevel}->title('HIDUI - Preset Configuration');
    $self->{toplevel}->geometry('500x400');
    
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
    
    # Event mappings frame
    $self->{events_frame} = $main_frame->Frame(
        -borderwidth => 2,
        -relief => 'sunken'
    )->pack(-fill => 'both', -expand => 1, -pady => 10);
    
    # Buttons
    my $button_frame = $main_frame->Frame()->pack(-fill => 'x');
    
    $button_frame->Button(
        -text => 'Save',
        -command => sub { $self->_save_and_close() }
    )->pack(-side => 'left', -padx => 5);
    
    $button_frame->Button(
        -text => 'Cancel',
        -command => sub { $self->hide() }
    )->pack(-side => 'left', -padx => 5);
}

# Internal: Load current preset and display events
sub _load_preset {
    my ($self) = @_;
    
    my $preset = $self->{core}->config->get('presets', $self->{current_preset_id});
    
    return unless $preset;
    
    # Update preset label
    $self->{preset_label}->configure(-text => $preset->{label});
    
    # Clear existing event widgets
    for my $child ($self->{events_frame}->children) {
        $child->destroy();
    }
    
    %{$self->{event_labels}} = ();
    @{$self->{event_list}} = ();
    
    # Get events for this preset
    my $events = $preset->{events} // {};
    
    # Display each event mapping
    my $row = 0;
    for my $event_name (sort keys %$events) {
        push @{$self->{event_list}}, $event_name;
        
        my $action_id = $events->{$event_name};
        
        my $event_frame = $self->{events_frame}->Frame()->pack(
            -fill => 'x',
            -pady => 2
        );
        
        my $label = $event_frame->Label(
            -text => "$event_name -> $action_id",
            -width => 50,
            -anchor => 'w'
        )->pack(-side => 'left');
        
        $self->{event_labels}{$event_name} = $label;
        
        $row++;
    }
    
    # Highlight first event
    $self->{current_event_index} = 0;
    $self->_highlight_current_event();
}

# Internal: Highlight current event
sub _highlight_current_event {
    my ($self) = @_;
    
    return unless @{$self->{event_list}};
    
    # Remove highlight from all
    for my $label (values %{$self->{event_labels}}) {
        $label->configure(-background => 'SystemButtonFace');
    }
    
    # Highlight current
    my $event_name = $self->{event_list}[$self->{current_event_index}];
    my $label = $self->{event_labels}{$event_name};
    $label->configure(-background => 'yellow') if $label;
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