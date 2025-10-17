package HIDUI::Core;

use strict;
use warnings;

use HIDUI::Config;
use HIDUI::ActionRegistry;
use HIDUI::EventMapper;
use HIDUI::Actions::Mouse;
use HIDUI::Actions::Keyboard;
use HIDUI::Actions::UI;
use HIDUI::Actions::Presets;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $config_dir = $args{config_dir} // 'config';
    
    my $self = {
        config => undef,
        registry => undef,
        mapper => undef,
        ui => undef,  # Will be set later when UI is created
    };
    
    bless $self, $class;
    
    # Initialize components
    $self->{config} = HIDUI::Config->new(config_dir => $config_dir);
    $self->{registry} = HIDUI::ActionRegistry->new();
    $self->{mapper} = HIDUI::EventMapper->new(config => $self->{config});
    
    # Register all built-in actions
    $self->_register_builtin_actions();
    
    return $self;
}

# Main event handler - called from gesture callback
sub event {
    my ($self, $event_name) = @_;
    
    unless ($event_name) {
        warn "event() called without event name\n";
        return;
    }
    
    # Determine current mode
    my $mode = $self->_determine_mode();
    
    # Resolve event to action
    my $action_id = $self->{mapper}->resolve($event_name, $mode);
    
    unless ($action_id) {
        warn "No action mapped for event '$event_name' in mode '$mode'\n";
        return;
    }
    
    # Execute action
    my $params = {};  # Could be expanded later
    $self->{registry}->execute($action_id, $self, $event_name, $params);
}

# Register a custom action (for user scripts)
sub register_action {
    my ($self, %args) = @_;
    $self->{registry}->register(%args);
}

# Get action registry (for UI to list available actions)
sub registry {
    my ($self) = @_;
    return $self->{registry};
}

# Get config object
sub config {
    my ($self) = @_;
    return $self->{config};
}

# Get event mapper
sub mapper {
    my ($self) = @_;
    return $self->{mapper};
}

# Get/Set UI object
sub ui {
    my ($self, $ui_obj) = @_;
    $self->{ui} = $ui_obj if defined $ui_obj;
    return $self->{ui};
}

# Run the application (starts Tk MainLoop)
sub run {
    my ($self) = @_;
    
    # UI must be initialized before calling run
    unless ($self->{ui}) {
        die "UI not initialized. Create UI components before calling run()\n";
    }
    
    # Start Tk event loop
    require Tk;
    Tk::MainLoop();
}

# Internal: Register all built-in actions
sub _register_builtin_actions {
    my ($self) = @_;
    
    HIDUI::Actions::Mouse->register_all($self->{registry});
    HIDUI::Actions::Keyboard->register_all($self->{registry});
    HIDUI::Actions::UI->register_all($self->{registry});
    HIDUI::Actions::Presets->register_all($self->{registry});
}

# Internal: Determine current mode based on UI state
sub _determine_mode {
    my ($self) = @_;
    
    # If no UI or UI not active, we're in desktop mode
    return 'desktop_mode' unless $self->{ui} && $self->{ui}->active;
    
    # Check which window is active
    my $current_window = $self->{ui}->current_window;
    
    return 'main_ui' if $current_window eq 'main';
    return 'preset_config' if $current_window eq 'preset_config';
    
    # Default to desktop mode
    return 'desktop_mode';
}

1;

__END__

=head1 NAME

HIDUI::Core - Main coordinator for HIDUI

=head1 SYNOPSIS

    use HIDUI::Core;
    
    my $hidui = HIDUI::Core->new(config_dir => 'config');
    
    # Register custom actions
    $hidui->register_action(
        id => 'my_action',
        label => 'My Custom Action',
        group => 'custom',
        handler => sub {
            my ($core, $event, $action, $params) = @_;
            # Do something
        }
    );
    
    # Wire up gesture callback
    $gesture->on_event(sub {
        my $event_name = shift;
        $hidui->event($event_name);
    });
    
    # Initialize UI (done separately)
    # my $ui_manager = HIDUI::UI::Manager->new(core => $hidui);
    # $hidui->ui($ui_manager);
    
    # Start event loop
    $hidui->run();

=head1 DESCRIPTION

Core coordinator that ties together config, actions, event mapping,
and UI. Provides the main event() method that gesture callbacks invoke.

=cut