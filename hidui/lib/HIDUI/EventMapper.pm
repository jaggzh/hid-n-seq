package HIDUI::EventMapper;

use strict;
use warnings;

# Constructor
# Takes config object reference
sub new {
    my ($class, %args) = @_;
    
    my $config = $args{config} or die "EventMapper requires config object";
    
    my $self = {
        config => $config,
        current_mode => 'desktop_mode',  # Start in desktop mode
    };
    
    return bless $self, $class;
}

# Resolve event name to action id
# Takes: event name (e.g., "click"), current mode (optional)
# Returns: action id (e.g., "mouse_left_click") or undef
sub resolve {
    my ($self, $event_name, $mode) = @_;
    
    $mode //= $self->{current_mode};
    
    # Mode-specific mappings override everything when not in desktop mode
    if ($mode ne 'desktop_mode') {
        my $mode_action = $self->_get_mode_mapping($event_name, $mode);
        return $mode_action if defined $mode_action;
    }
    
    # Desktop mode uses override stack
    
    # 1. Check sticky quick assignments
    my $sticky = $self->_get_quick_assignment('sticky', $event_name);
    return $sticky if defined $sticky;
    
    # 2. Check transient quick assignments
    my $transient = $self->_get_quick_assignment('transient', $event_name);
    return $transient if defined $transient;
    
    # 3. Check active preset
    my $preset_action = $self->_get_preset_action($event_name);
    return $preset_action if defined $preset_action;
    
    # 4. No mapping found
    return undef;
}

# Set current mode (desktop_mode, main_ui, preset_config, etc.)
sub set_mode {
    my ($self, $mode) = @_;
    $self->{current_mode} = $mode;
}

# Get current mode
sub get_mode {
    my ($self) = @_;
    return $self->{current_mode};
}

# Add quick assignment (transient or sticky)
sub add_quick_assignment {
    my ($self, $type, $event_name, $action_id) = @_;
    
    die "Quick assignment type must be 'transient' or 'sticky'"
        unless $type eq 'transient' || $type eq 'sticky';
    
    my $quick = $self->{config}->get('quick_assignments', $type) // {};
    $quick->{$event_name} = $action_id;
    
    $self->{config}->set(['quick_assignments', $type], $quick);
}

# Remove quick assignment
sub remove_quick_assignment {
    my ($self, $type, $event_name) = @_;
    
    my $quick = $self->{config}->get('quick_assignments', $type) // {};
    delete $quick->{$event_name};
    
    $self->{config}->set(['quick_assignments', $type], $quick);
}

# Clear all transient quick assignments
sub clear_transient {
    my ($self) = @_;
    $self->{config}->set(['quick_assignments', 'transient'], {});
}

# Get all quick assignments of a type
sub get_quick_assignments {
    my ($self, $type) = @_;
    return $self->{config}->get('quick_assignments', $type) // {};
}

# Internal: Get mode-specific mapping
sub _get_mode_mapping {
    my ($self, $event_name, $mode) = @_;
    return $self->{config}->get('event_mappings', $mode, $event_name);
}

# Internal: Get quick assignment
sub _get_quick_assignment {
    my ($self, $type, $event_name) = @_;
    my $quick = $self->{config}->get('quick_assignments', $type) // {};
    return $quick->{$event_name};
}

# Internal: Get action from active preset
sub _get_preset_action {
    my ($self, $event_name) = @_;
    
    my $active_preset = $self->{config}->get('active_preset');
    return undef unless defined $active_preset;
    
    return $self->{config}->get('presets', $active_preset, 'events', $event_name);
}

1;

__END__

=head1 NAME

HIDUI::EventMapper - Maps events to actions based on mode and overrides

=head1 SYNOPSIS

    use HIDUI::EventMapper;
    
    my $mapper = HIDUI::EventMapper->new(config => $config);
    
    # Resolve event to action
    my $action_id = $mapper->resolve('click');
    
    # Change mode
    $mapper->set_mode('main_ui');
    
    # Add quick assignments
    $mapper->add_quick_assignment('sticky', 'click', 'some_action');
    
    # Clear transient assignments (e.g., when preset changes)
    $mapper->clear_transient();

=head1 DESCRIPTION

Resolves event names to action IDs using priority:
  1. Mode-specific mappings (when not in desktop_mode)
  2. Sticky quick assignments
  3. Transient quick assignments
  4. Active preset mappings

=cut
# vim: et ts=4 sts=4 sw=4
