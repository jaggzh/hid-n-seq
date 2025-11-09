package HIDUI::ActionRegistry;

use strict;
use warnings;
use Carp qw(croak);

# Constructor
sub new {
    my ($class) = @_;
    
    my $self = {
        actions => {},  # id => { id, label, group, handler }
        groups => {},   # group_name => [ action_id1, action_id2, ... ]
    };
    
    return bless $self, $class;
}

# Register an action
# Parameters:
#   id => unique identifier for this action
#   label => human-readable name
#   group => category/group name for organizing actions
#   handler => coderef that executes the action
sub register {
    my ($self, %args) = @_;
    
    my $id = $args{id} // croak "Action registration requires 'id'";
    my $label = $args{label} // $id;  # Default to id if no label
    my $group = $args{group} // 'ungrouped';
    my $handler = $args{handler} // croak "Action registration requires 'handler'";
    
    croak "Handler must be a code reference" unless ref $handler eq 'CODE';
    
    # Store action
    $self->{actions}{$id} = {
        id => $id,
        label => $label,
        group => $group,
        handler => $handler,
    };
    
    # Add to group index
    push @{$self->{groups}{$group}}, $id;
    
    return 1;
}

# Execute an action by id
# Passes core object and optional params to handler
sub execute {
    my ($self, $action_id, $core, $event_name, $params) = @_;
    
    my $action = $self->{actions}{$action_id};
    
    unless ($action) {
        warn "Unknown action: $action_id";
        return 0;
    }
    
    $params //= {};
    
    # Call the handler with standard parameters
    eval {
        $action->{handler}->($core, $event_name, $action_id, $params);
    };
    
    if ($@) {
        warn "Error executing action '$action_id': $@";
        return 0;
    }
    
    return 1;
}

# Get action metadata by id
sub get_action {
    my ($self, $action_id) = @_;
    return $self->{actions}{$action_id};
}

# Get all action ids
sub get_all_ids {
    my ($self) = @_;
    return keys %{$self->{actions}};
}

# Get all actions in a specific group
sub get_group {
    my ($self, $group_name) = @_;
    
    my $action_ids = $self->{groups}{$group_name} // [];
    return @$action_ids;
}

# Get all group names
sub get_all_groups {
    my ($self) = @_;
    return keys %{$self->{groups}};
}

# Get all actions with their metadata
# Returns hashref: { action_id => { id, label, group } }
sub get_all_actions {
    my ($self) = @_;
    
    my %actions_info;
    
    for my $id (keys %{$self->{actions}}) {
        my $action = $self->{actions}{$id};
        $actions_info{$id} = {
            id => $action->{id},
            label => $action->{label},
            group => $action->{group},
            # Note: we don't include handler in the metadata
        };
    }
    
    return \%actions_info;
}

# Check if an action exists
sub has_action {
    my ($self, $action_id) = @_;
    return exists $self->{actions}{$action_id};
}

1;

__END__

=head1 NAME

HIDUI::ActionRegistry - Registry for action handlers

=head1 SYNOPSIS

    use HIDUI::ActionRegistry;
    
    my $registry = HIDUI::ActionRegistry->new();
    
    # Register actions
    $registry->register(
        id => 'mouse_left_click',
        label => 'Mouse: Left Click',
        group => 'mouse_actions',
        handler => sub {
            my ($core, $event, $action, $params) = @_;
            system('xdotool', 'click', '1');
        }
    );
    
    # Execute action
    $registry->execute('mouse_left_click', $core_obj, 'click', {});
    
    # Query actions
    my @mouse_actions = $registry->get_group('mouse_actions');
    my $action_info = $registry->get_action('mouse_left_click');

=head1 DESCRIPTION

Manages registration and execution of action handlers. Actions can be
grouped for easier navigation in UI.

=cut
# vim: et ts=4 sts=4 sw=4
