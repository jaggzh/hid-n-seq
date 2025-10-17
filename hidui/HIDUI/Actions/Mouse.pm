package HIDUI::Actions::Mouse;

use strict;
use warnings;

# Register all mouse actions with the registry
sub register_all {
    my ($registry) = @_;
    
    $registry->register(
        id => 'mouse_left_click',
        label => 'Mouse: Left Click',
        group => 'mouse_actions',
        handler => \&mouse_left_click
    );
    
    $registry->register(
        id => 'mouse_right_click',
        label => 'Mouse: Right Click',
        group => 'mouse_actions',
        handler => \&mouse_right_click
    );
    
    $registry->register(
        id => 'mouse_middle_click',
        label => 'Mouse: Middle Click',
        group => 'mouse_actions',
        handler => \&mouse_middle_click
    );
    
    $registry->register(
        id => 'mouse_double_click',
        label => 'Mouse: Double Click',
        group => 'mouse_actions',
        handler => \&mouse_double_click
    );
    
    $registry->register(
        id => 'mouse_scroll_up',
        label => 'Mouse: Scroll Up',
        group => 'mouse_actions',
        handler => \&mouse_scroll_up
    );
    
    $registry->register(
        id => 'mouse_scroll_down',
        label => 'Mouse: Scroll Down',
        group => 'mouse_actions',
        handler => \&mouse_scroll_down
    );
}

# Action handlers

sub mouse_left_click {
    my ($core, $event_name, $action_id, $params) = @_;
    system('xdotool', 'click', '1');
}

sub mouse_right_click {
    my ($core, $event_name, $action_id, $params) = @_;
    system('xdotool', 'click', '3');
}

sub mouse_middle_click {
    my ($core, $event_name, $action_id, $params) = @_;
    system('xdotool', 'click', '2');
}

sub mouse_double_click {
    my ($core, $event_name, $action_id, $params) = @_;
    system('xdotool', 'click', '--repeat', '2', '1');
}

sub mouse_scroll_up {
    my ($core, $event_name, $action_id, $params) = @_;
    my $amount = $params->{amount} // 3;
    system('xdotool', 'click', '--repeat', $amount, '4');
}

sub mouse_scroll_down {
    my ($core, $event_name, $action_id, $params) = @_;
    my $amount = $params->{amount} // 3;
    system('xdotool', 'click', '--repeat', $amount, '5');
}

1;

__END__

=head1 NAME

HIDUI::Actions::Mouse - Mouse simulation actions

=head1 DESCRIPTION

Provides mouse click and scroll actions using xdotool.

=cut