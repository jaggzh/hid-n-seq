package HIDUI::Actions::Keyboard;

use strict;
use warnings;

# Register all keyboard actions with the registry
sub register_all {
    my ($class, $registry) = @_;
    
    # Basic keys
    $registry->register(
        id => 'key_enter',
        label => 'Key: Enter',
        group => 'keyboard_keys',
        handler => sub { _key_press('Return') }
    );
    
    $registry->register(
        id => 'key_space',
        label => 'Key: Space',
        group => 'keyboard_keys',
        handler => sub { _key_press('space') }
    );
    
    $registry->register(
        id => 'key_escape',
        label => 'Key: Escape',
        group => 'keyboard_keys',
        handler => sub { _key_press('Escape') }
    );
    
    $registry->register(
        id => 'key_tab',
        label => 'Key: Tab',
        group => 'keyboard_keys',
        handler => sub { _key_press('Tab') }
    );
    
    $registry->register(
        id => 'key_backspace',
        label => 'Key: Backspace',
        group => 'keyboard_keys',
        handler => sub { _key_press('BackSpace') }
    );
    
    $registry->register(
        id => 'key_delete',
        label => 'Key: Delete',
        group => 'keyboard_keys',
        handler => sub { _key_press('Delete') }
    );
    
    # Arrow keys
    $registry->register(
        id => 'key_up',
        label => 'Key: Up Arrow',
        group => 'keyboard_arrows',
        handler => sub { _key_press('Up') }
    );
    
    $registry->register(
        id => 'key_down',
        label => 'Key: Down Arrow',
        group => 'keyboard_arrows',
        handler => sub { _key_press('Down') }
    );
    
    $registry->register(
        id => 'key_left',
        label => 'Key: Left Arrow',
        group => 'keyboard_arrows',
        handler => sub { _key_press('Left') }
    );
    
    $registry->register(
        id => 'key_right',
        label => 'Key: Right Arrow',
        group => 'keyboard_arrows',
        handler => sub { _key_press('Right') }
    );
    
    # Special repeating action for scrolling
    $registry->register(
        id => 'key_down_repeat',
        label => 'Key: Down Arrow (10x)',
        group => 'keyboard_special',
        handler => sub {
            my ($core, $event_name, $action_id, $params) = @_;
            my $repeat = $params->{repeat} // 10;
            system('xdotool', 'key', '--repeat', $repeat, 'Down');
        }
    );
    
    $registry->register(
        id => 'key_up_repeat',
        label => 'Key: Up Arrow (10x)',
        group => 'keyboard_special',
        handler => sub {
            my ($core, $event_name, $action_id, $params) = @_;
            my $repeat = $params->{repeat} // 10;
            system('xdotool', 'key', '--repeat', $repeat, 'Up');
        }
    );
}

# Internal helper to press a key
sub _key_press {
    my ($key_name) = @_;
    return sub {
        system('xdotool', 'key', $key_name);
    };
}

1;

__END__

=head1 NAME

HIDUI::Actions::Keyboard - Keyboard simulation actions

=head1 DESCRIPTION

Provides keyboard key press actions using xdotool.

=cut
# vim: et ts=4 sts=4 sw=4
