package HIDUI::Actions::UI;

use strict;
use warnings;

# Register all UI control actions with the registry
sub register_all {
    my ($class, $registry) = @_;
    
    # Main UI window actions
    $registry->register(
        id => 'open_main_ui',
        label => 'UI: Open Main Window',
        group => 'ui_control',
        handler => \&open_main_ui
    );
    
    $registry->register(
        id => 'close_current_window',
        label => 'UI: Close Current Window',
        group => 'ui_control',
        handler => \&close_current_window
    );
    
    # Navigation actions (used within UI mode)
    $registry->register(
        id => 'ui_navigate_next',
        label => 'UI: Navigate to Next Cell',
        group => 'ui_navigation',
        handler => \&ui_navigate_next
    );
    
    $registry->register(
        id => 'ui_navigate_prev',
        label => 'UI: Navigate to Previous Cell',
        group => 'ui_navigation',
        handler => \&ui_navigate_prev
    );
    
    $registry->register(
        id => 'ui_activate_current',
        label => 'UI: Activate Current Cell',
        group => 'ui_navigation',
        handler => \&ui_activate_current
    );
    
    # Preset config window actions
    $registry->register(
        id => 'open_preset_config',
        label => 'UI: Open Preset Config',
        group => 'ui_control',
        handler => \&open_preset_config
    );
    
    $registry->register(
        id => 'config_next_event',
        label => 'Config: Next Event Slot',
        group => 'preset_config',
        handler => \&config_next_event
    );
    
    $registry->register(
        id => 'config_prev_event',
        label => 'Config: Previous Event Slot',
        group => 'preset_config',
        handler => \&config_prev_event
    );
    
    $registry->register(
        id => 'config_select_action',
        label => 'Config: Select Action for Event',
        group => 'preset_config',
        handler => \&config_select_action
    );
    
    $registry->register(
        id => 'config_close',
        label => 'Config: Close and Save',
        group => 'preset_config',
        handler => \&config_close
    );
    
    # Quick assign window (placeholder for future)
    $registry->register(
        id => 'open_quick_assign',
        label => 'UI: Open Quick Assign',
        group => 'ui_control',
        handler => \&open_quick_assign
    );
    
    # No-op action
    $registry->register(
        id => 'noop',
        label => 'No Action',
        group => 'special',
        handler => sub { }  # Does nothing
    );
}

# Action handlers

sub open_main_ui {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return if $core->ui->active;  # Already open
    
    $core->ui->show_main_window();
}

sub close_current_window {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    
    my $current_window = $core->ui->current_window;
    
    if ($current_window eq 'main') {
        $core->ui->hide_main_window();
    } elsif ($current_window eq 'preset_config') {
        $core->ui->hide_preset_config();
    }
}

sub ui_navigate_next {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    
    my $window = $core->ui->get_active_window_object();
    $window->navigate_next() if $window && $window->can('navigate_next');
}

sub ui_navigate_prev {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    
    my $window = $core->ui->get_active_window_object();
    $window->navigate_prev() if $window && $window->can('navigate_prev');
}

sub ui_activate_current {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    
    my $window = $core->ui->get_active_window_object();
    $window->activate_current() if $window && $window->can('activate_current');
}

sub open_preset_config {
    my ($core, $event_name, $action_id, $params) = @_;
    
    $core->ui->show_preset_config();
}

sub config_next_event {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    return unless $core->ui->current_window eq 'preset_config';
    
    my $window = $core->ui->get_active_window_object();
    $window->next_event() if $window && $window->can('next_event');
}

sub config_prev_event {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    return unless $core->ui->current_window eq 'preset_config';
    
    my $window = $core->ui->get_active_window_object();
    $window->prev_event() if $window && $window->can('prev_event');
}

sub config_select_action {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    return unless $core->ui->current_window eq 'preset_config';
    
    my $window = $core->ui->get_active_window_object();
    $window->select_action() if $window && $window->can('select_action');
}

sub config_close {
    my ($core, $event_name, $action_id, $params) = @_;
    
    return unless $core->ui->active;
    return unless $core->ui->current_window eq 'preset_config';
    
    $core->ui->hide_preset_config();
}

sub open_quick_assign {
    my ($core, $event_name, $action_id, $params) = @_;
    
    # Placeholder for future implementation
    warn "Quick assign not yet implemented\n";
}

1;

__END__

=head1 NAME

HIDUI::Actions::UI - UI control and navigation actions

=head1 DESCRIPTION

Provides actions for controlling HIDUI windows and navigating within them.

=cut
# vim: et ts=4 sts=4 sw=4
