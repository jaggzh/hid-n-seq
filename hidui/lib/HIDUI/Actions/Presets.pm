package HIDUI::Actions::Presets;

use strict;
use warnings;

# Register preset management actions
sub register_all {
    my ($class, $registry) = @_;
    
    $registry->register(
        id => 'preset_switch',
        label => 'Switch to Preset',
        group => 'preset_management',
        handler => \&preset_switch
    );
}

# Switch to a specific preset
# Expects params->{preset_id} to be set
sub preset_switch {
    my ($core, $event_name, $action_id, $params) = @_;
    
    my $preset_id = $params->{preset_id};
    
    unless ($preset_id) {
        warn "preset_switch requires preset_id parameter\n";
        return;
    }
    
    # Verify preset exists
    my $preset = $core->config->get('presets', $preset_id);
    unless ($preset) {
        warn "Unknown preset: $preset_id\n";
        return;
    }
    
    # Clear transient quick assignments when switching presets
    $core->mapper->clear_transient();
    
    # Set new active preset
    $core->config->set(['active_preset'], $preset_id);
    
    # Save config to persist the change
    $core->config->save();
    
    # Update UI if it's showing
    if ($core->ui->active) {
        $core->ui->update_info_panels();
    }
}

1;

__END__

=head1 NAME

HIDUI::Actions::Presets - Preset management actions

=head1 DESCRIPTION

Provides actions for switching between presets.

=cut
# vim: et ts=4 sts=4 sw=4
