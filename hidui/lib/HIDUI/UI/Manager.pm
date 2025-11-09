package HIDUI::UI::Manager;

use strict;
use warnings;
use Tk;
use HIDUI::UI::MainWindow;
use HIDUI::UI::PresetConfig;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "UI::Manager requires core object";
    
    my $self = {
        core => $core,
        main_window => undef,      # Tk toplevel for main UI
        preset_config => undef,    # Tk toplevel for preset config
        current_window => undef,   # 'main' or 'preset_config'
    };
    
    bless $self, $class;
    
    # Register self with core
    $core->ui($self);
    
    return $self;
}

# Check if any UI window is active
sub active {
    my ($self) = @_;
    return defined $self->{current_window};
}

# Get name of current active window
sub current_window {
    my ($self) = @_;
    return $self->{current_window};
}

# Get the actual window object for the active window
sub get_active_window_object {
    my ($self) = @_;
    
    return undef unless $self->{current_window};
    
    if ($self->{current_window} eq 'main') {
        return $self->{main_window};
    } elsif ($self->{current_window} eq 'preset_config') {
        return $self->{preset_config};
    }
    
    return undef;
}

# Show main UI window
sub show_main_window {
    my ($self) = @_;
    
    # Create window if it doesn't exist
    unless ($self->{main_window}) {
        $self->{main_window} = HIDUI::UI::MainWindow->new(core => $self->{core});
    }
    
    # Show window
    $self->{main_window}->show();
    $self->{current_window} = 'main';
    
    # Update mode in mapper
    $self->{core}->mapper->set_mode('main_ui');
}

# Hide main UI window
sub hide_main_window {
    my ($self) = @_;
    
    return unless $self->{main_window};
    
    $self->{main_window}->hide();
    $self->{current_window} = undef;
    
    # Return to desktop mode
    $self->{core}->mapper->set_mode('desktop_mode');
}

# Show preset config window
sub show_preset_config {
    my ($self) = @_;
    
    # Create window if it doesn't exist
    unless ($self->{preset_config}) {
        $self->{preset_config} = HIDUI::UI::PresetConfig->new(core => $self->{core});
    }
    
    # Show window
    $self->{preset_config}->show();
    $self->{current_window} = 'preset_config';
    
    # Update mode in mapper
    $self->{core}->mapper->set_mode('preset_config');
}

# Hide preset config window
sub hide_preset_config {
    my ($self) = @_;
    
    return unless $self->{preset_config};
    
    $self->{preset_config}->hide();
    $self->{current_window} = undef;
    
    # Return to desktop mode
    $self->{core}->mapper->set_mode('desktop_mode');
}

# Update info panels (called when preset changes)
sub update_info_panels {
    my ($self) = @_;
    
    if ($self->{main_window}) {
        $self->{main_window}->update_info_panels();
    }
}

1;

__END__

=head1 NAME

HIDUI::UI::Manager - Manages UI windows and state

=head1 DESCRIPTION

Coordinates between different UI windows (main, preset config, etc.)
and manages which window is currently active.

=cut
# vim: et ts=4 sts=4 sw=4
