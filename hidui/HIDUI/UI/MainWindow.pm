package HIDUI::UI::MainWindow;

use strict;
use warnings;
use Tk;
use HIDUI::UI::NavigableGrid;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "MainWindow requires core object";
    
    my $self = {
        core => $core,
        toplevel => undef,
        grid => undef,
        info_frame => undef,
        highlighted_label => undef,
        active_label => undef,
        quick_label => undef,
    };
    
    bless $self, $class;
    
    $self->_build_window();
    
    return $self;
}

# Show the window
sub show {
    my ($self) = @_;
    $self->{toplevel}->deiconify();
    $self->{toplevel}->raise();
    $self->update_info_panels();
}

# Hide the window
sub hide {
    my ($self) = @_;
    
    # Save current position before hiding
    $self->_save_position();
    
    $self->{toplevel}->withdraw();
}

# Navigation methods (called by UI actions)
sub navigate_next {
    my ($self) = @_;
    $self->{grid}->navigate_next();
    $self->_update_highlighted_info();
}

sub navigate_prev {
    my ($self) = @_;
    $self->{grid}->navigate_prev();
    $self->_update_highlighted_info();
}

sub activate_current {
    my ($self) = @_;
    $self->{grid}->activate_current();
}

# Internal: Build the window
sub _build_window {
    my ($self) = @_;
    
    # Create toplevel window
    $self->{toplevel} = MainWindow->new();
    $self->{toplevel}->title('HIDUI - Main');
    $self->{toplevel}->geometry('600x500');
    
    # Hide initially
    $self->{toplevel}->withdraw();
    
    # Prevent window from being closed via X button (use our close action instead)
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
    
    # Title label
    $main_frame->Label(
        -text => 'HIDUI - Single Button Interface',
        -font => ['helvetica', 14, 'bold']
    )->pack(-pady => 5);
    
    # Build cell list from config
    my $cells = $self->_build_cell_list();
    
    # Get initial position from config
    my $initial_index = $self->_get_saved_position();
    
    # Create navigable grid
    $self->{grid} = HIDUI::UI::NavigableGrid->new(
        parent => $main_frame,
        cells => $cells,
        initial_index => $initial_index,
        on_activate => sub {
            my $cell = shift;
            $self->_handle_cell_activation($cell);
        }
    );
    
    $self->{grid}->frame->pack(-pady => 10);
    
    # Create info panel
    $self->_build_info_panel($main_frame);
    
    # Initialize info displays
    $self->_update_highlighted_info();
}

# Internal: Build flat list of cells from config
sub _build_cell_list {
    my ($self) = @_;
    
    my @cells;
    
    my $layout = $self->{core}->config->get('ui_layout', 'main_window');
    my $sections = $layout->{sections} // [];
    
    for my $section (@$sections) {
        my $rows = $section->{rows} // [];
        
        for my $row (@$rows) {
            my $row_cells = $row->{cells} // [];
            push @cells, @$row_cells;
        }
    }
    
    return \@cells;
}

# Internal: Get saved position from config
sub _get_saved_position {
    my ($self) = @_;
    
    my $remember = $self->{core}->config->get('ui_layout', 'main_window', 'remember_position');
    return 0 unless $remember;
    
    my $saved = $self->{core}->config->get('ui_state', 'main_window_position', 'cell_index');
    return $saved // 0;
}

# Internal: Save current position to config
sub _save_position {
    my ($self) = @_;
    
    my $remember = $self->{core}->config->get('ui_layout', 'main_window', 'remember_position');
    return unless $remember;
    
    my $index = $self->{grid}->get_current_index();
    $self->{core}->config->set(['ui_state', 'main_window_position', 'cell_index'], $index);
    $self->{core}->config->save();
}

# Internal: Build info panel
sub _build_info_panel {
    my ($self, $parent) = @_;
    
    $self->{info_frame} = $parent->Frame(
        -borderwidth => 2,
        -relief => 'sunken'
    )->pack(-fill => 'both', -expand => 1, -pady => 5);
    
    # Highlighted preset info
    $self->{info_frame}->Label(
        -text => 'Currently Highlighted:',
        -font => ['helvetica', 10, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{highlighted_label} = $self->{info_frame}->Label(
        -text => '',
        -justify => 'left'
    )->pack(-anchor => 'w', -padx => 10);
    
    $self->{info_frame}->Frame(-height => 10)->pack();  # Spacer
    
    # Active preset info
    $self->{info_frame}->Label(
        -text => 'Active Preset:',
        -font => ['helvetica', 10, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{active_label} = $self->{info_frame}->Label(
        -text => '',
        -justify => 'left'
    )->pack(-anchor => 'w', -padx => 10);
    
    $self->{info_frame}->Frame(-height => 10)->pack();  # Spacer
    
    # Quick assignments info
    $self->{info_frame}->Label(
        -text => 'Quick Assignments:',
        -font => ['helvetica', 10, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{quick_label} = $self->{info_frame}->Label(
        -text => '',
        -justify => 'left'
    )->pack(-anchor => 'w', -padx => 10);
}

# Internal: Handle cell activation
sub _handle_cell_activation {
    my ($self, $cell) = @_;
    
    my $type = $cell->{type};
    
    if ($type eq 'preset') {
        # Switch to this preset
        my $preset_id = $cell->{preset_id};
        my $params = { preset_id => $preset_id };
        $self->{core}->registry->execute('preset_switch', $self->{core}, 'activate', $params);
        
    } elsif ($type eq 'action_button') {
        # Execute the action
        my $action_id = $cell->{action_id};
        $self->{core}->registry->execute($action_id, $self->{core}, 'activate', {});
    }
}

# Update info panels
sub update_info_panels {
    my ($self) = @_;
    
    $self->_update_highlighted_info();
    $self->_update_active_info();
    $self->_update_quick_info();
}

# Internal: Update highlighted preset info
sub _update_highlighted_info {
    my ($self) = @_;
    
    my $cell = $self->{grid}->get_current_cell();
    
    if ($cell && $cell->{type} eq 'preset') {
        my $preset_id = $cell->{preset_id};
        my $preset = $self->{core}->config->get('presets', $preset_id);
        
        if ($preset) {
            my $text = $preset->{label} . "\n";
            my $events = $preset->{events} // {};
            
            for my $event (sort keys %$events) {
                my $action = $events->{$event};
                $text .= "  $event -> $action\n";
            }
            
            $self->{highlighted_label}->configure(-text => $text);
        }
    } else {
        $self->{highlighted_label}->configure(-text => '(Not on a preset)');
    }
}

# Internal: Update active preset info
sub _update_active_info {
    my ($self) = @_;
    
    my $active_id = $self->{core}->config->get('active_preset');
    my $preset = $self->{core}->config->get('presets', $active_id);
    
    if ($preset) {
        my $text = $preset->{label} . "\n";
        my $events = $preset->{events} // {};
        
        for my $event (sort keys %$events) {
            my $action = $events->{$event};
            $text .= "  $event -> $action\n";
        }
        
        $self->{active_label}->configure(-text => $text);
    }
}

# Internal: Update quick assignments info
sub _update_quick_info {
    my ($self) = @_;
    
    my $sticky = $self->{core}->mapper->get_quick_assignments('sticky');
    my $transient = $self->{core}->mapper->get_quick_assignments('transient');
    
    my $text = '';
    
    if (%$sticky) {
        $text .= "Sticky:\n";
        for my $event (sort keys %$sticky) {
            $text .= "  $event -> $$sticky{$event}\n";
        }
    }
    
    if (%$transient) {
        $text .= "Transient:\n";
        for my $event (sort keys %$transient) {
            $text .= "  $event -> $$transient{$event}\n";
        }
    }
    
    $text = '(None)' if $text eq '';
    
    $self->{quick_label}->configure(-text => $text);
}

1;

__END__

=head1 NAME

HIDUI::UI::MainWindow - Main UI window

=head1 DESCRIPTION

The main navigation UI that displays presets and quick actions.

=cut