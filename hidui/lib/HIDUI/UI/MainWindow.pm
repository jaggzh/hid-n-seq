package HIDUI::UI::MainWindow;

use strict;
use warnings;
use utf8;
use Tk;
use HIDUI::UI::NavigableGrid;

# UI Styling Configuration
my $FONT_SIZE_HEADING = 14;
my $FONT_SIZE_TEXT = 12;

my $COLOR_EVENT = '#a000b0';    # Purple - for event names like "click", "doubleclick"
my $COLOR_ACTION = '#000090';   # Blue - for action names like "mouse_left_click"

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "MainWindow requires core object";
    
    my $self = {
        core => $core,
        toplevel => undef,
        grid => undef,
        info_frame => undef,
        ui_help_text => undef,
        highlighted_text => undef,
        active_text => undef,
        quick_text => undef,
    };
    
    bless $self, $class;
    
    $self->_build_window();
    
    $self->{toplevel}->bind('<FocusIn>', sub {
        $self->{core}->mapper->set_mode('main_ui');
        $self->{core}->ui->{current_window} = 'main';
    });

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
    $self->{toplevel}->geometry('800x500');
    
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
        -font => ['helvetica', $FONT_SIZE_HEADING, 'bold']
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
    
    # ===== UI Help Section =====
    $self->{info_frame}->Label(
        -text => 'UI Controls:',
        -font => ['helvetica', $FONT_SIZE_HEADING, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{ui_help_text} = $self->{info_frame}->Text(
        -height => 2,
        -wrap => 'word',
        -relief => 'flat',
        -background => $self->{info_frame}->cget('-background'),
        -font => ['helvetica', $FONT_SIZE_TEXT]
    )->pack(-anchor => 'w', -padx => 10, -fill => 'x');
    
    # Tag: event name (e.g., "click", "doubleclick")
    $self->{ui_help_text}->tagConfigure('event',
        -foreground => $COLOR_EVENT,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold']
    );
    
    # Tag: action name (e.g., "ui_navigate_next", "mouse_left_click")
    $self->{ui_help_text}->tagConfigure('action',
        -foreground => $COLOR_ACTION,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold', 'italic']
    );
    
    $self->{info_frame}->Frame(-height => 1)->pack();  # Spacer
    
    # ===== Highlighted Preset Section =====
    $self->{info_frame}->Label(
        -text => 'Currently Highlighted:',
        -font => ['helvetica', $FONT_SIZE_HEADING, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{highlighted_text} = $self->{info_frame}->Text(
        -height => 2,
        -wrap => 'word',
        -relief => 'flat',
        -background => $self->{info_frame}->cget('-background'),
        -font => ['helvetica', $FONT_SIZE_TEXT]
    )->pack(-anchor => 'w', -padx => 10, -fill => 'x');
    
    # Tag: event name (e.g., "click", "doubleclick")
    $self->{highlighted_text}->tagConfigure('event',
        -foreground => $COLOR_EVENT,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold']
    );
    
    # Tag: action name (e.g., "mouse_left_click", "key_space")
    $self->{highlighted_text}->tagConfigure('action',
        -foreground => $COLOR_ACTION,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold', 'italic']
    );
    
    $self->{info_frame}->Frame(-height => 1)->pack();  # Spacer
    
    # ===== Active Preset Section =====
    $self->{info_frame}->Label(
        -text => 'Active Preset:',
        -font => ['helvetica', $FONT_SIZE_HEADING, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{active_text} = $self->{info_frame}->Text(
        -height => 2,
        -wrap => 'word',
        -relief => 'flat',
        -background => $self->{info_frame}->cget('-background'),
        -font => ['helvetica', $FONT_SIZE_TEXT]
    )->pack(-anchor => 'w', -padx => 10, -fill => 'x');
    
    # Tag: event name (e.g., "click", "longpress")
    $self->{active_text}->tagConfigure('event',
        -foreground => $COLOR_EVENT,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold']
    );
    
    # Tag: action name (e.g., "mouse_left_click", "open_main_ui")
    $self->{active_text}->tagConfigure('action',
        -foreground => $COLOR_ACTION,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold', 'italic']
    );
    
    $self->{info_frame}->Frame(-height => 1)->pack();  # Spacer
    
    # ===== Quick Assignments Section =====
    $self->{info_frame}->Label(
        -text => 'Quick Assignments:',
        -font => ['helvetica', $FONT_SIZE_HEADING, 'bold']
    )->pack(-anchor => 'w');
    
    $self->{quick_text} = $self->{info_frame}->Text(
        -height => 2,
        -wrap => 'word',
        -relief => 'flat',
        -background => $self->{info_frame}->cget('-background'),
        -font => ['helvetica', $FONT_SIZE_TEXT]
    )->pack(-anchor => 'w', -padx => 10, -fill => 'x');
    
    # Tag: event name (e.g., "click", "mediumpress")
    $self->{quick_text}->tagConfigure('event',
        -foreground => $COLOR_EVENT,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold']
    );
    
    # Tag: action name (e.g., "custom_scroll_down")
    $self->{quick_text}->tagConfigure('action',
        -foreground => $COLOR_ACTION,
        -font => ['helvetica', $FONT_SIZE_TEXT, 'bold', 'italic']
    );
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
    
    $self->_update_ui_help();
    $self->_update_highlighted_info();
    $self->_update_active_info();
    $self->_update_quick_info();
}

# Internal: Update UI help text
sub _update_ui_help {
    my ($self) = @_;
    
    my $text = $self->{ui_help_text};
    $text->delete('1.0', 'end');
    
    my $ui_events = $self->{core}->config->get('event_mappings', 'main_ui') // {};
    
    my @events = sort keys %$ui_events;
    return unless @events;
    
    for my $i (0 .. $#events) {
        my $event = $events[$i];
        my $action = $ui_events->{$event};
        
        $text->insert('end', $event, 'event');
        $text->insert('end', ' → ');
        $text->insert('end', $action, 'action');
        
        if ($i < $#events) {
            $text->insert('end', ' — ');
        }
    }
}

# Internal: Update highlighted preset info
sub _update_highlighted_info {
    my ($self) = @_;
    
    my $text = $self->{highlighted_text};
    $text->delete('1.0', 'end');
    
    my $cell = $self->{grid}->get_current_cell();
    
    if ($cell && $cell->{type} eq 'preset') {
        my $preset_id = $cell->{preset_id};
        my $preset = $self->{core}->config->get('presets', $preset_id);
        
        if ($preset) {
            $text->insert('end', $preset->{label} . ': ');
            
            my $events = $preset->{events} // {};
            my @events = sort keys %$events;
            
            for my $i (0 .. $#events) {
                my $event = $events[$i];
                my $action = $events->{$event};
                
                $text->insert('end', $event, 'event');
                $text->insert('end', ' → ');
                $text->insert('end', $action, 'action');
                
                if ($i < $#events) {
                    $text->insert('end', ' — ');
                }
            }
        }
    } else {
        $text->insert('end', '(Not on a preset)');
    }
}

# Internal: Update active preset info
sub _update_active_info {
    my ($self) = @_;
    
    my $text = $self->{active_text};
    $text->delete('1.0', 'end');
    
    my $active_id = $self->{core}->config->get('active_preset');
    my $preset = $self->{core}->config->get('presets', $active_id);
    
    if ($preset) {
        $text->insert('end', $preset->{label} . ': ');
        
        my $events = $preset->{events} // {};
        my @events = sort keys %$events;
        
        for my $i (0 .. $#events) {
            my $event = $events[$i];
            my $action = $events->{$event};
            
            $text->insert('end', $event, 'event');
            $text->insert('end', ' → ');
            $text->insert('end', $action, 'action');
            
            if ($i < $#events) {
                $text->insert('end', ' — ');
            }
        }
    }
}

# Internal: Update quick assignments info
sub _update_quick_info {
    my ($self) = @_;
    
    my $text = $self->{quick_text};
    $text->delete('1.0', 'end');
    
    my $sticky = $self->{core}->mapper->get_quick_assignments('sticky');
    my $transient = $self->{core}->mapper->get_quick_assignments('transient');
    
    my @parts;
    
    if (%$sticky) {
        for my $event (sort keys %$sticky) {
            push @parts, { event => $event, action => $$sticky{$event}, type => 'sticky' };
        }
    }
    
    if (%$transient) {
        for my $event (sort keys %$transient) {
            push @parts, { event => $event, action => $$transient{$event}, type => 'transient' };
        }
    }
    
    if (@parts) {
        for my $i (0 .. $#parts) {
            my $p = $parts[$i];
            
            $text->insert('end', $p->{event}, 'event');
            $text->insert('end', ' → ');
            $text->insert('end', $p->{action}, 'action');
            $text->insert('end', " ($p->{type})");
            
            if ($i < $#parts) {
                $text->insert('end', ' — ');
            }
        }
    } else {
        $text->insert('end', '(None)');
    }
}

1;

__END__

=head1 NAME

HIDUI::UI::MainWindow - Main UI window

=head1 DESCRIPTION

The main navigation UI that displays presets and quick actions.

=cut

# vim: et ts=4 sts=4 sw=4
