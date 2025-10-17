# HIDUI - Single Button Interface

Accessibility UI for single-button gesture-based control. Part of the hid-n-seq project.

## Overview

HIDUI provides a configurable interface that maps gesture events (from hid-n-seq) to computer actions. Users can navigate menus, switch presets, and perform actions using only a single button with different press patterns.

## Features

- **Event-to-Action Mapping**: Map gesture events (click, longpress, etc.) to actions (mouse clicks, keyboard keys, UI navigation)
- **Presets**: Multiple preset configurations for different contexts (gaming, coding, general use)
- **Quick Assignments**: Temporarily or persistently override preset mappings
- **Navigable UI**: Grid-based interface navigable via single button
- **Preset Configuration**: Edit event mappings for each preset

## Installation

### Dependencies

```bash
# Debian/Ubuntu
sudo apt-get install perl perl-tk xdotool libjson-pp-perl

# The following Perl modules should be available:
# - Tk (perl-tk package)
# - JSON::PP (libjson-pp-perl or core in modern Perl)
# - File::Path (core)
# - File::Basename (core)
```

### Setup

```bash
cd hid-n-seq/hidui
chmod +x main.pl
```

## Configuration

Configuration is stored in JSON files in the `config/` directory:

- `defaults.json` - System defaults (shipped with HIDUI)
- `user.json` - User overrides (created on first run)

### Config Structure

See `config/defaults.json` for the full structure. Key sections:

- **presets**: Define event-to-action mappings for different contexts
- **ui_layout**: Configure the main UI grid
- **event_mappings**: Mode-specific event mappings (main_ui, preset_config)
- **quick_assignments**: Sticky and transient quick overrides

## Usage

### Basic Usage

```perl
use HIDUI::Core;
use HIDUI::UI::Manager;

# Initialize
my $hidui = HIDUI::Core->new(config_dir => 'config');
my $ui = HIDUI::UI::Manager->new(core => $hidui);

# Wire up gesture callback
$gesture_module->on_event(sub {
    my $event_name = shift;
    $hidui->event($event_name);
});

# Run
$hidui->run();
```

### Registering Custom Actions

```perl
$hidui->register_action(
    id => 'my_custom_action',
    label => 'My Custom Action',
    group => 'custom',
    handler => sub {
        my ($core, $event, $action, $params) = @_;
        # Do something
    }
);
```

## Architecture

```
HIDUI/
├── Core.pm              # Main coordinator
├── Config.pm            # JSON config management
├── ActionRegistry.pm    # Action registration/execution
├── EventMapper.pm       # Event-to-action resolution
├── Actions/             # Built-in action modules
│   ├── Mouse.pm
│   ├── Keyboard.pm
│   ├── UI.pm
│   └── Presets.pm
└── UI/                  # UI components
    ├── Manager.pm
    ├── MainWindow.pm
    ├── PresetConfig.pm
    └── NavigableGrid.pm
```

## Event Resolution Priority

When an event occurs, HIDUI resolves it to an action using this priority:

1. **Mode-specific mappings** (when in UI mode)
2. **Sticky quick assignments** (persist across preset changes)
3. **Transient quick assignments** (cleared on preset change)
4. **Active preset mappings**
5. **No action** (warning logged)

## Modes

- **desktop_mode**: Normal desktop operation, uses active preset
- **main_ui**: Main UI window is open, uses UI navigation mappings
- **preset_config**: Preset config window open, uses config navigation mappings

## Built-in Actions

### Mouse Actions
- mouse_left_click, mouse_right_click, mouse_middle_click
- mouse_double_click
- mouse_scroll_up, mouse_scroll_down

### Keyboard Actions
- key_enter, key_space, key_escape, key_tab
- key_up, key_down, key_left, key_right
- key_down_repeat, key_up_repeat (10x)

### UI Actions
- open_main_ui, close_current_window
- ui_navigate_next, ui_navigate_prev, ui_activate_current
- open_preset_config

### Preset Actions
- preset_switch (switches active preset)

## Development

### Adding New Actions

1. Create action in appropriate module (or new module in `Actions/`)
2. Register in the module's `register_all()` function
3. Add to a preset or quick assignment in config

### Adding UI Features

- Extend `UI/MainWindow.pm` for main UI enhancements
- Create new UI windows similar to `PresetConfig.pm`
- Update `UI/Manager.pm` to coordinate new windows

## TODO / Future Features

- Action selector dialog for preset config
- Quick assign UI
- Audio feedback
- Position/breadcrumb announcements
- Window auto-close on inactivity
- Context-aware preset switching (per-application)
- Undo/redo for complex operations

## License

[Your license here]

## Credits

Created for accessibility, particularly for users with limited motor control. Designed in collaboration with users who need single-button interfaces.