package ButtonGesture::OverlayStatus;
use v5.36;
use strict;
use warnings;
use Tk;
use List::Util qw(max);

sub new {
    my ($class, %args) = @_;
    
    my $patterns = $args{patterns} // [];
    my $quantum_s = $args{quantum_s} // 0.02;
    my $width_px = $args{width_px} // 200;
    my $height_px = $args{height_px} // 30;
    my $pattern_spacing_y = $args{pattern_spacing_y_px} // 1;
    
    # Calculate max pattern length for scaling
    my $max_pattern_time_s = 0;
    for my $p (@$patterns) {
        my $len_total = $p->{len_total} // 0;
        my $time_s = $len_total * $quantum_s;
        $max_pattern_time_s = $time_s if $time_s > $max_pattern_time_s;
    }
    $max_pattern_time_s = 1.0 if $max_pattern_time_s == 0;  # Avoid division by zero
    
    # Scale factor: patterns use 1/1.2 of width (leave 20% padding on right)
    my $scale_factor = ($width_px / 1.2) / $max_pattern_time_s;
    
    # Default pattern colors (30% opacity)
    my @default_colors = (
        [255, 80, 80],      # Red
        [255, 200, 80],     # Orange  
        [100, 255, 100],    # Green
        [100, 200, 255],    # Blue
        [200, 100, 255],    # Purple
        [255, 100, 200],    # Pink
    );
    
    my $pattern_colors = $args{pattern_colors} // \@default_colors;
    
    # Create Tk window
    my $mw = MainWindow->new();
    $mw->overrideredirect(1);  # Remove window decorations
    $mw->attributes('-topmost', 1);  # Stay on top
    $mw->geometry("${width_px}x${height_px}+100+100");
    
    # Create canvas
    my $canvas = $mw->Canvas(
        -width => $width_px,
        -height => $height_px,
        -background => '#000000',
        -highlightthickness => 0,
    )->pack();
    
    my $self = bless {
        mw => $mw,
        canvas => $canvas,
        patterns => $patterns,
        quantum_s => $quantum_s,
        width_px => $width_px,
        height_px => $height_px,
        scale_factor => $scale_factor,
        pattern_spacing_y => $pattern_spacing_y,
        pattern_colors => $pattern_colors,
        user_press_color => [235, 60, 255],
        user_release_color => [0, 0, 0],
        _drag_x => undef,
        _drag_y => undef,
    }, $class;
    
    # Draw pattern backgrounds
    $self->_draw_patterns();
    
    # Setup drag bindings
    $canvas->bind('<Button-1>', sub { $self->_start_drag(@_) });
    $canvas->bind('<B1-Motion>', sub { $self->_do_drag(@_) });
    
    return $self;
}

sub _draw_patterns {
    my ($self) = @_;
    
    my $num_patterns = scalar(@{$self->{patterns}});
    return if $num_patterns == 0;
    
    my $total_spacing = ($num_patterns - 1) * $self->{pattern_spacing_y};
    my $row_height = int(($self->{height_px} - $total_spacing) / $num_patterns);
    
    my $y_offset = 0;
    for my $i (0 .. $num_patterns - 1) {
        my $p = $self->{patterns}[$i];
        my $len_total = $p->{len_total} // 0;
        my $time_s = $len_total * $self->{quantum_s};
        my $width = int($time_s * $self->{scale_factor});
        
        # Get color for this pattern (cycle through palette)
        my $color_idx = $i % scalar(@{$self->{pattern_colors}});
        my ($r, $g, $b) = @{$self->{pattern_colors}[$color_idx]};
        my $color = sprintf('#%02x%02x%02x', $r, $g, $b);
        
        # Draw pattern rectangle with 30% opacity (stipple approximation)
        $self->{canvas}->createRectangle(
            0, $y_offset,
            $width, $y_offset + $row_height,
            -fill => $color,
            -outline => '',
            -stipple => 'gray50',  # Approximate 50% opacity
            -tags => ['pattern', "pattern_$i"],
        );
        
        $y_offset += $row_height + $self->{pattern_spacing_y};
    }
}

sub update {
    my ($self, $obs_runs) = @_;
    
    # Delete previous user overlay
    $self->{canvas}->delete('user_overlay');
    
    return unless $obs_runs && @$obs_runs;
    
    # Calculate total width of observation
    my $x_pos = 0;
    for my $run (@$obs_runs) {
        my $len_dots = $run->{len} // 0;
        my $time_s = $len_dots * $self->{quantum_s};
        my $width = int($time_s * $self->{scale_factor});
        
        # Determine color based on press/release
        my ($r, $g, $b);
        if ($run->{sym} eq '.') {
            ($r, $g, $b) = @{$self->{user_press_color}};
        } else {
            ($r, $g, $b) = @{$self->{user_release_color}};
        }
        
        # Skip drawing if transparent (release)
        if ($r == 0 && $g == 0 && $b == 0) {
            $x_pos += $width;
            next;
        }
        
        my $color = sprintf('#%02x%02x%02x', $r, $g, $b);
        
        # Draw full-height rectangle with 40% opacity (stipple approximation)
        $self->{canvas}->createRectangle(
            $x_pos, 0,
            $x_pos + $width, $self->{height_px},
            -fill => $color,
            -outline => '',
            -stipple => 'gray50',  # Approximate 50% opacity
            -tags => ['user_overlay'],
        );
        
        $x_pos += $width;
    }
    
    $self->{canvas}->update();
}

sub clear {
    my ($self) = @_;
    $self->{canvas}->delete('user_overlay');
    $self->{canvas}->update();
}

sub _start_drag {
    my ($self, $widget) = @_;
    my $e = $widget->XEvent;
    $self->{_drag_x} = $e->x;
    $self->{_drag_y} = $e->y;
}

sub _do_drag {
    my ($self, $widget) = @_;
    my $e = $widget->XEvent;
    
    return unless defined $self->{_drag_x} && defined $self->{_drag_y};
    
    my $dx = $e->x - $self->{_drag_x};
    my $dy = $e->y - $self->{_drag_y};
    
    my $x = $self->{mw}->x + $dx;
    my $y = $self->{mw}->y + $dy;
    
    $self->{mw}->geometry("+$x+$y");
}

1;