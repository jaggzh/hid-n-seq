package HIDUI::Config;

use strict;
use warnings;
use JSON::PP;
use File::Path qw(make_path);
use File::Basename qw(dirname);
use Carp qw(croak);

# Constructor
# Takes config_dir path (e.g., "config/")
sub new {
    my ($class, %args) = @_;
    
    my $config_dir = $args{config_dir} // 'config';
    
    my $self = {
        config_dir => $config_dir,
        defaults_file => "$config_dir/defaults.json",
        user_file => "$config_dir/user.json",
        data => {},  # Merged config data
    };
    
    bless $self, $class;
    
    # Load configuration on construction
    $self->load();
    
    return $self;
}

# Load configuration (defaults + user overrides)
sub load {
    my ($self) = @_;
    
    # Start with defaults
    my $defaults = $self->_load_json_file($self->{defaults_file});
    croak "Failed to load defaults from $self->{defaults_file}" unless $defaults;
    
    # Try to load user overrides (might not exist yet)
    my $user = $self->_load_json_file($self->{user_file});
    $user //= {};  # Empty hash if no user config exists
    
    # Merge: user overrides defaults
    $self->{data} = $self->_deep_merge($defaults, $user);
    
    return $self->{data};
}

# Save current configuration to user file
sub save {
    my ($self) = @_;
    
    # Ensure config directory exists
    my $dir = dirname($self->{user_file});
    make_path($dir) unless -d $dir;
    
    # Save to user config file
    return $self->_save_json_file($self->{user_file}, $self->{data});
}

# Get configuration value by key path
# Example: $config->get('presets', 'preset_general', 'label')
sub get {
    my ($self, @keys) = @_;
    
    my $value = $self->{data};
    
    for my $key (@keys) {
        return undef unless ref $value eq 'HASH';
        $value = $value->{$key};
        return undef unless defined $value;
    }
    
    return $value;
}

# Set configuration value by key path
# Example: $config->set(['presets', 'preset_general', 'label'], 'New Label')
sub set {
    my ($self, $keys, $value) = @_;
    
    croak "Keys must be an array reference" unless ref $keys eq 'ARRAY';
    croak "Need at least one key" unless @$keys;
    
    my $current = $self->{data};
    
    # Navigate to the parent of the final key
    for my $i (0 .. $#$keys - 1) {
        my $key = $keys->[$i];
        $current->{$key} //= {};  # Create intermediate hashes as needed
        $current = $current->{$key};
    }
    
    # Set the final value
    my $final_key = $keys->[-1];
    $current->{$final_key} = $value;
    
    return $value;
}

# Get entire config data structure
sub data {
    my ($self) = @_;
    return $self->{data};
}

# Load JSON from file
sub _load_json_file {
    my ($self, $filepath) = @_;
    
    return undef unless -e $filepath;
    
    open my $fh, '<', $filepath or do {
        warn "Could not open $filepath: $!";
        return undef;
    };
    
    my $json_text = do { local $/; <$fh> };
    close $fh;
    
    my $data = eval { decode_json($json_text) };
    if ($@) {
        warn "Failed to parse JSON from $filepath: $@";
        return undef;
    }
    
    return $data;
}

# Save data structure to JSON file
sub _save_json_file {
    my ($self, $filepath, $data) = @_;
    
    my $json_text = encode_json($data);
    
    open my $fh, '>', $filepath or do {
        warn "Could not write to $filepath: $!";
        return 0;
    };
    
    print $fh $json_text;
    close $fh;
    
    return 1;
}

# Deep merge two hash structures
# Second hash overrides first hash
sub _deep_merge {
    my ($self, $base, $override) = @_;
    
    # If override is not a hash, it completely replaces base
    return $override unless ref $override eq 'HASH';
    return $override unless ref $base eq 'HASH';
    
    my %merged = %$base;  # Start with base
    
    # Apply overrides
    for my $key (keys %$override) {
        if (ref $override->{$key} eq 'HASH' && ref $base->{$key} eq 'HASH') {
            # Recursively merge nested hashes
            $merged{$key} = $self->_deep_merge($base->{$key}, $override->{$key});
        } else {
            # Direct override
            $merged{$key} = $override->{$key};
        }
    }
    
    return \%merged;
}

1;

__END__

=head1 NAME

HIDUI::Config - Configuration management for HIDUI

=head1 SYNOPSIS

    use HIDUI::Config;
    
    my $config = HIDUI::Config->new(config_dir => 'config');
    
    # Get values
    my $label = $config->get('presets', 'preset_general', 'label');
    my $active = $config->get('active_preset');
    
    # Set values
    $config->set(['active_preset'], 'preset_gaming');
    $config->set(['presets', 'preset_new', 'label'], 'New Preset');
    
    # Save changes
    $config->save();

=head1 DESCRIPTION

Handles loading defaults.json and user.json, merging them, and
providing access to configuration data.

=cut