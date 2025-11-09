# lib/HIDUI/EventClient.pm
package HIDUI::EventClient;

use strict;
use warnings;
use IO::Socket::INET;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $port = $args{port} // 9876;
    my $host = $args{host} // 'localhost';
    
    my $self = {
        port => $port,
        host => $host,
    };
    
    bless $self, $class;
    
    return $self;
}

# Send event to HIDUI server
sub send_event {
    my ($self, $event_name) = @_;
    
    return unless defined $event_name && $event_name ne '';
    
    # Connect and send
    my $socket = IO::Socket::INET->new(
        PeerHost => $self->{host},
        PeerPort => $self->{port},
        Proto => 'tcp',
        Timeout => 1,
    );
    
    unless ($socket) {
        warn "Cannot connect to HIDUI server: $!\n";
        return 0;
    }
    
    print $socket "$event_name\n";
    close $socket;
    
    return 1;
}

1;

__END__

=head1 NAME

HIDUI::EventClient - TCP client for sending events to HIDUI

=head1 SYNOPSIS

    use HIDUI::EventClient;
    
    my $client = HIDUI::EventClient->new(port => 9876);
    $client->send_event('click');

=head1 DESCRIPTION

Sends event names to HIDUI via TCP socket. Used by gesture recognizer.

=cut

# vim: et ts=4 sts=4 sw=4