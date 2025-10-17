# lib/HIDUI/EventServer.pm
package HIDUI::EventServer;

use strict;
use warnings;
use IO::Socket::INET;
use Tk;

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $core = $args{core} or die "EventServer requires core object";
    my $port = $args{port} // 9876;
    my $host = $args{host} // 'localhost';
    
    my $self = {
        core => $core,
        port => $port,
        host => $host,
        socket => undef,
        clients => [],
    };
    
    bless $self, $class;
    
    return $self;
}

# Start listening for events
# Must be called after at least one Tk window exists
sub start {
    my ($self) = @_;
    
    # Create listening socket
    $self->{socket} = IO::Socket::INET->new(
        LocalHost => $self->{host},
        LocalPort => $self->{port},
        Proto => 'tcp',
        Listen => 5,
        ReuseAddr => 1,
    ) or die "Cannot create socket: $!";
    
    print "HIDUI EventServer listening on $$self{host}:$$self{port}\n";
    
    # Make socket non-blocking
    $self->{socket}->blocking(0);
    
    # Use Tk's repeating timer to poll the socket
    # This is more portable than fileevent
    $self->_setup_polling();
}

# Internal: Setup polling timer
sub _setup_polling {
    my ($self) = @_;
    
    # Poll every 50ms for incoming connections
    my $timer;
    $timer = sub {
        $self->_check_for_connections();
        Tk->after(50, $timer);
    };
    
    Tk->after(50, $timer);
}

# Internal: Check for incoming connections (non-blocking)
sub _check_for_connections {
    my ($self) = @_;
    
    return unless $self->{socket};
    
    # Try to accept (non-blocking)
    my $client = $self->{socket}->accept();
    return unless $client;
    
    # Read event name from client
    my $event_name = <$client>;
    
    if (defined $event_name) {
        chomp $event_name;
        $event_name =~ s/\s+$//;  # Strip trailing whitespace
        
        if ($event_name ne '') {
            print "Received event: $event_name\n";
            $self->{core}->event($event_name);
        }
    }
    
    close $client;
}

# Stop listening
sub stop {
    my ($self) = @_;
    
    if ($self->{socket}) {
        close $self->{socket};
        $self->{socket} = undef;
    }
}

1;

__END__

=head1 NAME

HIDUI::EventServer - TCP server for receiving gesture events

=head1 SYNOPSIS

    my $server = HIDUI::EventServer->new(
        core => $hidui,
        port => 9876
    );
    
    $server->start();

=head1 DESCRIPTION

Listens for TCP connections and forwards event names to HIDUI core.
Integrates with Tk's event loop using a polling timer.

=cut

# vim: et ts=4 sts=4 sw=4
