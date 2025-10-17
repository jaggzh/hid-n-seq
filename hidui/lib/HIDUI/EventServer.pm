# lib/HIDUI/EventServer.pm
package HIDUI::EventServer;

use strict;
use warnings;
use IO::Socket::INET;
use IO::Select;

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
        select => undef,
    };
    
    bless $self, $class;
    
    return $self;
}

sub start {
    my ($self, $main_window) = @_;
    
    # Create listening socket
    $self->{socket} = IO::Socket::INET->new(
        LocalHost => $self->{host},
        LocalPort => $self->{port},
        Proto => 'tcp',
        Listen => 5,
        ReuseAddr => 1,
    ) or die "Cannot create socket on $$self{host}:$$self{port}: $!";
    
    $self->{socket}->blocking(0);
    $self->{select} = IO::Select->new($self->{socket});
    
    print "HIDUI EventServer listening on $$self{host}:$$self{port}\n";
    
    # Use the actual MainWindow's repeat method
    $main_window->repeat(50, sub { $self->_poll() });
}

sub _poll {
    my ($self) = @_;
    
    my @ready = $self->{select}->can_read(0);
    return unless @ready;
    
    my $client = $self->{socket}->accept();
    return unless $client;
    
    my $event_name = <$client>;
    if (defined $event_name) {
        chomp $event_name;
        $event_name =~ s/\s+$//;
        
        if ($event_name ne '') {
            print "Received event: $event_name\n";
            $self->{core}->event($event_name);
        }
    }
    
    close $client;
}

sub stop {
    my ($self) = @_;
    close $self->{socket} if $self->{socket};
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
