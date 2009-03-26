use strict;
use Harbinger;
use Time::HiRes qw ( time );
use IO::File;

my %typemap = (
               Acid => "ACID",
               16 => "EATERWALL",
               200 => "WALL",
               DustGenerator => "SNOWMAKER",
               198 => "DUSTMAKER",
               LightDust => "SNOW",
               Dust => "DUST",
              );




$SIG{PIPE} = sub { die join(",",@_)," PIPE$/"; };
my $H = Harbinger->new(port=>15011);
$H->addHandler("Sand5",new
	Harbinger::PipeHandler(
		'open'=>"csound -dm6 -L stdin -o devaudio sine2.orc sine.sco",
		autoflush=>1,
		terminator=>$/,
		filter=>\&wrap_filterit,
	)
);

#records the score
my $file = "scores/".time().".sco";
my $fd = IO::File->new($file, "w+");
warn "READY";
my $then = time;

$fd->autoflush(1);
$H->run;

#records the score
sub wrap_filterit {
    my ($name,$id,$dest,$smsg) = filterit(@_);
    if ($smsg) {
        foreach my $msg (split($/,$smsg)) {
            next unless $msg;
            my @parts = split(/\s+/,$msg);
            my $newtime = time - $then;
            $parts[1] += $newtime;
            $fd->print(join(" ",@parts).$/);
        }
    }
    return ($name,$id,$dest,$smsg);
}

sub filterit {
	my ($self,$name,$id,$dest,$msg) = @_;
	my @args = split(/\s+/,$msg);
	my $command = shift @args;
        my $freq;
        my $loudness;
        warn ".";
        if ($msg =~ /^Eaten at/) {
            my ($x,$y,$eaten) = ($msg =~ /Eaten at (\d+) (\d+) (\w+)\s*$/);
            $x *= 10;
            $y *= 10;
            my $duration = 3*duration($eaten);
            my $loudness = 0.3*(100 + $y);
            my $pitch = 40 + abs(240 - $y) + abs(320 - $x);
            $pitch *= pitchmult($eaten);
            my $wait = 0.001+0.2*rand();
            my $nmsg = "i1 $wait $duration $loudness $pitch";
            return ($name,$id,$dest,$nmsg);
        }
	warn "DID NOT HANDLE: $msg";
	return ($name,$id,$dest,undef);
}


sub duration {
    my ($type) = @_;
    my %duration = (
                    ACID => 0.2,
                    DUST => 0.05,
                    SNOW => 0.03,
                    SNOWMAKER => 0.1,                    
                    DUSTMAKER => 0.1,                    
                   );
    $duration{$typemap{$type}};
}

sub pitchmult {
    my ($type) = @_;
    my %duration = (
                    ACID => 0.5,
                    DUST => 3,
                    SNOW => 5,
                    SNOWMAKER => 20,  
                    DUSTMAKER => 0.3,
                   );
    $duration{$typemap{$type}};
}
