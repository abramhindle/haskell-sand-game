use Harbinger;

my $H = Harbinger->new(port=>15011);
$H->addHandler("Sand5",new Harbinger::DebugHandler());
$H->run;
