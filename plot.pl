use Modern::Perl;
use Chart::Gnuplot;
use DBI;
use POSIX q(strftime);
use Time::Local;

# TODO: getopt
our $DB_FILE       = defined $ARGV[0] ? $ARGV[0] : "lightp.db";
our $IGNORE_EVERY  = defined $ARGV[1] ? int($ARGV[1]) : 1;
our $TARGET        = defined $ARGV[2] ? $ARGV[2] : "plot.png";

my $dbh = DBI->connect("dbi:SQLite:dbname=$DB_FILE", "", "");

my $sth = $dbh->prepare(q(select timestamp, bavg from avgs where id % ? = 0;));
$sth->bind_param(1, $IGNORE_EVERY);
my $r = $sth->execute or die $DBI::errstr;

my (@xs, @ys);
my @t = localtime(time);
my $offset = timegm(@t) - timelocal(@t);

while ($_ = $sth->fetch) {
  push @xs, int(0+$_->[0]/1000) + $offset;
  push @ys, 0+$_->[1];
}

my ($begin, $end) = ($xs[0], $xs[$#xs-1]);

sub disp($) {
  my ($t) = @_;
  strftime "%F %T", localtime $t
}

printf "begin: %s, end: %s\n", disp $begin, disp $end;

my $chart = Chart::Gnuplot->new(
  output   => $TARGET,
  title    => "Light levels",
  xlabel   => "time",
  ylabel   => "level (0-255)",
  timeaxis => "x",
  bg       => "white"
);

my $ds = Chart::Gnuplot::DataSet->new(
  xdata   => \@xs,
  ydata   => \@ys,
  style   => "lines",
  timefmt => '%s',
);

$chart->plot2d($ds);
