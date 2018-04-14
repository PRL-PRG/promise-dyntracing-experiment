#!/usr/bin/env perl
# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Ingo Korb, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
# ------------------------------------------------------------------


use warnings;
use strict;
use feature ':5.10';
use Cwd;
use DBI;
use Data::Dumper;
use Errno;
use File::Basename;
use File::Spec::Functions qw(catfile rel2abs splitpath);
use File::Temp qw/ :mktemp /;
use FileHandle;
use FindBin qw($Bin);
use Getopt::Long;
use IPC::Open3;
use Pod::Usage;
use POSIX ':sys_wait_h';
use Time::HiRes qw/gettimeofday tv_interval/;

# ----- constants -----

use constant RunsTableVersion     => 1;
use constant ResultsTableVersion  => 1;

# ----- subs -----

### config file parser ###
sub parse_config {
    my $confname = shift;
    my %config = ( "variables" => {}, "interpreters" => {} );

    # variable substitution in a string
    sub variable_subst {
        my $config = shift;
        my $str    = shift;

        while ($str =~ /\$(\w+)/) {
            my $varname = $1;

            if (!exists($$config{variables}{$varname})) {
                say STDERR "ERROR: Did not find variable \$$varname in line $. of config file";
                exit 2;
            }

            $str =~ s/\$$varname/$$config{variables}{$varname}/e;
        }

        return $str;
    }

    # consume a string element from the input,
    # handles quoting and variable expansion
    sub consume_string {
        my $config = shift;
        my $str    = shift;

        if (substr($str, 0, 1) eq "\"") {
            # quoted string
            if ($str =~ /^\"([^\"]*)\"\s*(.*)$/) {
                return (variable_subst($config, $1), $2);
            } else {
                say STDERR "ERROR: Could not parse string in line $. of config file";
                exit 2;
            }

        } else {
            # unquoted string
            my ($arg, $rest) = split(/\s+/, $str, 2);
            return (variable_subst($config, $arg), $rest);
        }
    }

    # consumes a trailing comment
    sub consume_eol {
        my $str     = shift;
        my $command = shift;

        return if !defined($str);
        return if $str eq "";
        if ($str !~ /^\s*\#/) {
            say STDERR "ERROR: Too many arguments for $command in line $. of config file";
            exit 2;
        }
    }

    # complains if no current interpreter is set
    sub check_interpreter {
        my $cur     = shift;
        my $command = shift;

        if (!defined($cur)) {
            say STDERR "ERROR: No current interpreter set for $command in line $. of config file";
            exit 2;
        }
    }


    return \%config unless -r $confname; # FIXME: Return type

    open IN, "<", $confname or die "Can't open $confname for reading: $!";

    my $current_interpreter;

    while (<IN>) {
        chomp;
        s/^\s+//; s/\s+$//;
        next if $_ eq "";
        next if /^\#/;

        if (!/^(\w+)\s*(.*)$/) {
            say STDERR "ERROR: No keyword in line $. of config file";
            exit 2;
        }

        my ($keyword, $args) = ($1, $2);

        if ($keyword eq "set") {
            my ($name, $value);

            ($name, $args)  = consume_string(\%config, $args);
            ($value, $args) = consume_string(\%config, $args);
            consume_eol($args, "set");

            if (exists($config{variables}{$name}) &&
               $config{variables}{$name} ne $value) {
                say STDERR "WARNING: Variable $name redefined in line $. of config file";
            }

            $config{variables}{$name} = $value;

        } elsif ($keyword eq "interpreter") {
            my $name;

            ($name, $args) = consume_string(\%config, $args);
            consume_eol($args, "interpreter");

            if (exists($config{interpreters}{$name})) {
                say STDERR "ERROR: Found another interpreter section for $name in line $. of config file";
                exit 2;
            }

            $config{interpreters}{$name} = {"envvars" => {}};
            $current_interpreter = $name;

        } elsif ($keyword eq "run") {
            my $cmdline;

            check_interpreter($current_interpreter, "run");

            ($cmdline, $args) = consume_string(\%config, $args);
            consume_eol($args, "run");

            # FIXME: Check for redef
            $config{interpreters}{$current_interpreter}{run} = $cmdline;

        } elsif ($keyword eq "setenv") {
            my ($name, $value);

            ($name, $args)  = consume_string(\%config, $args);
            ($value, $args) = consume_string(\%config, $args);
            consume_eol($args, "setenv");

            # FIXME: Check for redef
            $config{interpreters}{$current_interpreter}{envvars}{$name} = $value;

        } else {
            say STDERR "ERROR: Unknown keyword $keyword in line $. of config file";
            exit 2;
        }
    }

    close IN;

    return \%config;
}


### interface to database ###

# DJB hash, used to checksum the column names for extra tables
sub string_checksum {
    my $checksum = 5381;

    while (my $string = shift) {
        for (my $i = 0; $i < length($string); $i++) {
            $checksum = (0xffffffff & (33 * $checksum)) ^
                ord(substr($string, $i, 1));
        }
    }
    return $checksum;
}

sub check_table_version {
    my $dbh    = shift;
    my $name   = shift;
    my $want   = shift;
    my $create = shift;

    my $result = $dbh->selectall_arrayref("SELECT version FROM schema_versions WHERE name = ?",
                                          undef, $name);
    if (scalar(@$result) == 0) {
        # no data, create table
        $dbh->do("CREATE TABLE $name ($create)");
        $dbh->do("INSERT INTO schema_versions (name, version) VALUES (?,?)",
                 undef, $name, $want);
    } else {
        # check version number
        if ($$result[0][0] != $want) {
            say STDERR "ERROR: Version number of table $name does not match!";
            say STDERR "       Please specify a different database or remove the current one.";
            exit 2;
        }
    }
}

sub create_tables {
    my $dbh = shift;

    # check if the database is empty
    my $sth = $dbh->table_info("", undef, "schema_versions", "TABLE");
    if (!$sth->fetch) {
        # create schema_version table
        # FIXME: Allow md5 checksums of table schemas?
        $dbh->do("CREATE TABLE schema_versions (name VARCHAR NOT NULL, version INTEGER NOT NULL)");
    }

    # check/create tables
    check_table_version($dbh, "runs", RunsTableVersion,
                        "id INTEGER NOT NULL PRIMARY KEY," .
                        "execpath VARCHAR NOT NULL," .
                        "scriptname VARCHAR NOT NULL," .
                        "interpretertag VARCHAR," .
                        "repetition INTEGER NOT NULL," .
                        "endtime INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP");

    check_table_version($dbh, "results", ResultsTableVersion,
                        "id INTEGER NOT NULL PRIMARY KEY," .
                        "run_id INTEGER NOT NULL REFERENCES Runs(id)," .
                        "key VARCHAR NOT NULL," .
                        "value INTEGER NOT NULL");
}

sub create_pivot {
    my $dbh            = shift;
    my $tablename      = shift;
    my $do_materialize = shift;

    my $pivotname = $tablename . "_pivot";
    my $sth;

    # remove old table/pivot
    $sth = $dbh->table_info("", undef, $pivotname, "TABLE");
    if ($sth->fetch) {
        say STDERR "NOTICE: Removing previous pivot table for $tablename";
        $dbh->do("DROP TABLE IF EXISTS $pivotname");
    }

    $sth = $dbh->table_info("", undef, $pivotname, "VIEW");
    if ($sth->fetch) {
        say STDERR "NOTICE: Removing previous pivot view for $tablename";
        $dbh->do("DROP VIEW IF EXISTS $pivotname");
    }

    # gather column names
    my @columns;

    $sth = $dbh->prepare("SELECT DISTINCT key FROM $tablename ORDER BY id");
    $sth->execute;
    while (my @row = $sth->fetchrow_array) {
        my $k = $row[0];
        if ($k =~ /^[a-zA-Z_0-9]+$/) {
            push @columns, $k;
        }
    }

    # generate select statement
    my $statement = "SELECT $tablename.run_id, runs.scriptname, runs.interpretertag, runs.repetition";
    foreach my $col (@columns) {
        $statement .= ",GROUP_CONCAT(CASE WHEN $tablename.key='$col' THEN $tablename.value ELSE NULL END) AS $col";
    }
    $statement .= " FROM $tablename LEFT JOIN runs ON $tablename.run_id=runs.id " .
        "GROUP BY run_id";

    # create table if materialized
    if ($do_materialize) {
        my $tabledef = "run_id INTEGER NOT NULL PRIMARY KEY, scriptname VARCHAR NOT NULL, interpretertag VARCHAR NOT NULL, repetition INTEGER NOT NULL";
        foreach my $col (@columns) {
            $tabledef .= ", $col INTEGER";
        }
        $dbh->do("CREATE TABLE ${tablename}_pivot ($tabledef)");
    }

    # create view or populate table
    if ($do_materialize) {
        $statement = "INSERT INTO ${tablename}_pivot ".$statement;
    } else {
        $statement = "CREATE VIEW ${tablename}_pivot AS ".$statement;
    }

    $dbh->do($statement);
}


### interface to R interpreters ###

sub subst_vars {
    my $input     = shift;
    my $script    = shift;
    my $benchargs = shift;
    my $statsfile = shift;

    my $output = "";

    my $subst_char = sub {
        my $ch = shift;
        return "%" if $ch eq "%";
        return "$script $benchargs" if $ch eq "c";
        return "$script --args $benchargs" if $ch eq "r";
        return $statsfile if $ch eq "s";
        say STDERR "ERROR: Unknown substitution character '$ch' in $input";
        exit 2;
    };

    $input =~ s/%(.)/&$subst_char($1)/ge;

    return $input
}

sub run_interp {
    my $interp    = shift;
    my $benchcmd  = shift;
    my $statsfile = rel2abs(mktemp("stats-$$-XXXXXX"));
    my ($bench, $benchargs)        = split / +/, $benchcmd, 2; # FIXME: Fails with spaces in bench path or filename
    $benchargs ||= "";
    my (undef, $benchdir, $script) = splitpath($bench);

    # prepare environment variables for this interpreter
    foreach my $e (keys %{$$interp{envvars}}) {
        my $val = $$interp{envvars}{$e};

        $val = subst_vars($val, $script, $benchargs, $statsfile);

        $ENV{$e} = $val;
    }

    my $curdir = getcwd();
    if ($benchdir ne "") {
        chdir($benchdir) or die "Can't change into $benchdir: $!";
    }

    my $stdout          = FileHandle->new;
    my $stderr          = FileHandle->new;
    my $autobench_aware = 0;
    my $alarm_active    = 0;
    my $alarm_hit       = 0;
    my $kill_tries      = 0;
    my $linebuf         = "";
    my @itertimes;
    my $interp_run = subst_vars($$interp{run}, $script, $benchargs, $statsfile);

    local $SIG{ALRM} = sub { $alarm_hit = 1; };

    my $endtime;
    my $starttime = [gettimeofday()];
    my $pid = open3(0, $stdout, $stderr, $interp_run); # FIXME: Split-args form would be better
    do {
        my ($rin, $rout, $nfound);

        $rin = '';
        vec($rin, fileno($stdout), 1) = 1;
        vec($rin, fileno($stderr), 1) = 1;

        if ($alarm_active) {
            $nfound = select($rout = $rin, undef, undef, 1);
        } else {
            $nfound = select($rout = $rin, undef, undef, undef);
        }

        if ($nfound < 0) {
            if (! $!{EINTR}) {
                say STDERR "Error while waiting for R interpreter:\n$!";
                exit 2;
            }
        } elsif ($nfound > 0) {
            if (vec($rout, fileno($stdout), 1)) {
                my ($buf, $nbyte);

                $nbyte = sysread($stdout, $buf, 1024);
                if ($nbyte < 0) {
                    # Error
                    say STDERR "Error reading R interpreter output: $!";
                    goto LOOPEND; # can't "last" in do {}
                }
                print "$buf";

                # parse embedded autobench markers in the output
                $linebuf .= $buf;
                while (1) {
                    # parse only full lines
                    my $idx = index($linebuf, "\n");

                    last if $idx < 0;
                    my $line = substr($linebuf, 0, $idx);
                    $linebuf = substr($linebuf, $idx + 1);

                    while ($line =~ /!!AUTOBENCH:([^!]+)!!/g) {
                        $autobench_aware = 1;

                        if ($1 eq "STARTED") {
                            $starttime = [gettimeofday()];
                        } elsif ($1 eq "FINISHED") {
                            $endtime = [gettimeofday()];
                            # give the program a grace period of 10 seconds to finish cleanly
                            $alarm_active = 1;
                            alarm 10;
                        } elsif ($1 =~ /^ITERTIME (.*)/) {
                            push @itertimes, 0+$1;
                        }
                    }
                }
            }

            if (vec($rout, fileno($stderr), 1)) {
                my ($buf, $nbyte);

                $nbyte = sysread($stderr, $buf, 1024);
                if ($nbyte < 0) {
                    # Error
                    say STDERR "Error reading R interpreter output: $!";
                    goto LOOPEND; # can't "last" in do {}
                }
                print $buf;
            }
        } elsif (!$alarm_active) {
            say STDERR "Huh, select returned with nfound=0?";
        } else {
            # select timed out or returned because of a signal
            if ($alarm_hit) {
                if ($kill_tries < 10) {
                    kill 'TERM', $pid;
                    $kill_tries++;
                } else {
                    kill 'KILL', $pid;
                }
            }
        }
      LOOPEND:
    } while (waitpid($pid, WNOHANG) == 0);

    # if the alarm is active, the end time was already measured
    $endtime = [gettimeofday()] unless $alarm_active;
    alarm 0;

    chdir($curdir) or die "Can't change back into $curdir: $!";

    # remove envvars again
    foreach my $e (keys %{$$interp{envvars}}) {
        delete $ENV{$e};
    }

    if ($? >> 8 && !$alarm_hit) {
        # non-zero exit status and not shot down by us
        return (-1, $statsfile, []);
    } else {
        my $elapsed = tv_interval($starttime, $endtime);
        printf STDERR "---- took %.2f seconds\n", $elapsed;
        return ($elapsed, $statsfile, \@itertimes);
    }
}

# read a tab-separated data file from timeR/r-instrumented
sub read_datafile {
    my $filename = shift;
    my @labels = ();
    my %data;
    my @dataorder;
    my %tables;

    open IN, "<", $filename or die "Can't open $filename: $!";
    while (<IN>) {
        chomp;
        next if /^(#[^!]|\s+#)/;
        my @words = split /\t/, $_;

        if ($words[0] =~ /^#!(.*)$/) {
            # found a marker line
            if ($1 eq "LABEL") {
                # label marker
                # arguments: labels for the subelements of the following data line(s)
                @labels = @words;
                shift @labels;

            } elsif ($1 eq "TABLE") {
                # output table marker
                # arguments: field name, table name
                # must come after a label line that defines the column names
                if (exists($tables{$words[1]})) {
                    say STDERR "WARNING: Table for $words[1] redefined in $filename line $.";
                    next;
                }

                $tables{$words[1]} = {
                    tablename => $words[2],
                    columns   => [@labels], # forces a copy
                    data      => []
                };

            } else {
                # unknown marker
                say STDERR "WARNING: Unknown marker \"$1\" ignored in $filename line $.";
            }

        } else {
            # found a data line

            if (exists($tables{$words[0]})) {
                # data is for a seperate table
                my $table = $words[0];

                if (scalar(@{$tables{$table}{columns}}) != scalar(@words)-1) {
                    say STDERR "ERROR: Column count mismatch in $filename line $.";
                    say STDERR "       Expected ", scalar(@{$tables{$table}{columns}}),
                               " columns, found ", scalar(@words)-1;
                    #say join("/", @{$tables{$table}{columns}}); # Debug
                    #say join("/", @words); # Debug
                    exit 2;
                }

                shift @words;
                push @{$tables{$table}{data}}, \@words;

            } else {
                # data is a normal data line
                if (exists($data{$words[0]})) {
                    if ($words[0] =~ /^<\.Internal>:La_rs/) {
                        # workaround for two duplicate entries in names.c
                        $words[0] .= "!5";
                    } else {
                        say STDERR "WARNING: Ignoring duplicate entry $words[0] in $filename line $.";
                        next;
                    }
                }

                push @dataorder, $words[0];

                if (scalar(@words) == 1) {
                    # no data values, error
                    say STDERR "ERROR: No value found in $filename line $.";
                    exit 2;
                } elsif (scalar(@words) == 2) {
                    # one data value, no suffix
                    $data{$words[0]} = $words[1];
                } else {
                    # multiple data values, add as hash of arrays (labels+data)
                    if (scalar(@words) != scalar(@labels) + 1) {
                        # difference between label count and value count
                        say STDERR "ERROR: Incorrect number of values found in $filename line $.";
                        say STDERR "(expected ", scalar(@labels),", found ", scalar(@words)-1, ")";
                        #say join("/",@labels); # Debug
                        #say join("/",@words);  # Debug
                        exit 2;
                    }

                    # generate value-hash
                    my $key = shift @words;
                    my %e = (
                        "labels" => [@labels],
                        "values" => [@words]
                        );

                    $data{$key} = \%e;
                }
            }
        }
    }
    close IN;

    return {
        order       => \@dataorder,
        data        => \%data,
        extratables => \%tables
    };
}

sub insert_results {
    my $dbh    = shift;
    my $run_id = shift;
    my $table  = shift;
    my $data   = shift;
    my @order  = @{$data->{order}};

    $dbh->begin_work();
    my $sth = $dbh->prepare("INSERT INTO $table (run_id, key, value) VALUES (?,?,?)");
    foreach (@order) {
        my $value = $data->{data}{$_};

        if (ref($value)) {
            # value with multiple fields
            my @keys;
            foreach my $k (@{$$value{labels}}) {
                push @keys, "${_}_$k";
            }

            $sth->bind_param_array(1, $run_id);
            $sth->bind_param_array(2, \@keys);
            $sth->bind_param_array(3, $$value{values});
            $sth->execute_array({});
        
        } else {
            # value with a single field
            $sth->execute($run_id, $_, $value);
        }
    }
    $dbh->commit();
}

sub insert_extratables {
    my $dbh       = shift;
    my $run_id    = shift;
    my $tablehash = shift;

    foreach my $tab (keys %$tablehash) {
        my $tablename = $$tablehash{$tab}{tablename};
        my @columns   = @{$$tablehash{$tab}{columns}};
        my $data      = $$tablehash{$tab}{data};

        # check/create table
        my $tabledef = "id INTEGER PRIMARY KEY NOT NULL," .
            "run_id INTEGER NOT NULL REFERENCES Runs(id)," .
            join(",", map { "$_ INTEGER" } @columns);

        check_table_version($dbh, $tablename,
                            string_checksum(@columns), $tabledef);

        # insert data
        $dbh->begin_work();
        my $sth = $dbh->prepare("INSERT INTO $tablename (run_id," .
                                join(",", @columns) . ") VALUES (?" .
                                (",?" x scalar(@columns)) . ")");

        foreach (@$data) {
            $sth->execute($run_id, @$_);
        }            

        $dbh->commit();
    }
}


# ----- main -----

# FIXME: Allow command-line specified config file?
my $conffile = "autobench.conf";
$conffile = catfile($Bin, $conffile) unless -r $conffile;

my %config = %{parse_config($conffile)};

my %interpreters;
my $show_help   = 0;
my $pivot_only  = 0;
my $DBFile      = "output.sqlite";
my $repetitions = 1;

sub parse_interpreter {
    my $arg = $_[1];

    if ($arg =~ /^([^=]+)=(.+)$/) {
        $interpreters{$1} = $2;

        if (!exists($config{interpreters}{$1})) {
            say STDERR "No interpreter named $1 found in config file";
            exit 2;
        }
    } else {
        $interpreters{$arg} = $arg;

        if (!exists($config{interpreters}{$arg})) {
            say STDERR "No interpreter named $arg found in config file";
            exit 2;
        }
    }
}

GetOptions(
    "help"           => \$show_help,
    "pivot-only!"    => \$pivot_only,
    "interpreter=s@" => \&parse_interpreter,
    "database|db=s"  => \$DBFile,
    "repetitions=i"  => \$repetitions,
    ) or pod2usage(1);

pod2usage(0) if $show_help;

if ($pivot_only) {
    if (scalar(@ARGV) != 0) {
        say STDERR "WARNING: Arguments supplied when none are expected, did you forget --db?";
    }

    # FIXME: Slight duplication
    # connect to database
    my $dbh = DBI->connect("DBI:SQLite:dbname=$DBFile", "", "",
                           { RaiseError => 1 } );
    $dbh->do("PRAGMA foreign_keys = ON");

    create_pivot($dbh, "results", 1);

    $dbh->disconnect;

    exit 0;
}

if (scalar(keys %interpreters) == 0) {
    say STDERR "No interprers selected on command line, exiting.";
    exit 0;
}

my @all_benches;
my $failed = 0;

while (scalar(@ARGV) != 0) {
    my $bench = shift;

    # use a default script if a directory is specified
    if (-d $bench) {
        $bench = catfile($bench, "run-benchmark.r");
    }

    # check if the specified file exists (after chopping args)
    # FIXME: Fails if the path or benchmark file name contains spaces
    my $script = $bench;

    $script =~ s/ .*$//;

    if (! -f $script) {
        say STDERR "ERROR: Benchmark $script does not exist!";
        $failed = 1;
        next;
    }

    push @all_benches, $bench;
}

if ($failed) {
    say STDERR "Exiting due to previous errors.";
    exit 2;
}

# connect to database
my $dbh = DBI->connect("DBI:SQLite:dbname=$DBFile", "", "",
    { RaiseError => 1 } );
$dbh->do("PRAGMA foreign_keys = ON");

# prepare database
create_tables($dbh);


for (my $iter = 1; $iter <= $repetitions; $iter++) {
        foreach my $bench (@all_benches) {
        my (undef, $benchpath, $benchname) = splitpath($bench);

        foreach my $interp (sort keys %interpreters) {
            say STDERR "---- Running $benchname with $interp, run $iter";

            my ($elapsed, $statsfile, $itertimes) = run_interp($config{interpreters}{$interp}, $bench);
            my $rundata;

            if (-e $statsfile) {
                $rundata = read_datafile($statsfile);
            } else {
                $rundata = {order => [], data => {}};
            }

            # add elapsed time
            unshift $rundata->{order}, "elapsed";
            $rundata->{data}->{elapsed} = $elapsed;

            # add iteration time (if present)
            if (@$itertimes > 0) {
                my @iterdata;

                for (my $i = 1; $i <= @$itertimes; $i++) {
                    push @iterdata, [$i, $$itertimes[$i-1]];
                }

                $rundata->{extratables}->{itertimes} = {
                    tablename => "iteration_times",
                    columns   => ["iteration", "time"],
                    data      => \@iterdata
                };
            }

            $dbh->do("INSERT INTO runs (execpath, scriptname, interpretertag, repetition) VALUES (?,?,?,?)",
                     undef, $benchpath, $benchname, $interpreters{$interp}, $iter);

            my $run_id = $dbh->sqlite_last_insert_rowid();
            insert_results($dbh, $run_id, "results", $rundata);
            insert_extratables($dbh, $run_id, $rundata->{extratables});

            if (-e $statsfile) {
                unlink($statsfile) or die "Can't delete stats file $statsfile: $!";
            }
        }
    }
}

create_pivot($dbh, "results", 1);

$dbh->disconnect;

### command line help ###

=head1 SYNOPSIS

autobench.pl [options] rscript [rscript...]

=head1 OPTIONS

=over 8

=item B<--help>

prints this help message

=item B<--interpreter> string(=string)

select an interpreter to run (optionally giving it a different tag)

=item B<--database> file

sets the database file name

=item B<--repetitions> count

sets the number of repetitions

=item B<--pivot-only>

Just create the pivot table and exit

=back

=cut
