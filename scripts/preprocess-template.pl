#!/usr/bin/perl -w

# This script generates foo.hs from foo.hs-template
# First argument is input file; second argument is output file.

use strict;
use Cwd;

die "Usage: $0 infile.hs-template outfile.hs\n" if (scalar (@ARGV) != 2);

my ($templateFull, $outputFull) = @ARGV;

my $cwd = getcwd();

my $scriptRel    = $0;
my $templateRel  = $templateFull;
my $outputRel    = $outputFull;

$outputRel =~ s%^\Q$cwd\E/?%%;

my %typeGroups = ();
$typeGroups{'sizedInt'} =
    [qw(Int8 Int16 Int32 Int64 Word8 Word16 Word32 Word64)];
$typeGroups{'nativeInt'} = [qw (Int Word)];
$typeGroups{'floating'}  = [qw (Float Double)];
$typeGroups{'vectors'}   = [qw (V2 V3 V4)];

my @input  = ();
my @output = ();

sub pushLine {
    my $line = $_[0];

    my $prevBlank = ($#output < 0 or $output[$#output] eq "");
    push @output, $line unless ($line eq "" and $prevBlank);
}

sub doTemplate {
    my ($beginLine, $endLine) = @_;
    my @types                 = ();

    my $header = $input[$beginLine];
    if ($header =~ /^--FOR\s+(\w[\w\s,]+)$/) {
        my $groups = $1;

        foreach my $group (split (/,\s*/, $groups)) {
            my $addGroups = $typeGroups{$group};

            if (not defined $addGroups) {
                my $errLine = $beginLine + 1; # report 1-based line number
                die "line $errLine: unrecognized group $group\n";
            }

            foreach my $type (@$addGroups) {
                push @types, $type;
            }
        }
    } else {
        my $errLine = $beginLine + 1; # report 1-based line number
        die "line $errLine: can't parse FOR directive\n";
    }

    foreach my $type (@types) {
        my $ltype  = lc ($type);
        my $name   = $type;
        $name      =~ s/Word/Unt/;
        my $namexx = sprintf ("%-6s", $name);
        my $vtype  = $type;
        $vtype     =~ s/^V(\d)$/Vt$1/;

        for (my $lineNo = $beginLine + 1; $lineNo < $endLine; $lineNo++) {
            my $line = $input[$lineNo];

            $line =~ s/NAMEXX/$namexx/g;
            $line =~ s/LTYPE/$ltype/g;
            $line =~ s/VTYPE/$vtype/g;
            $line =~ s/NAME/$name/g;
            $line =~ s/TYPE/$type/g;

            pushLine ($line);
        }
    }
}

open F, "<", $templateFull or die;

my $skip = 1;

while (<F>) {
    chomp;
    $skip = 0 if (/^\{-# LANGUAGE/ or /^module/);
    push @input, $_ if (not $skip);
}

close F;

while (<DATA>) {
    chomp;

    s/TEMPLATE/$templateRel/g;
    s/SCRIPT/$scriptRel/g;
    s/OUTPUT/$outputRel/g;

    pushLine ($_);
}

pushLine ("");

my $beginLine = undef;

for (my $lineNo = 0; $lineNo <= $#input; $lineNo++) {
    my $line = $input[$lineNo];

    if ($line =~ /^--FOR/ or $line =~ /^--END/) {
        if (defined $beginLine) {
            doTemplate ($beginLine, $lineNo);
            $beginLine = undef;
        }
    }

    if ($line =~ /^--FOR/) {
        $beginLine = $lineNo;
    } elsif ($line !~ /^--END/ and not defined $beginLine) {
        pushLine ($line);
    }
}

pop @output if ($output[$#output] eq "");

open F, ">", $outputFull or die;

foreach my $line (@output) {
    print F $line, "\n";
}

close F;

__DATA__
{- GENERATED FILE - DO NOT EDIT

   To modify, edit the template file.  Cabal should automatically
   regenerate this file.

   template: TEMPLATE
     script: SCRIPT
     output: OUTPUT
-}
