#!/usr/bin/perl -w

# This script generates foo.hs from foo.hs-template
# First argument is input file; second argument is output file.

use strict;
use Cwd;

my $useLineDirectives = 1;

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
$typeGroups{'floating'}  = [qw (Half Float Double)];
$typeGroups{'vectors'}   = [qw (V2 V3 V4)];

my %floatingSize = (
    "Half"   => 16,
    "Float"  => 32,
    "Double" => 64
    );

my %knownTypes = ();
foreach my $group (sort keys %typeGroups) {
    my $types = $typeGroups{$group};

    foreach my $type (@$types) {
        $knownTypes{$type} = 1;
    }
}

my @input  = ();
my @output = ();

sub pushLine {
    my $line = $_[0];

    # If printing {-# LINE #-} directives, suppress two consecutive
    # directives with the same line number.
    #
    # If not printing {-# LINE #-} directives, suppress two
    # consecutive blank lines.

    my $suppress = 0;
    my $prevLine = undef;
    $prevLine    = $output[$#output] unless ($#output < 0);

    if (defined $prevLine and $prevLine eq $line) {
        if ($useLineDirectives) {
            $suppress = 1 if ($line =~ /^\{-# LINE/);
        } else {
            $suppress = 1 if ($line eq "");
        }
    }

    push @output, $line unless ($suppress);
}

sub lineDirective {
    my $lineNo = $_[0];

    my $n = $lineNo + 1;
    my $ln = qq[{-# LINE $n "$templateRel" #-}];
    pushLine ($ln) if ($useLineDirectives);
}

my $uniqueCounter = 1000;

sub doTemplate {
    my ($beginLine, $endLine) = @_;
    my @types                 = ();

    my $header = $input[$beginLine];
    if ($header =~ /^--FOR\s+(\w[\w\s,]+)$/) {
        my $groups = $1;

        foreach my $group (split (/,\s*/, $groups)) {
            my $addTypes = undef;
            my $what     = undef;

            if ($group =~ /^[A-Z]/) {
                $what     = "type";
                $addTypes = [$group] if (exists $knownTypes{$group});
            } else {
                $what     = "group";
                $addTypes = $typeGroups{$group};
            }

            if (not defined $addTypes) {
                my $errLine = $beginLine + 1; # report 1-based line number
                die "line $errLine: unrecognized $what $group\n";
            }

            foreach my $type (@$addTypes) {
                push @types, $type;
            }
        }
    } else {
        my $errLine = $beginLine + 1; # report 1-based line number
        die "line $errLine: can't parse FOR directive\n";
    }

    my ($bar, $comma) = ("   ", "     ");

    foreach my $type (@types) {
        my $ltype  = lc ($type);
        my $typexx = sprintf ("%-6s", $type);
        my $name   = $type;
        $name      =~ s/Word/Unt/;
        my $namexx = sprintf ("%-6s", $name);
        my $vtype  = $type;
        $vtype     =~ s/^V(\d)$/Vt$1/;
        my $uniq   = sprintf ("%04d", $uniqueCounter++);

        my ($signed, $bits) = ("Unknown", "undefined");
        $signed = "Signed"   if ($type =~ /^Int/);
        $signed = "Unsigned" if ($type =~ /^Word/);
        $signed = "Vector"   if ($type =~ /^V\d$/);
        $bits   = $1         if ($type =~ /(\d+)/);
        if (exists $floatingSize{$type}) {
            $signed = "Float";
            $bits   = $floatingSize{$type};
        }
        my $lsigned  = lc ($signed);
        my $signedxx = sprintf ("%-8s", $signed);

        my $begLine = $beginLine + 1;
        lineDirective ($begLine);

        for (my $lineNo = $begLine; $lineNo < $endLine; $lineNo++) {
            my $line = $input[$lineNo];

            $line =~ s/TYPEXX/$typexx/g;
            $line =~ s/NAMEXX/$namexx/g;
            $line =~ s/LTYPE/$ltype/g;
            $line =~ s/VTYPE/$vtype/g;
            $line =~ s/NAME/$name/g;
            $line =~ s/TYPE/$type/g;

            $line =~ s/UNIQ/$uniq/g;
            $line =~ s/BAR/$bar/g;
            $line =~ s/COMMA/$comma/g;

            $line =~ s/SIGNEDXX/$signedxx/g;
            $line =~ s/LSIGNED/$lsigned/g;
            $line =~ s/SIGNED/$signed/g;
            $line =~ s/BITS/$bits/g;

            pushLine ($line);
        }

        $bar   = " | ";
        $comma = ",    ";
    }
}

open F, "<", $templateFull or die;

while (<F>) {
    chomp;
    push @input, $_;
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

lineDirective (0);

for (my $lineNo = 0; $lineNo <= $#input; $lineNo++) {
    my $line = $input[$lineNo];

    if ($line =~ /^--FOR/ or $line =~ /^--END/) {
        if (defined $beginLine) {
            doTemplate ($beginLine, $lineNo);
            $beginLine = undef;
            lineDirective ($lineNo + 1);
        }
    }

    if ($line =~ /^--FOR/) {
        $beginLine = $lineNo;
    } elsif ($line !~ /^--END/ and not defined $beginLine) {
        pushLine ($line);
    }
}

die "$0: unterminated FOR loop in $templateRel\n" if (defined $beginLine);

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
