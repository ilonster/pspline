#!/usr/bin/perl
#! convert F77NAME function name to upper case
#!
#! USAGE:  perl F77UP.pl file.c > out.c
#! 07/15/99 C.Ludescher
#!
$file = $ARGV[$#ARGV];

open(FILE, $file) ||  die "Can't open $file\n\n";

#
while(<FILE>){
    s/F77NAME(\([a-zA-Z0-9_ ]+)\)*/F77NAME\U$1)/g;
    print;
}
