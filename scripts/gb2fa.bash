#!/bin/bash

extract () {
	FILE_INPUT=$1
	SEGMENT=$2
	FILE_OUTPUT=$3

	extractfeat \
	   $FILE_INPUT \
	   -filter \
	   -join TRUE \
	   -tag 'standard_name' \
	   -value "$SEGMENT" \
	   -describe standard_name \
	   -osformat2 fasta |
	perl -pe '
	     next unless />/ ;
	     /standard_name="(.+?)"/ and $_ = ">$1\n"' \
	> $FILE_OUTPUT
}

# V
extract	TRA@.gb 'TRAV*' TRAV.fa
extract	TRB@.gb 'TRBV*' TRBV.fa
extract	TRA@.gb 'TRDV*' TRDV.fa
extract	TRG@.gb 'TRGV*' TRGV.fa

# (D)
extract	TRB@.gb 'TRBD*' TRBD.fa
extract	TRG@.gb 'TRGD*' TRGD.fa

# J
extract	TRA@.gb 'TRAJ*' TRAJ.fa
extract	TRB@.gb 'TRBJ*' TRBJ.fa
extract	TRA@.gb 'TRDJ*' TRDJ.fa
extract	TRG@.gb 'TRGJ*' TRGJ.fa

# C
extract TRA@.gb 'TRAC*' TRAC.fa
extract TRB@.gb 'TRBC*' TRBC.fa
extract TRA@.gb 'TRDC*' TRDC.fa
extract TRG@.gb 'TRGC*' TRGC.fa
