#!/bin/bash 

LIBRARY=$1
V_LIB=${2-V}
J_LIB=${3-J}

REFDIR=../../../references

VS_MISMATCH=${VS_MISMATCH-0}
MAPQ_CUTOFF=${MAPQ_CUTOFF-1}

AS_CUTOFF=${AS_CUTOFF-170}
#AS_CUTOFF=50 #for the TRAV6-6-TRAJ52 Sanger sequences.

# ${V_NAME//\//} will remove all slashes in V_NAME.
# samtools idxstats allows to find which V segments were detected.

rm -rf extraction_files/$LIBRARY.tmp ; mkdir extraction_files/$LIBRARY.tmp
rm -f clonotypes/$LIBRARY.tsv
cd extraction_files/$LIBRARY.tmp

for V_NAME in $(samtools idxstats ../$LIBRARY.bam | awk '{if ($3 > 0) print $1}')
do
  echo -ne "$V_NAME\t" 1>&2
  V_SEQ=$(seqret -filter $REFDIR/${V_LIB}-C.fa:$V_NAME[-20:] raw:stdout)
  echo -ne "$V_SEQ\t" 1>&2
  samtools view -q $MAPQ_CUTOFF ../$LIBRARY.bam $V_NAME |
    perl -ne "(\$score) = /AS:i:(\d+)\t/ ; print if \$score >= $AS_CUTOFF" > $LIBRARY-${V_NAME//\//}-filtered.sam
  [ -s $LIBRARY-${V_NAME//\//}-filtered.sam ] && vectorstrip -auto \
        -readfile N \
        -mismatch $VS_MISMATCH \
        -alinker $V_SEQ \
        -sequence sam::$LIBRARY-${V_NAME//\//}-filtered.sam \
        -outseq sam::stdout \
        -outfile $LIBRARY.${V_NAME//\//}.vectorstrip |
    tee $LIBRARY.${V_NAME//\//}-found.sam |
    grep -v ^@ | wc -l - | cut -f1 -d' ' 1>&2
  [ -s $LIBRARY.${V_NAME//\//}-found.sam ] && for J_NAME in $(infoseq -filter $REFDIR/${J_LIB}-FGxG.fa -only -name -heading N)
  do
    echo -ne "\t$J_NAME\t" 1>&2
    J_SEQ=$(seqret -filter $REFDIR/${J_LIB}-FGxG.fa:$J_NAME[:20] raw:stdout)
    echo -ne "\t$J_SEQ\t" 1>&2
    vectorstrip -auto \
        -readfile N \
        -mismatch $VS_MISMATCH \
        -blinker $(seqret -filter $REFDIR/${J_LIB}-FGxG.fa:$J_NAME[:20] raw:stdout) \
        -sequence sam::$LIBRARY.${V_NAME//\//}-found.sam \
        -outseq sam::stdout \
        -outfile $LIBRARY-${V_NAME//\//}.$J_NAME.vectorstrip |
      perl -MBio::Seq -E '
        while (<STDIN>) {
          next if /^@/ ;
	  next if /^$/ ;
          chomp ;
          ($name, $seq, $qual) = (split "\t")[0, 9, 10] ;
          $name =~ s/([.]*)_from.*/$1/ ;
          $trans = new Bio::Seq(-seq => $seq )->translate->seq ;
          say join ("\t", @ARGV[0], @ARGV[1], @ARGV[2], $name, $seq, $qual, $trans) ;
        } ' $LIBRARY $V_NAME $J_NAME |
      tee -a ../../clonotypes/$LIBRARY.tsv |
      wc -l - | cut -f1 -d' ' 1>&2
  done
  echo
done
