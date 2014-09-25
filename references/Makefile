all: gb2fa bwa-index

gb2fa: TRA@.gb TRB@.gb TRG@.gb
	../scripts/gb2fa.bash

bwa-index: V-C.fa
	ln -s ../V-C.fa V-C/index
	bwa index V-C/index
	rm V-C/index

degap: V-C.fa V_after_C.fa J_before_FGxG.fa J-FGxG.fa
	for FA in V-C.fa V_after_C.fa J_before_FGxG.fa J-FGxG.fa ; do \
	  degapseq --filter $$FA $$FA.degapped && mv $$FA.degapped $$FA ; done

refresh-data:
	R CMD BATCH --no-save V_after_C.R
	R CMD BATCH --no-save J_before_FGxG.R

clean:
	@rm --force --verbose V-C/index.* V_after_C.Rout J_before_FGxG.Rout
