all: gb2fa bwa-index

gb2fa: TRA@.gb TRB@.gb TRG@.gb
	../scripts/gb2fa.bash

bwa-index: V-C.fa
	ln -s ../V-C.fa V/index
	bwa index V/index
	rm V/index

clean:
	@rm --force --verbose V/index.*
