PARTS = pedigree.Rnw \
	kinship.Rnw \
	align.Rnw \
	align2.Rnw \
	plot.Rnw \
	pedigree.shrink.Rnw \
	checks.Rnw

SFUN =  align.pedigree.R\
	alignped1.R \
	alignped2.R \
	alignped3.R \
	alignped4.R \
	autohint.R \
	check.hint.R \
	familycheck.R \
	kinship.R \
	kindepth.R \
	makekinship.R \
	pedigree.R \
	plot.pedigree.R \
	pedigree.legend.R \
	print.pedigree.R \
	as.data.frame.pedigree.R \
	ped2df.R \
	pedigree.shrink.R \
	bitSize.R \
	pedigree.trim.R \
	findUnavailable.R \
	findAvailNonInform.R \
	findAvailAffected.R \
	print.pedigree.shrink.R \
	pedigree.shrink.minor.R \
	plot.pedigree.shrink.R \
	printBanner.R \
	pedigree.unrelated.R


RDIR = ../R

all.pdf: all.tex noweb.sty
	pdflatex all


all.nw: $(PARTS)
	cat $(PARTS) > all.nw
	echo "\\end{document}" >> all.nw

all.tex: all.nw
	echo "library(noweb); noweave('all.nw')" | R  --slave

RFUN = $(SFUN:%=$(RDIR)/%)

lfun: $(SFUN)
fun:  all.nw $(RFUN) 

$(SFUN): all.nw
$(RFUN): all.nw

%.R: 
	echo "# Automatically generated from all.nw using noweb" > $@
	echo "source('noweb.R'); print(notangle(nwread('all.nw'), target='$(*F)'), file='zz')" | R --vanilla --slave
	cat zz >> $@
	rm zz
#	notangle -R$(*F) all.nw >> $@

clean:
	-rm all.nw all.log all.aux all.toc all.tex 
	-rm $(RFUN)

noweb.sty:
	echo 'library(noweb); data(noweb); cat(noweb.sty, sep="\n", file="noweb.sty")' |  R  --slave

