
SRC= Makefile mow-dokumentacja2.tex bibliografia.bib

# pdf, ps, gs, gs2, dvi
default: t.ps

pdf: t.pdf
	echo "Making DVI to PDF"

ps: t.ps

gs: t.ps
	gv -scale -2 -geometry +0+0 mow-dokumentacja2.ps

gs2: t.ps
	psnup -2 t.ps > tt.ps
	gv -landscape -scale -1 -geometry +0+0 tt.ps

dvi: t.dvi
	@xdvi + -s 4 -topmargin 6cm -bg white -keep -geometry 715x724+0+0 mow-dokumentacja2

ed:
	vim mow-dokumentacja2.tex

t: default


t.dvi: $(SRC)
	latex mow-dokumentacja2
	latex mow-dokumentacja2
	bibtex mow-dokumentacja2
	latex mow-dokumentacja2 
	latex mow-dokumentacja2

t.pdf: t.dvi
	dvipdf t.dvi

t.ps: t.dvi
	dvips mow-dokumentacja2.dvi -o mow-dokumentacja2.ps

tgz: clean
	( D=`basename $$PWD`; cd ..; tar czf $$D.tgz $$D; ls -l $$D.tgz )

aspell:
	aspell --lang=pl  -t check mow-dokumentacja2.tex
	
v: t t.ps

clean:
	rm -f *.dvi *.bbl *.blg *.log *.toc *.aux *.ps *.lof *.pdf

