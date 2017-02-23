######################################################################
###   makefile for the docu                                        ###
###                                                                ###
###   just to start somehow...                                     ###
###   please change/modify as you like (mn)                        ###
###                                                                ###
###                                                                ###
###   type "make" and it generates the pdf file with pdflatex      ###
###   and deletes all log files                                    ###
###                                                                ###
######################################################################

######################################################################
FILE =  user_guide_v1.0
TEX  =  insar.tex io.tex sar.tex subap.tex user_guide_v1.0.tex \
	polinsar.tex intro.tex devel.tex
BIB  =  rat_references.bib
######################################################################
PDF  = xpdf
DVI  = xdvi -s 7
######################################################################

all: ${FILE}.pdf clean
	${PDF} ${FILE}.pdf &

dvi: ${FILE}.dvi
	${DVI} ${FILE} &

ps: ${FILE}.ps
	gv ${FILE}.ps &

pdf: ${FILE}.pdf
	${PDF} ${FILE}.pdf &

pdfeps: ${FILE}.ps
	ps2pdf ${FILE}.ps ${FILE}.pdf
	${PDF} ${FILE}.pdf &

acro: ${FILE}.pdf
	acroread ${FILE}.pdf &

# remake without output
re: ${FILE}.pdf clean

# Edit in Emacs
edit: 
	emacs ${FILE}.tex &
e: 
	emacs ${FILE}.tex &

${FILE}.dvi: ${TEX} ${BIB}
	latex ${FILE}
	bibtex ${FILE}
	latex ${FILE}
	latex ${FILE}

${FILE}.ps: ${FILE}.dvi
	dvips -o ${FILE}.ps ${FILE}.dvi

${FILE}.pdf: ${TEX} ${BIB}
	pdflatex ${FILE}.tex
	bibtex ${FILE}
	pdflatex ${FILE}.tex
	pdflatex ${FILE}.tex

# delete unwanted files
clean:
	- rm -f *~
	- rm -f *.aux *.log
	- rm -f *.bbl *.blg
	- rm -fr auto &> /dev/null
	- rm -f *.lof *.lot *.toc

mrproper: clean
	- rm -f  *.dvi *.ps *.pdf
