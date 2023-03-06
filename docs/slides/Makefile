slides.pdf: slides.tex
	pdflatex --shell-escape $<

slides.tex: slides.lhs
	lhs2TeX -o $@ $<

.PHONY: clean
clean:
	rm -f *.aux *.log *.nav *.out *.ptb *.snm *.toc *.xdv *.fls slides.tex
