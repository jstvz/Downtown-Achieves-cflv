# cache and figure directories
CACHEDIR= cache
FIGUREDIR= figures

nv-report-card-data_self.html: nv-report-card-data.md nv-report-card-data.html
	pandoc  --template templates/template.html --css templates/template.css --self-contained --highlight-style tango --mathjax -w html5  --toc --toc-depth 3 -o $@ $<

# extract an R file from an RNoWeb file
nv-report-card-data.R:  nv-report-card-data.Rmd
	Rscript\
		-e "require(knitr)" \
		-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
		-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
		-e "knitr::purl('$<', '$@')"

nv-report-card-data.tex: nv-report-card-data.md templates/report_template.tex 
	pandoc -N --template=templates/report_template.tex --biblio templates/references.bib \
	--csl templates/genetics.csl --highlight-style tango --latex-engine=xelatex --toc \
	--variable mainfont="Adobe Caslon Pro" --variable sansfont="Myriad Pro" \
	--variable monofont=Consolas --variable fontsize=12pt -s -o $@ $<

nv-report-card-data.pdf: nv-report-card-data.md templates/report_template.tex 
	pandoc -N --template=templates/report_template.tex --biblio templates/references.bib \
	--csl templates/genetics.csl --highlight-style tango --latex-engine=xelatex --toc \
	--variable mainfont="Adobe Caslon Pro" --variable sansfont="Myriad Pro" \
	--variable monofont=Consolas --variable fontsize=12pt -s -o $@ $<
