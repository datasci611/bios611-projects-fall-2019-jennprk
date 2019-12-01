
.PHONY: project3

project3:
	docker build -t project3 .
	python DataWrangling.py
	Rscript -e "library(rmarkdown); rmarkdown::render('project3script.Rmd', 'html_document')" && mv project3script.html ../results
