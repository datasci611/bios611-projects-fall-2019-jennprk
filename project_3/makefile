build-python: Dockerfile.dpython
        docker build -t dockerpy . -f Dockerfile.dpython && \
        touch $@

analytic_first.tsv: build-python
        docker run -it --rm -v $(shell pwd):/workdir -w /workdir dockerpy\
        python /scripts/DataWrangling.py

build-r: Dockerfile.rtidyverse
        docker build -t dockerrtidy . -f Dockerfile.rtidyverse && \
        touch $@

Project-3-Report.html: build-r .csv build-python
        docker run -it --rm -v $(shell pwd):/workdir -
