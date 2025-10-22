##
# Jolly Brancher
#
# @file
# @version 0.1

.PHONY: build docs clean publish.test publish.pypi deploy

build:
	tox -r -e build

docs:
	tox -e docs

clean:
	rm -rf ./dist/*

publish.test:
	tox -e publish

publish.pypi:
	tox -e publish -- --repository pypi

deploy:
	make clean; make docs && make build && make publish.pypi
# end
