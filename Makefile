##
# Jolly Brancher
#
# @file
# @version 0.1

build:
	tox -e build

docs:
	tox -e docs

publish.test:
	tox -e publish

publish.pypi:
	tox -e publish -- --repository pypi

deploy:
	make docs && make build && make publish.pypi
# end
