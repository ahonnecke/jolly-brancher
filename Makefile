##
# Jolly Brancher
#
# @file
# @version 0.1

build:
	tox -e build

publish.test:
	tox -e publish

publish.pypi:
	tox -e publish -- --repository pypi

deploy:
	build publish.pypi
# end
