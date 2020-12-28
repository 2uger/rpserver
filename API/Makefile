PROJECT_NAME ?= rpserver_API
VERSION = $(shell python3 setup.py --version)

all:
	@echo "make createvenv		- Create virtual environment"
	@echo "make dev-env		- Install requirements.dev.txt"
	@echo "make test		- Run ALL test"
	@exit 0

creatvenv:
	rm -rf env
	python3 -m venv .env

dev-env:
	.env/bin/pip install -Ue '.[dev]'

test:
	python3 test/run_test.py
