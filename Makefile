.PHONY: default build fmt test test-promote benchmark

# If INSTALL_ROOT is in your PATH, so will be the installed executable
INSTALL_ROOT=~/bin

default: fmt build test

# Build the executable then copy it under INSTALL_ROOT
install: fmt build test
	# Copy the executable into installation directory
	cp _build/install/default/bin/type_driven_search $(INSTALL_ROOT)/type_driven_search
	# Make the installed file writable to allow future deletion or replacement
	chmod +w $(INSTALL_ROOT)/type_driven_search

build:
	dune build
	dune build @doc

# Format sources, formatting can be configured in .ocamlformat
fmt:
	-dune fmt

test:
	dune test

# Update expected test results with whatever the current output is, run this once you are ok with the current behaviour
test-promote:
	dune test --auto-promote

#
benchmark:
	echo "" >> stats.md
	echo "## Last benchmark" >> stats.md
	echo "" >> stats.md
	dune exec bin/benchmark.exe >> stats.md

docker-build: Dockerfile
	docker build -t tds .

docker-shell:
	docker run -it --rm --entrypoint bash -v "$$(pwd)/test_resources:/app/mount" tds

clean:
	-rm temp*.txt
	-rm -rf report/*
