ECL=ecl --norc
current_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
CFLAGS=-Ofast

define ENV
env C_INCLUDE_PATH="$$C_INCLUDE_PATH:${HOME}/.linuxbrew/include:$(current_dir)" \
	LIBRARY_PATH="${HOME}/.linuxbrew/lib" \
	CFLAGS="$(CFLAGS)"
endef

bin/crbk: bin/crbk.o bin/data-bindings.o bin/data.o
	$(ENV) $(ECL) --shell main-build.lisp

bin/crbk.o: src/crbk.lisp
	$(ENV) $(ECL) --shell compile-file.lisp -- -if $^ -of $@

bin/data-bindings.o: src/data-bindings.lisp
	$(ENV) $(ECL) --shell compile-file.lisp -- -if $^ -of $@

bin/%.o: src/%.c
	gcc $(CFLAGS) -c -o $@ $^

clean:
	rm bin/*

testenv:
	@echo $(ENV)
