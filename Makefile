ECL=ecl --norc
current_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
CFLAGS=-Ofast

LISPFILES := data-bindings io sodium crbk
LISPOBJECTS := $(addprefix bin/, $(LISPFILES:=.obj))

CFILES := data
COBJECTS := $(addprefix bin/, $(CFILES:=.o))

define ENV
env C_INCLUDE_PATH="$$C_INCLUDE_PATH:${HOME}/.linuxbrew/include:$(current_dir)" \
	LIBRARY_PATH="${HOME}/.linuxbrew/lib" \
	CFLAGS="$(CFLAGS)"
endef

bin: FORCE
	mkdir -p bin
	make bin/crbk

bin/crbk: $(LISPOBJECTS) $(COBJECTS)
	$(ENV) $(ECL) --shell main-build.lisp -- -of $@ -if $^

bin/%.obj: src/%.lisp
	$(ENV) $(ECL) --shell compile-file.lisp -- -if $^ -of $@

bin/%.o: src/%.c
	gcc $(CFLAGS) -c -o $@ $^

clean:
	rm -f $(COBJECTS) $(LISPOBJECTS)

FORCE: ;
