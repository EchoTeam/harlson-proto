R = ./rebar

.PHONY: all
all: compile

.PHONY: clean
clean:
	$(R) clean

.PHONY: compile
compile:
	$(R) compile

.PHONY: eunit
eunit:
	$(R) eunit -v skip_deps=true

# Dialyzer
.PHONY: build_plt
build_plt: erl_std.plt

erl_std.plt:
	dialyzer --build_plt --apps erts kernel stdlib crypto --output_plt erl_std.plt

.PHONY: dialyzer
dialyzer: build_plt
	dialyzer src/*.erl --plts erl_std.plt
