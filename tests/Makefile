
FILES=\
	hello.arm \
	hello.x86_64 \
	verifyPIN_A.axf \
	add.arm \
	add.x86_64 \
	calls.x86_64

all: tests

tests: $(FILES)
	for x in $(FILES); do \
		../wab $$x > $$x.out 2> $$x.err; \
	done

.PHONY: clean
clean:
	@rm -f *~
