
FILES=\
	hello.arm \
	hello.x86_64 \
	verifyPIN_A.axf

all: tests

tests: $(FILES)
	for x in $(FILES); do \
		../obaf $$x > $$x.out 2> $$x.err; \
	done

.PHONY: clean
clean:
	@rm -f *~
