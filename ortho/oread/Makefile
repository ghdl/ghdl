BE = gcc
ortho_srcdir=..
BACK_END=$(ortho_srcdir)/$(BE)
ortho_exec=oread-$(BE)
all: $(ortho_exec)

test: test.s
	$(CC) -o $@ $^

test.s: $(ortho_exec)
	./$(ortho_exec) test

$(ortho_exec): force
	$(MAKE) -f $(BACK_END)/Makefile ortho_exec=$(ortho_exec)

clean:
	$(MAKE) -f $(BACK_END)/Makefile clean
	$(RM) -f oread *.o *~

distclean: clean
	$(MAKE) -f $(BACK_END)/Makefile distclean

force:

.PHONY: force
