CFLAGS=-Wall -g

#ARCH_OBJS=i386.o linux.o
ARCH_OBJS=ppc.o linux.o

teststack: teststack.o $(ARCH_OBJS)
	$(CC) -o $@ $< $(ARCH_OBJS)

ppc.o: ppc.S
	$(CC) -c -o $@ -g $<

clean:
	$(RM) -f *.o *~ teststack

