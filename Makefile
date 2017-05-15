all: contmem.out

contmem.out: contmem.scm
	bigloo $^ -o $@

clean:
	rm -rf *.out *.o *~
