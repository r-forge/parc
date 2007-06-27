MPI = /opt/SUNWhpc/bin/
mul: mul.o
	$(MPI)mpcc -g -xs -o mul mul.o -L/opt/SUNWhpc/lib -R/opt/SUNWhpc/lib -lmpi -lm

mul.o: mul.c mul.h
	$(MPI)mpcc -xs -DFORTRANUNDERSCORE -DHAVE_STDLIB_H=1 -g -I/opt/SUNWhpc/include -DMPI_solaris -w -c mul.c

# A -W option may be used to wrap processes on processors.  It won't help
# for this one the way the code is written!
run: mul
	$(MPI)mprun -np 12 mul

clean:
	rm -r mul mul.o

