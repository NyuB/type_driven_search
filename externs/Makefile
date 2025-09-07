CC=gcc
CFLAGS=-Wall -Wextra -Werror

fs.o: fs.c fs.h
	$(CC) $(CFLAGS) -c -o fs.o fs.c

libfs.a: fs.o
	ar -r libfs.a fs.o

libfs.so: fs.o
	$(CC) $(CFLAGS) -shared -o libfs.so fs.o

fs_test.o: fs_test.c fs.h
	$(CC) $(CFLAGS) -c -o fs_test.o fs_test.c

fs_test: fs.a fs_test.c
	$(CC) $(CFLAGS) -o fs_test fs_test.c libfs.a

.PHONY: test clean
test: fs_test
	echo "A" > a
	-./fs_test a b
	-cat b
	-rm b

SILENT_ERROR=2> /dev/null
clean:
	@-rm *.o $(SILENT_ERROR)
	@-rm *.a $(SILENT_ERROR)
	@-rm *.so $(SILENT_ERROR)
	@-rm fs_test $(SILENT_ERROR)
