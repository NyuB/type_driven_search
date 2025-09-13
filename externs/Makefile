CC=gcc
CFLAGS=-Wall -Wextra -Werror

fmt:
	clang-format *.c *.h

fs.o: fs.c fs.h
	$(CC) $(CFLAGS) -c -o $@ fs.c

lib%.a: %.o
	ar -r $@ $<

lib%.so: %.o
	$(CC) $(CFLAGS) -shared -o $@ $<

libcamlite.a: camlite.o sqlite3/sqlite3.o
	ar -r $@ $^

libcamlite.so: camlite.o sqlite3/sqlite3.o
	$(CC) $(CFLAGS) -shared -o $@ $^

fs_test.o: fs_test.c fs.h
	$(CC) $(CFLAGS) -c -o $@.o fs_test.c

fs_test: fs.a fs_test.c
	$(CC) $(CFLAGS) -o $@ fs_test.c libfs.a

.PHONY: fs_test_run sqlite_test_run db-shell db-query clean
fs_test_run: fs_test
	echo "A" > a
	-./fs_test a b
	-cat b
	-rm b

OCAMLC_WHERE=$(shell ocamlc -where)
camlite.o: camlite.c
	$(CC) $(CFLAGS) -I$(OCAMLC_WHERE) -c -o $@ camlite.c

compile_commands.json: compile_commands.in.json
	cp compile_commands.in.json $@
	sed --i 's#@@CURDIR@@#$(CURDIR)#g' $@
	sed --i 's#@@CC@@#$(CC)#g' $@
	sed --i 's#@@OCAMLC_WHERE@@#$(OCAMLC_WHERE)#g' $@

sqlite/sqlite3.c:
	echo "Download sqlite3 from the official release page"
	exit 1
sqlite/shell.c:
	echo "Download sqlite3 from the official release page"
	exit 1
sqlite/sqlite3.h:
	echo "Download sqlite3 from the official release page"
	exit 1

sqlite_test.o: sqlite_test.c sqlite3/sqlite3.h
	$(CC) $(CFLAGS) -c -o $@ sqlite_test.c

sqlite3/sqlite3.o: sqlite3/sqlite3.c sqlite3/sqlite3.h
	$(CC) $(CFLAGS) -fPIC -c -o $@ sqlite3/sqlite3.c

sqlite_test: sqlite_test.o sqlite3/sqlite3.o
	$(CC) $(CFLAGS) -o $@ sqlite3/sqlite3.o sqlite_test.o

LINK_MATH=-lm
sqlite3/shell: sqlite3/sqlite3.o sqlite3/shell.c
	$(CC) $(CFLAGS) -o $@ sqlite3/sqlite3.o sqlite3/shell.c $(LINK_MATH)

db.sqlite: sqlite3/shell sql/db_init.sql
	echo .exit | sqlite3/shell db.sqlite -init sql/db_init.sql

sqlite_test_run: sqlite_test db.sqlite
	./sqlite_test db.sqlite

db-shell: db.sqlite
	sqlite3/shell db.sqlite

db-query:
	echo ".read ./sql/query.sql" | sqlite3/shell db.sqlite

SILENT_ERROR=2> /dev/null
clean:
	@-rm *.o $(SILENT_ERROR)
	@-rm *.a $(SILENT_ERROR)
	@-rm *.so $(SILENT_ERROR)
	@-rm *.sqlite $(SILENT_ERROR)
	@-rm fs_test $(SILENT_ERROR)
	@-rm sqlite3/shell $(SILENT_ERROR)
	@-rm sqlite3/*.o $(SILENT_ERROR)
