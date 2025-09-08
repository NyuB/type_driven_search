  $ type_driven_search
  Usage: type_driven_search <command>
  Where command is one of { help; explain; index }
  help: print this help message
  explain <signature>: explains the C function <signature>
  index [opts] { create; get; store; serve }: store and retrieve functions by signature
  |-- create <index>: initialize an empty index into the <index> file
  |-- store <index> <name> <signature>: stores the function <name> with the given <signature> into <index>
  |-- get <index> <signature>: list all functions stored within <index> having exactly <signature>
  |-- query <index> <query>: list all functions stored within <index> matching <query>
  |-- serve <index>: enter an interactive mode waiting for queries on the standard input
  | --index=<index-id>: choose the indexing method where <index-id> is one of { FileBased (default); FileBasedSorted }
  [1]

Explain  
  $ type_driven_search explain "oops("
  not a valid C/C++ signature
  $ type_driven_search explain "int (char const**, int)"
  a function returning an int from (a pointer to a pointer to an immutable char, an int)
  $ type_driven_search explain "void (int*const***const)"
  a function returning a void from (an immutable pointer to a pointer to a pointer to an immutable pointer to an int)
  $ type_driven_search explain "void (unsigned int*)"
  a function returning a void from (a pointer to an unsigned int)

Index  
  $ type_driven_search index create index.txt
  $ cat index.txt
  $ type_driven_search index store index.txt "main" "int (int,char**)"
  $ type_driven_search index store index.txt "add" "int (int,int)"
  $ type_driven_search index store index.txt "mul" "int (int,int)"
  $ type_driven_search index get index.txt "int (int,int)"
  int add(int, int)
  int mul(int, int)
  $ rm index.txt

  $ type_driven_search index --index="FileBasedSorted" create index.txt
  $ cat index.txt
  0
  $ type_driven_search index --index="FileBasedSorted" store index.txt "main" "int (int,char**)"
  $ type_driven_search index --index="FileBasedSorted" store index.txt "add" "int (int,int)"
  $ type_driven_search index --index="FileBasedSorted" store index.txt "mul" "int (int,int)"
  $ cat index.txt
  3
  main:int (int,char**)                                                                                                                                
  add:int (int,int)                                                                                                                                    
  mul:int (int,int)                                                                                                                                    
  $ type_driven_search index --index="FileBasedSorted" get index.txt "int (int,int)" | sort
  int add(int, int)
  int mul(int, int)
  $ rm index.txt

  $ type_driven_search index --index="FileBasedSorted" create index.txt 
  $ type_driven_search index --index="FileBasedSorted" store index.txt "a" "int ()"
  $ cat index.txt
  1
  a:int ()                                                                                                                                             
  $ type_driven_search index --index="FileBasedSorted" store index.txt "x1" "void ()"
  $ cat index.txt
  2
  a:int ()                                                                                                                                             
  x1:void ()                                                                                                                                           
  $ type_driven_search index --index="FileBasedSorted" store index.txt "b" "int ()"
  $ cat index.txt
  3
  b:int ()                                                                                                                                             
  a:int ()                                                                                                                                             
  x1:void ()                                                                                                                                           
  $ type_driven_search index --index="FileBasedSorted" get index.txt "int ()" | sort
  int a()
  int b()
  $ rm index.txt

Ingest
  $ type_driven_search index --index="FileBasedSorted" ingest --format=c index.txt test_resources/curses.h
  $ type_driven_search index --index="FileBasedSorted" get index.txt "int (int, int, WINDOW*)" | sort
  int mvderwin(WINDOW*, int, int)
  int mvwdelch(WINDOW*, int, int)
  int mvwdeleteln(WINDOW*, int, int)
  int mvwgetch(WINDOW*, int, int)
  int mvwin(WINDOW*, int, int)
  int mvwinsertln(WINDOW*, int, int)
  int touchline(WINDOW*, int, int)
  int wmove(WINDOW*, int, int)
  int wredrawln(WINDOW*, int, int)
  int wresize(WINDOW*, int, int)
  int wsetscrreg(WINDOW*, int, int)

Query
  $ type_driven_search index --index="FileBasedSorted" query index.txt "int(WINDOW*, int, int, int)" 
  int pnoutrefresh(WINDOW*, int, int, int, int, int, int)
  int prefresh(WINDOW*, int, int, int, int, int, int)
  int wtouchln(WINDOW*, int, int, int)
  int mvwhline(WINDOW*, int, int, chtype, int)
  int mvwvline(WINDOW*, int, int, chtype, int)
  int copywin(WINDOW const*, WINDOW*, int, int, int, int, int, int, int)
  int mvwvline_set(WINDOW*, int, int, cchar_t const*, int)
  int mvwhline_set(WINDOW*, int, int, cchar_t const*, int)
  int mvwadd_wchnstr(WINDOW*, int, int, cchar_t const*, int)
  int mvwinsnstr(WINDOW*, int, int, char const*, int)
  int mvwaddnstr(WINDOW*, int, int, char const*, int)
  int mvwaddchnstr(WINDOW*, int, int, chtype const*, int)
  int mvwchgat(WINDOW*, int, int, int, attr_t, short, void const*)
  int mvwins_nwstr(WINDOW*, int, int, wchar_t const*, int)
  int mvwaddnwstr(WINDOW*, int, int, wchar_t const*, int)
  int mvwin_wchnstr(WINDOW*, int, int, cchar_t*, int)
  int mvwinnstr(WINDOW*, int, int, char*, int)
  int mvwgetnstr(WINDOW*, int, int, char*, int)
  int mvwinchnstr(WINDOW*, int, int, chtype*, int)
  int mvwinnwstr(WINDOW*, int, int, wchar_t*, int)
  int mvwgetn_wstr(WINDOW*, int, int, wint_t*, int)
  $ rm index.txt

Error cases
  $ type_driven_search index --index="Oops" create index.txt
  Invalid index type: 'Oops'
  [2]
