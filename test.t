  $ type_driven_search
  Usage: type_driven_search <command>
  Where command is one of { help; explain; index }
  help: print this help message
  explain <signature>: explains the C function <signature>
  index [opts] { create; get; store; serve }: store and retrieve functions by signature
  |-- create <index>: initialize an empty index into the <index> file
  |-- store <index> <name> <signature>: stores the function <name> with the given <signature> into <index>
  |-- get <index> <query>: list all functions stored within <index> matching <query>
  |-- serve <index>: enter an interactive mode waiting for queries on the standard input
  | --index=<index-id>: choose the indexing method where <index-id> is one of { FileBased (default); FileBasedSorted }
  [1]

Explain  
  $ type_driven_search explain "oops("
  Invalid C/C++ signature
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
  $ type_driven_search index --index="FileBasedSorted" get index.txt "int (int,int)" | sort
  int add(int, int)
  int mul(int, int)
  $ rm index.txt

  $ type_driven_search index --index="Oops" create index.txt
  Invalid index type: 'Oops'
  [2]
  $ ls
