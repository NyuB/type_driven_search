  $ type_driven_search
  Usage: type_driven_search <command>
  Where command is one of { help, explain }
  help: print this help message
  explain <signature>: explains the C function <signature>
  [1]
  $ type_driven_search explain "oops("
  Invalid C/C++ signature
  $ type_driven_search explain "int (char const**, int)"
  a function returning an int from (a pointer to a pointer to an immutable char, an int)
  $ type_driven_search explain "void (int*const***const)"
  a function returning a void from (an immutable pointer to a pointer to a pointer to an immutable pointer to an int)
  $ type_driven_search explain "void (unsigned int*)"
  a function returning a void from (a pointer to an unsigned int)
