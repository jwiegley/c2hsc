Converts C API header files to `.hsc` and `.hsc.helper.c` files

    Usage: c2hsc <Lib Prefix> [cpp options...] <FILE>.h

This will create `<FILE>.hsc`, and `<FILE>.hsc.helper.c` if the header file
contains inline functions.

For example, in `hlibgit2` on the Mac I'm using:

    c2hsc Bindings.Libgit2 -U__BLOCKS__ libgit2/include/git2/tree.h

Known issues:

 - Varargs functions do not translate
 - Regular "unsigned" does not translate
 - Arrays are not handled at all (use #array_field $name , $type)
 - Handle type synonyms
 - A function taking void shouldn't have any arguments
 - Encode const char * as CString
 - Encode function pointers using FunPtr
 - Unnamed enums are not being emitted
 - Array parameters, like char x[] are not converted

Also, please note that this tool will never be 100% accurate.  It cannot
translate macros, or anything related to the preprocessor, for example.  It
often misses necessary `#include` files, and will get them wrong in any case
if preprocessor conditionals are involved.

The goal of `c2hsc` is to solve the hardest 80% of the problem of creating an
FFI library.  The remaining 20%, plus validation of the results, is an
exercise necessarily left to the user.
