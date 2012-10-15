Converts C API header files to `.hsc` and `.hsc.helper.c` files

    Usage: c2hsc --prefix=<Lib Prefix> <FILE>.h

This will create `<FILE>.hsc`, and `<FILE>.hsc.helper.c` if the header file
contains inline functions.

For example, in `hlibgit2` on the Mac I'm using:

    c2hsc --prefix=Bindings.Libgit2 --cppopts=-U__BLOCKS__ \
        libgit2/include/git2/tree.h

Known issues:

 - Need to output vararg functions with a comment mentioning they are not
   translatable to the Haskell FFI

Also, please note that this tool will never be 100% accurate.  It cannot
translate macros, or anything related to the preprocessor, for example.  It
often misses necessary `#include` files, and will get them wrong in any case
if preprocessor conditionals are involved.

The goal of `c2hsc` is to solve the hardest 80% of the problem of creating an
FFI library.  The remaining 20%, plus validation of the results, is an
exercise necessarily left to the user.
