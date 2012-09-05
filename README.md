Converts C API header files to `.hsc` and `.hsc.helper.c` files

    Usage: c2hsc <Lib Prefix> [cpp options...] <FILE>.h

This will create `<FILE>.hsc`, and `<FILE>.hsc.helper.c` if the header file
contains inline functions.

For example, in `hlibgit2` on the Mac I'm using:

    c2hsc Bindings.Libgit2 -U__BLOCKS__ libgit2/include/git2/tree.h

Known issues:

 - Filename matching is not accurate enough
 - Pointers to "struct foo" are being rendered as Ptr () [void *]
 - Function pointers of void return type are rendered incorrectlyb
 - `const` is being dropped from BC_INLINE macros
 - Handle type synonyms (output them as type a = b)
 - Global variables are not being emitted
 - Varargs functions do not translate
 - Inline helper generator outputs the wrong headers

Also, please note that this tool will never be 100% accurate.  It cannot
translate macros, or anything related to the preprocessor, for example.  It
often misses necessary `#include` files, and will get them wrong in any case
if preprocessor conditionals are involved.

The goal of `c2hsc` is to solve the hardest 80% of the problem of creating an
FFI library.  The remaining 20%, plus validation of the results, is an
exercise necessarily left to the user.
