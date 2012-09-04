Converts C API header files to `.hsc` and `.hsc.helper.c` files

    Usage: c2hsc <Lib Prefix> [cpp options...] <FILE>.h

This will create `<FILE>.hsc`, and `<FILE>.hsc.helper.c` if the header file
contains inline functions.

For example, in `hlibgit2` on the Mac I'm using:

    c2hsc Bindings.Libgit2 -U__BLOCKS__ libgit2/include/git2/tree.h
