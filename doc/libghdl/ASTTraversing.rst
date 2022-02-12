.. _LIB:ASTTraversing:

AST Traversing
##############

If someone wants to use `libghdl`, then he probably wants to know how to traverse the AST to get the information about nodes of design units.
It would be really nice to provide an example showing how to traverse the tree.
In the `vhdl_ls` Load_File function is used to parse the file and get the tree root.
The return value is an integer.

* What does this integer represent?
* How to use this integer to traverse the AST and get information about nodes?
