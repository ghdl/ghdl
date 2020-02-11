.. _INT:Frontend:

Front-end
#########

Input files (or source files) are read by `files_map.ad[sb]`. Only regular files can be
read, because they are read entirely before being scanned. This simplifies the scanner,
but this also allows to have a unique index for each character in any file.
Therefore the source location is a simple 32-bit integer whose type is `Location_Type`.
From the location, `files_map` can deduce the source file (type is `Source_File_Entry`)
and then the offset in the source file.  There is a line table for each source file in
order to speed-up the conversion from file offset to line number and column number.

The scanner (file :file:`vhdl-scanner.ad[sb]`) reads the source files and creates token
from them.  The tokens are defined in file :file:`vhdl-tokens.ads`.  Tokens are scanned
one by one, so the scanner doesn't keep in memory the previous token.  Integer or
floating point numbers are special tokens because beside the token itself there is
also a variable for the value of the number.

For identifiers there is a table containing all identifiers.  This is implemented by
file :file:`name_table.ad[sb]`.  Each identifier is associated to a 32-bit number
(they are internalized).  So the number is used to reference an identifier.  About
one thousand identifiers are predefined (by :file:`std_names.ad[sb]`).  Most of
them are reserved identifiers (or keywords).  When the scanner find an identifier, it
checks if it is a keyword.  In that case it changes the token to the keyword token.

The procedure `scan` is called to get the next token.  The location of the token and
the location after the token are available to store it in the parser tree.

The main client of the scanner is the parser.
