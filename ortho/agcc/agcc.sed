# SED script used to extract lines enclosed in /* BEGIN ... END  */ of a
# gnatbind C generated files.
# Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold

# If the current line starts with /* BEGIN, read next line and go to keep.
\@/* BEGIN@ {
 n
 b keep
}
# The current line is discarded, and a the cycle is restarted.
d

# keep the lines.
: keep
# If the current line starts with END, then it is removed and a new cycle is
# started.
\@  END@ d
# Print the current line
p
# Read the next line
n
# Go to keep.
b keep
