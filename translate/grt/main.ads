--  GHDL Run Time (GRT) - C-like entry point.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

--  In the usual case of a standalone executable, this file defines the
--  standard entry point, ie the main() function.
--
--  However, as explained in the manual, the user can use its own main()
--  function, and calls the ghdl entry point ghdl_main.
with System;

function Main (Argc : Integer; Argv : System.Address) return Integer;
pragma Export (C, Main, "main");
