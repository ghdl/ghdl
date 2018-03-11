--  Library interface for the analyzer.
--  Copyright (C) 2017 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Types; use Types;
with Iirs; use Iirs;

package Libghdl is
   --  To be called before Analyze_Init to set command line options.
   --  This decodes a driver option (so handle all analyzer options, and
   --  --ieee). Return 0 for success.
   function Set_Option (Opt : Thin_String_Ptr; Len : Natural) return Integer;

   --  To be called before Analyze_File to initialize analysis.
   procedure Analyze_Init;

   --  Analyze one file.
   function Analyze_File (File : Thin_String_Ptr; Len : Natural) return Iir;
end Libghdl;
