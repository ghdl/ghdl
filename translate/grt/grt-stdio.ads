--  GHDL Run Time (GRT) - stdio binding.
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
with System;
with Grt.C; use Grt.C;

--  This package provides a thin binding to the stdio.h of the C library.
--  It mimics GNAT package Interfaces.C_Streams.
--  The purpose of this package is to remove dependencies on the GNAT run time.

package Grt.Stdio is
   pragma Preelaborate (Grt.Stdio);

   --  Type FILE *.
   type FILEs is new System.Address;

   --  NULL for a stream.
   NULL_Stream : constant FILEs;

   --  Predefined streams.
   function stdout return FILEs;
   function stderr return FILEs;
   function stdin return FILEs;

   --  The following subprograms are translation of the C prototypes.

   function fopen (path: chars; mode : chars) return FILEs;

   function fwrite (buffer : voids;
                    size : size_t;
                    count : size_t;
                    stream : FILEs)
                   return size_t;

   function fread (buffer : voids;
                   size : size_t;
                   count : size_t;
                   stream : FILEs)
                  return size_t;

   function fputc (c : int; stream : FILEs) return int;
   procedure fputc (c : int; stream : FILEs);

   function fputs (s : chars; stream : FILEs) return int;

   function fgetc (stream : FILEs) return int;
   function fgets (s : chars; size : int; stream : FILEs) return chars;
   function ungetc (c : int; stream : FILEs) return int;

   function fflush (stream : FILEs) return int;
   procedure fflush (stream : FILEs);

   function feof (stream : FILEs) return int;

   function ftell (stream : FILEs) return long;

   function fclose (stream : FILEs) return int;
   procedure fclose (Stream : FILEs);
private
   --  This is a little bit dubious, but this package should be preelaborated,
   --  and Null_Address is not static (since defined in the private part
   --  of System).
   --  I am pretty sure the C definition of NULL is 0.
   NULL_Stream : constant FILEs := FILEs (System'To_Address (0));

   pragma Import (C, fopen);

   pragma Import (C, fwrite);
   pragma Import (C, fread);

   pragma Import (C, fputs);
   pragma Import (C, fputc);

   pragma Import (C, fgetc);
   pragma Import (C, fgets);
   pragma Import (C, ungetc);

   pragma Import (C, fflush);
   pragma Import (C, feof);
   pragma Import (C, ftell);
   pragma Import (C, fclose);

   pragma Import (C, stdout, "__ghdl_get_stdout");
   pragma Import (C, stderr, "__ghdl_get_stderr");
   pragma Import (C, stdin, "__ghdl_get_stdin");
end Grt.Stdio;
