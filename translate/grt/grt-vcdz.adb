--  GHDL Run Time (GRT) - VCD .gz module.
--  Copyright (C) 2005 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
with Grt.Vcd; use Grt.Vcd;
with Grt.Errors; use Grt.Errors;
with Grt.Types; use Grt.Types;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.Zlib; use Grt.Zlib;
with Grt.C; use Grt.C;

package body Grt.Vcdz is
   type Vcd_IO_Gzip is new Vcd_IO_Handler with record
      Stream : gzFile;
   end record;
   type IO_Gzip_Acc is access Vcd_IO_Gzip;
   procedure Vcd_Put (Handler : access Vcd_IO_Gzip; Str : String);
   procedure Vcd_Putc (Handler : access Vcd_IO_Gzip; C : Character);
   procedure Vcd_Close (Handler : access Vcd_IO_Gzip);

   procedure Vcd_Put (Handler : access Vcd_IO_Gzip; Str : String)
   is
      R : int;
   begin
      R := gzwrite (Handler.Stream, Str'Address, Str'Length);
   end Vcd_Put;

   procedure Vcd_Putc (Handler : access Vcd_IO_Gzip; C : Character)
   is
      R : int;
   begin
      R := gzputc (Handler.Stream, Character'Pos (C));
   end Vcd_Putc;

   procedure Vcd_Close (Handler : access Vcd_IO_Gzip) is
   begin
      gzclose (Handler.Stream);
      Handler.Stream := NULL_gzFile;
   end Vcd_Close;

   --  VCD filename.

   --  Return TRUE if OPT is an option for VCD.
   function Vcdz_Option (Opt : String) return Boolean
   is
      F : Natural := Opt'First;
      Vcd_Filename : String_Access := null;
      Handler : IO_Gzip_Acc;
      Mode : constant String := "wb" & NUL;
   begin
      if Opt'Length < 7 or else Opt (F .. F + 6) /= "--vcdgz" then
         return False;
      end if;
      if Opt'Length > 7 and then Opt (F + 7) = '=' then
         if H /= null then
            Error ("--vcdgz: file already set");
            return True;
         end if;

         --  Add an extra NUL character.
         Vcd_Filename := new String (1 .. Opt'Length - 8 + 1);
         Vcd_Filename (1 .. Opt'Length - 8) := Opt (F + 8 .. Opt'Last);
         Vcd_Filename (Vcd_Filename'Last) := NUL;

         Handler := new Vcd_IO_Gzip;
         Handler.Stream := gzopen (Vcd_Filename.all'Address, Mode'Address);
         if Handler.Stream = NULL_gzFile then
            Error_C ("cannot open ");
            Error_E (Vcd_Filename (Vcd_Filename'First
                                   .. Vcd_Filename'Last - 1));
            return True;
         end if;
         H := Handler_Acc (Handler);
         return True;
      else
         return False;
      end if;
   end Vcdz_Option;

   procedure Vcdz_Help is
   begin
      Put_Line
        (" --vcdgz=FILENAME   dump signal values into a VCD gzip'ed file");
   end Vcdz_Help;

   Vcdz_Hooks : aliased constant Hooks_Type :=
     (Option => Vcdz_Option'Access,
      Help => Vcdz_Help'Access,
      Init => Proc_Hook_Nil'Access,
      Start => Proc_Hook_Nil'Access,
      Finish => Proc_Hook_Nil'Access);

   procedure Register is
   begin
      Register_Hooks (Vcdz_Hooks'Access);
   end Register;
end Grt.Vcdz;
