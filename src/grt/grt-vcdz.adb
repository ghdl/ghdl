--  GHDL Run Time (GRT) - VCD .gz module.
--  Copyright (C) 2005 - 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Vcd; use Grt.Vcd;
with Grt.Errors; use Grt.Errors;
with Grt.Types; use Grt.Types;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.Zlib; use Grt.Zlib;
with Grt.C; use Grt.C;

package body Grt.Vcdz is
   Stream : gzFile;

   procedure My_Vcd_Put (Str : String)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := gzwrite (Stream, Str'Address, Str'Length);
   end My_Vcd_Put;

   procedure My_Vcd_Putc (C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := gzputc (Stream, Character'Pos (C));
   end My_Vcd_Putc;

   procedure My_Vcd_Close is
   begin
      gzclose (Stream);
      Stream := NULL_gzFile;
   end My_Vcd_Close;

   --  VCD filename.

   --  Return TRUE if OPT is an option for VCD.
   function Vcdz_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
      Vcd_Filename : String_Access := null;
      Mode : constant String := "wb" & NUL;
   begin
      if Opt'Length < 7 or else Opt (F .. F + 6) /= "--vcdgz" then
         return False;
      end if;
      if Opt'Length > 7 and then Opt (F + 7) = '=' then
         if Vcd_Close /= null then
            Error ("--vcdgz: file already set");
            return True;
         end if;

         --  Add an extra NUL character.
         Vcd_Filename := new String (1 .. Opt'Length - 8 + 1);
         Vcd_Filename (1 .. Opt'Length - 8) := Opt (F + 8 .. Opt'Last);
         Vcd_Filename (Vcd_Filename'Last) := NUL;

         Stream := gzopen (Vcd_Filename.all'Address, Mode'Address);
         if Stream = NULL_gzFile then
            Error_S ("cannot open ");
            Error_E (Vcd_Filename (Vcd_Filename'First
                                   .. Vcd_Filename'Last - 1));
            return True;
         end if;
         Vcd_Putc := My_Vcd_Putc'Access;
         Vcd_Put := My_Vcd_Put'Access;
         Vcd_Close := My_Vcd_Close'Access;
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
     (Desc => new String'("vcdz: save waveforms in gzipped vcf file format"),
      Option => Vcdz_Option'Access,
      Help => Vcdz_Help'Access,
      Init => Proc_Hook_Nil'Access,
      Start => Proc_Hook_Nil'Access,
      Finish => Proc_Hook_Nil'Access);

   procedure Register is
   begin
      Register_Hooks (Vcdz_Hooks'Access);
   end Register;
end Grt.Vcdz;
