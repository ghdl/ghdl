--  Output subroutine for dumps
--  Copyright (C) 2023 Tristan Gingold
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

with Grt.Stdio; use Grt.Stdio;
with Grt.C;

package body Outputs is

   Output : FILEs;

   function Open_File (Filename : String_Acc) return Boolean is
   begin
      if Filename = null then
         Output := stdout;
         return True;
      end if;

      declare
         Cfile : constant String := Filename.all & ASCII.NUL;
         Mode : constant String := "wt" & ASCII.NUL;
      begin
         Output := fopen (Cfile'Address, Mode'Address);
         return Output /= NULL_Stream;
      end;
   end Open_File;

   procedure Close is
   begin
      if Output /= NULL_Stream and Output /= stdout then
         fclose (Output);
      end if;
   end Close;

   procedure Wr (S : String)
   is
      L : Grt.C.size_t;
   begin
      L := fwrite (S'Address, S'Length, 1, Output);
      pragma Unreferenced (L);
   end Wr;

   procedure Wr (C : Character) is
   begin
      fputc (Character'Pos (C), Output);
   end Wr;

   procedure Wr_Line (S : String := "") is
   begin
      Wr (S);
      Wr (ASCII.LF);
   end Wr_Line;

   procedure Wr_Trim (S : String) is
   begin
      if S'First <= S'Last and then S (S'First) = ' ' then
         Wr (S (S'First + 1 .. S'Last));
      else
         Wr (S);
      end if;
   end Wr_Trim;

   procedure Wr_Uns32 (V : Uns32) is
   begin
      Wr_Trim (Uns32'Image (V));
   end Wr_Uns32;

   procedure Wr_Int32 (V : Int32) is
   begin
      Wr_Trim (Int32'Image (V));
   end Wr_Int32;

   procedure Wr_Indent (Indent : Natural) is
   begin
      for I in 1 .. Indent loop
         Wr ("  ");
      end loop;
   end Wr_Indent;

end Outputs;
