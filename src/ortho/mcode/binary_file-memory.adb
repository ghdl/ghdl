--  Binary file execute in memory handler.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body Binary_File.Memory is
   --  Absolute section.
   Sect_Abs : Section_Acc;

   function To_Pc_Type is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Pc_Type);

   procedure Set_Symbol_Address (Sym : Symbol; Addr : System.Address)
   is
   begin
      Set_Symbol_Value (Sym, To_Pc_Type (Addr));
      Set_Scope (Sym, Sym_Global);
      Set_Section (Sym, Sect_Abs);
   end Set_Symbol_Address;

   procedure Write_Memory_Init is
   begin
      Create_Section (Sect_Abs, "*ABS*", Section_Exec);
      Sect_Abs.Vaddr := 0;
   end Write_Memory_Init;

   procedure Write_Memory_Relocate (Error : out Boolean)
   is
      Sect : Section_Acc;
      Rel : Reloc_Acc;
      N_Rel : Reloc_Acc;
   begin
      --  Relocate section in memory.
      Sect := Section_Chain;
      while Sect /= null loop
         if Sect.Data = null then
            if Sect.Pc > 0 then
               Resize (Sect, Sect.Pc);
               Sect.Data (0 .. Sect.Pc - 1) := (others => 0);
            else
               null;
               --Sect.Data := new Byte_Array (1 .. 0);
            end if;
         end if;
         if Sect.Data_Max > 0
           and (Sect /= Sect_Abs and Sect.Flags /= Section_Debug)
         then
            Sect.Vaddr := To_Pc_Type (Sect.Data (0)'Address);
         end if;
         Sect := Sect.Next;
      end loop;

      --  Do all relocations.
      Sect := Section_Chain;
      Error := False;
      while Sect /= null loop
--           Put_Line ("Section: " & Sect.Name.all & ", Flags:"
--                     & Section_Flags'Image (Sect.Flags));
         Rel := Sect.First_Reloc;
         while Rel /= null loop
            N_Rel := Rel.Sect_Next;
            if Get_Scope (Rel.Sym) = Sym_Undef then
               Put_Line ("symbol " & Get_Symbol_Name (Rel.Sym)
                         & " is undefined");
               Error := True;
            else
               Apply_Reloc (Sect, Rel);
            end if;
            Free (Rel);
            Rel := N_Rel;
         end loop;

         Sect.First_Reloc := null;
         Sect.Last_Reloc := null;
         Sect.Nbr_Relocs := 0;

         if (Sect.Flags and Section_Exec) /= 0
           and (Sect.Flags and Section_Write) = 0
         then
            Memsegs.Set_Rx (Sect.Seg);
         end if;

         Sect := Sect.Next;
      end loop;
   end Write_Memory_Relocate;
end Binary_File.Memory;
