--  Binary file execute in memory handler.
--  Copyright (C) 2006 - 2015 Tristan Gingold
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

package body Binary_File.Memory is
   --  Absolute section.
   Sect_Abs : Section_Acc;

   --  PLT section (for x86-64).
   Sect_Plt : Section_Acc;

   procedure Set_Symbol_Address (Sym : Symbol; Addr : System.Address) is
   begin
      if Arch = Arch_X86_64 and then Is_Symbol_Code (Sym) then
         --  Branches are limited on x86-64 to a 32 bit offset.  Create a
         --  trampoline so that functions created outside of the module could
         --  be reached using the standard ABI.
         --
         --  This works only for code, not for data.  Therefore we assume that
         --  data symbols are correctly handled.
         declare
            V : Unsigned_64;
            Pc : constant Pc_Type := Sect_Plt.Pc;
         begin
            Set_Current_Section (Sect_Plt);
            Prealloc (16);

            --  Emit: movabs $ADDR, %r11
            V := Unsigned_64 (To_Pc_Type (Addr));
            Sect_Plt.Data (Pc + 0) := 16#49#;
            Sect_Plt.Data (Pc + 1) := 16#BB#;
            for I in Pc_Type range 0 .. 7 loop
               Sect_Plt.Data (Pc + 2 + I) := Byte (V and 16#ff#);
               V := Shift_Right (V, 8);
            end loop;

            --  Emit: jmp *%r11
            Sect_Plt.Data (Pc + 10) := 16#41#;
            Sect_Plt.Data (Pc + 11) := 16#FF#;
            Sect_Plt.Data (Pc + 12) := 16#E3#;

            Sect_Plt.Pc := Pc + 13;
            Set_Symbol_Value (Sym, Pc);
            Set_Section (Sym, Sect_Plt);
         end;
      else
         Set_Symbol_Value (Sym, To_Pc_Type (Addr));
         Set_Section (Sym, Sect_Abs);
      end if;

      --  Symbol is not anymore undefined.
      Set_Scope (Sym, Sym_Global);
   end Set_Symbol_Address;

   procedure Write_Memory_Init is
   begin
      Create_Section (Sect_Abs, "*ABS*", Section_Exec);
      Sect_Abs.Vaddr := 0;

      if Arch = Arch_X86_64 then
         Create_Section (Sect_Plt, ".plt", Section_Exec);
      end if;
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
         --  Allocate memory if needed (eg: .bss)
         if Sect.Data = null then
            if Sect.Pc > 0 then
               Resize (Sect, Sect.Pc);
               Sect.Data (0 .. Sect.Pc - 1) := (others => 0);
            end if;
         end if;

         --  Set virtual address.
         if Sect.Pc > 0
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

   function Get_Section_Addr (Sect : Section_Acc) return System.Address is
   begin
      return Sect.Data (0)'Address;
   end Get_Section_Addr;

   function Get_Section_Size (Sect : Section_Acc) return Pc_Type is
   begin
      return Sect.Pc;
   end Get_Section_Size;
end Binary_File.Memory;
