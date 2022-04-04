--  Binary file execute in memory handler.
--  Copyright (C) 2006 - 2015 Tristan Gingold
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

   type Segment_Kind is (Seg_Text, Seg_Ro, Seg_Data, Seg_None);

   function Get_Segment (Sect : Section_Acc) return Segment_Kind is
   begin
      if Sect = Sect_Abs then
         return Seg_None;
      end if;
      if (Sect.Flags and Section_Exec) /= 0 then
         return Seg_Text;
      elsif (Sect.Flags and Section_Write) /= 0 then
         return Seg_Data;
      elsif (Sect.Flags and Section_Read) /= 0 then
         return Seg_Ro;
      else
         return Seg_None;
      end if;
   end Get_Segment;

   procedure Write_Memory_Relocate (Error : out Boolean)
   is
      Log_Pagesize : constant := 12;
      type Seg_Size_Array is array (Segment_Kind) of Pc_Type;
      Seg_Size : Seg_Size_Array;
      Seg_Offs : Seg_Size_Array;
      Size : Pc_Type;
      Program_Seg : Memsegs.Memseg_Type;
      Program : Byte_Array_Acc;
      Sect : Section_Acc;
      Rel : Reloc_Acc;
      N_Rel : Reloc_Acc;
   begin
      --  Compute sizes.
      Seg_Size := (others => 0);
      Sect := Section_Chain;
      while Sect /= null loop
         declare
            Seg : constant Segment_Kind := Get_Segment (Sect);
         begin
            --  Currently, offset in the segment.
            Sect.Img_Off := Pow_Align (Seg_Size (Seg), Sect.Align);
            Seg_Size (Seg) := Sect.Img_Off + Sect.Pc;
         end;
         Sect := Sect.Next;
      end loop;

      --  Align.
      for I in Seg_Text .. Seg_Data loop
         Seg_Size (I) := Pow_Align (Seg_Size (I), Log_Pagesize);
      end loop;

      --  Whole size.
      Size := 0;
      for I in Seg_Text .. Seg_Data loop
         Size := Size + Seg_Size (I);
      end loop;

      --  Allocate and copy.
      Program_Seg := Memsegs.Create;
      Memsegs.Resize (Program_Seg, Natural (Size));
      Program := To_Byte_Array_Acc (Memsegs.Get_Address (Program_Seg));
      Seg_Offs (Seg_Text) := 0;
      Seg_Offs (Seg_Ro) := Seg_Size (Seg_Text);
      Seg_Offs (Seg_Data) := Seg_Size (Seg_Text) + Seg_Size (Seg_Ro);

      Sect := Section_Chain;
      while Sect /= null loop
         declare
            Seg : constant Segment_Kind := Get_Segment (Sect);
            Off : Pc_Type renames Seg_Offs (Seg);
         begin
            if Seg /= Seg_None then
               --  From segment offset to image offset.
               Sect.Img_Off := Sect.Img_Off + Seg_Offs (Seg);

               if Sect.Pc > 0 then
                  Off := Pow_Align (Off, Sect.Align);
                  if Sect.Data = null then
                     --  For bss.
                     Program (Off .. Off + Sect.Pc - 1) := (others => 0);
                  else
                     Program (Off .. Off + Sect.Pc - 1) :=
                       Sect.Data (0 .. Sect.Pc - 1);
                     Memsegs.Delete (Sect.Seg);
                  end if;

                  Sect.Data := To_Byte_Array_Acc (Program (Off)'Address);

                  --  Set virtual address.
                  Sect.Vaddr := To_Pc_Type (Program (Off)'Address);

                  Off := Off + Sect.Pc;
               end if;
            end if;
         end;
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

         Sect := Sect.Next;
      end loop;

      if Seg_Size (Seg_Text) /= 0 then
         Memsegs.Set_Rx (Program_Seg, 0, Natural (Seg_Size (Seg_Text)));
      end if;
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
