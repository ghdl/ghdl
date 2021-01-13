--  Dwarf symbolizer.
--  Copyright (C) 2015 Tristan Gingold
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

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Dwarf; use Dwarf;

package body Symbolizer is
   type Abbrev_Array is array (Unsigned_32 range <>) of Address;
   type Abbrev_Array_Acc is access Abbrev_Array;

   --  Data for decoding abbrevs.
   --  Abbrevs are referenced by its number, but it is not possible to directly
   --  reference an abbrev from its number.  A map is required.
   --  The main purpose of these data is to build the map.
   type Abbrev_Data is record
      --  Static map.  Mcode doesn't generate a lot of abbrev.
      Sarray : Abbrev_Array (1 .. 64);
      --  First non-decoded abbrev.
      Next_Num : Unsigned_32;
      --  Address (in .debug_abbrev section) of the next abbrev to be decoded.
      Next_Addr : Address;
      --  Address of the first byte after the abbrev section.  Used to not read
      --  past the section.
      Last_Addr : Address;
      --  If there are too many abbrevs, use a resizable array instead of the
      --  static one.
      Map : Abbrev_Array_Acc;
   end record;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   procedure Read_Word4 (Addr : in out Address;
                         Res : out Unsigned_32)
   is
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      B2 := Read_Byte (Addr + 2);
      B3 := Read_Byte (Addr + 3);
      --  FIXME: we assume little-endian
      Res := Shift_Left (Unsigned_32 (B3), 24)
        or Shift_Left (Unsigned_32 (B2), 16)
        or Shift_Left (Unsigned_32 (B1), 8)
        or Shift_Left (Unsigned_32 (B0), 0);
      Addr := Addr + 4;
   end Read_Word4;

   procedure Read_Word8 (Addr : in out Address; Res : out Unsigned_64)
   is
      B : Unsigned_8;
   begin
      Res := 0;
      for I in 0 .. 7 loop
         B := Read_Byte (Addr + Storage_Offset (I));
         --  FIXME: we assume little-endian
         Res := Res or Shift_Left (Unsigned_64 (B), I * 8);
      end loop;
      Addr := Addr + 8;
   end Read_Word8;

   procedure Read_Word2 (Addr : in out Address;
                         Res : out Unsigned_16)
   is
      B0, B1 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      --  FIXME: we assume little-endian
      Res := Shift_Left (Unsigned_16 (B1), 8)
        or Shift_Left (Unsigned_16 (B0), 0);
      Addr := Addr + 2;
   end Read_Word2;

   procedure Read_Byte (Addr : in out Address;
                        Res : out Unsigned_8)
   is
   begin
      Res := Read_Byte (Addr);
      Addr := Addr + 1;
   end Read_Byte;

   procedure Read_ULEB128 (Addr : in out Address;
                           Res : out Unsigned_32)
   is
      B : Unsigned_8;
      Shift : Integer;
   begin
      Res := 0;
      Shift := 0;
      loop
         B := Read_Byte (Addr);
         Addr := Addr + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;
         Shift := Shift + 7;
      end loop;
   end Read_ULEB128;

   procedure Read_SLEB128 (Addr : in out Address;
                           Res : out Unsigned_32)
   is
      B : Unsigned_8;
      Shift : Integer;
   begin
      Res := 0;
      Shift := 0;
      loop
         B := Read_Byte (Addr);
         Addr := Addr + 1;
         Res := Res or Shift_Left (Unsigned_32 (B and 16#7f#), Shift);
         Shift := Shift + 7;
         exit when (B and 16#80#) = 0;
      end loop;
      if Shift < 32 and (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;
   end Read_SLEB128;

   procedure Init_Abbrev (Abbrevs : in out Abbrev_Data;
                          Sections : Dwarf_Sections;
                          Off : Storage_Offset)
   is
      Old_Map : Abbrev_Array_Acc;
   begin
      Old_Map := Abbrevs.Map;
      if Old_Map /= null then
         Old_Map.all := (others => Null_Address);
      end if;

      Abbrevs := (Sarray => (others => Null_Address),
                  Next_Num => 0,
                  Next_Addr => Sections.Debug_Abbrev.Vaddr + Off,
                  Last_Addr => (Sections.Debug_Abbrev.Vaddr
                                  + Sections.Debug_Abbrev.Size),
                  Map => Old_Map);
   end Init_Abbrev;

   procedure Find_Abbrev (Abbrevs : in out Abbrev_Data;
                          Num : Unsigned_32;
                          Res : out Address)
   is
      Code : Unsigned_32;
      Addr : Address;
      Tag, Name, Form : Unsigned_32;
   begin
      if Num > Abbrevs.Next_Num then
         --  Not yet decoded.
         Addr := Abbrevs.Next_Addr;

         while Addr < Abbrevs.Last_Addr loop
            --  Read abbreviation code.
            Read_ULEB128 (Addr, Code);

            if Code /= 0 then
               --  Not a pad.

               --  Insert address in map.
               if Abbrevs.Map = null then
                  if Code <= Abbrevs.Sarray'Last then
                     Abbrevs.Sarray (Code) := Addr;
                  else
                     raise Program_Error;
                  end if;
               else
                  if Code <= Abbrevs.Map'Last then
                     Abbrevs.Map (Code) := Addr;
                  else
                     --  Need to expand map.
                     raise Program_Error;
                  end if;
               end if;

               --  Read tag.
               Read_ULEB128 (Addr, Tag);

               --  Skip child flag.
               Addr := Addr + 1;

               --  Skip attribute specifications.
               loop
                  Read_ULEB128 (Addr, Name);
                  Read_ULEB128 (Addr, Form);
                  exit when Name = 0 and Form = 0;
               end loop;

               --  Found.
               exit when Code = Num;
            end if;
         end loop;

         --  Next entry to read.
         Abbrevs.Next_Addr := Addr;
      end if;

      --  Set result.
      if Abbrevs.Map = null then
         Res := Abbrevs.Sarray (Num);
      else
         Res := Abbrevs.Map (Num);
      end if;
   end Find_Abbrev;

   procedure Read_Uns32 (Addr : in out Address;
                         Form : Unsigned_32;
                         Res : out Unsigned_32) is
   begin
      case Form is
         when DW_FORM_Data4 =>
            Read_Word4 (Addr, Res);
         when others =>
            raise Program_Error;
      end case;
   end Read_Uns32;

   procedure Skip_String (Addr : in out Address) is
   begin
      while Read_Byte (Addr) /= 0 loop
         Addr := Addr + 1;
      end loop;
      Addr := Addr + 1;
   end Skip_String;

   procedure Read_Addr (Addr : in out Address; Res : out Address) is
   begin
      --  Turn off warnings for unmatched size.
      pragma Warnings (Off);
      if Address'Size = Unsigned_32'Size then
         declare
            function To_Address is new Ada.Unchecked_Conversion
              (Unsigned_32, Address);
            V : Unsigned_32;
         begin
            Read_Word4 (Addr, V);
            Res := To_Address (V);
         end;
      elsif Address'Size = Unsigned_64'Size then
         declare
            function To_Address is new Ada.Unchecked_Conversion
              (Unsigned_64, Address);
            V : Unsigned_64;
         begin
            Read_Word8 (Addr, V);
            Res := To_Address (V);
         end;
      else
         --  Unhandled address size.
         raise Program_Error;
      end if;
      pragma Warnings (On);
   end Read_Addr;

   procedure Read_Addr (Addr : in out Address;
                        Form : Unsigned_32;
                        Res : out Address)
   is
   begin
      case Form is
         when DW_FORM_Addr =>
            Read_Addr (Addr, Res);
         when DW_FORM_String =>
            Res := Addr;
            Skip_String (Addr);
         when others =>
            raise Program_Error;
      end case;
   end Read_Addr;

   procedure Read_Ref (Addr : in out Address;
                       Form : Unsigned_32;
                       Base : Address;
                       Res : out Address)
   is
      V : Unsigned_32;
   begin
      case Form is
         when DW_FORM_Ref4 =>
            Read_Word4 (Addr, V);
            Res := Base + Storage_Offset (V);
         when others =>
            raise Program_Error;
      end case;
   end Read_Ref;

   procedure Skip_Form (Addr : in out Address;
                        Form : Unsigned_32)
   is
   begin
      case Form is
         when DW_FORM_Addr =>
            Addr := Addr + 4;
         when DW_FORM_Flag =>
            Addr := Addr + 1;
         when DW_FORM_Block1 =>
            Addr := Addr + Storage_Offset (Read_Byte (Addr)) + 1;
         when DW_FORM_Data1 =>
            Addr := Addr + 1;
         when DW_FORM_Data2 =>
            Addr := Addr + 2;
         when DW_FORM_Data4 =>
            Addr := Addr + 4;
         when DW_FORM_Sdata
           | DW_FORM_Udata =>
            while (Read_Byte (Addr) and 16#80#) /= 0 loop
               Addr := Addr + 1;
            end loop;
            Addr := Addr + 1;
         when DW_FORM_Ref4 =>
            Addr := Addr + 4;
         when DW_FORM_Strp =>
            Addr := Addr + 4;
         when DW_FORM_String =>
            Skip_String (Addr);
         when others =>
            raise Program_Error;
      end case;
   end Skip_Form;

   procedure Find_Subprogram (Pc : Address;
                              Sections : Dwarf_Sections;
                              Res : out Symbolize_Result;
                              Abbrevs : in out Abbrev_Data;
                              Unit_Stmt_List : out Unsigned_32)
   is
      Base : Address;
      Addr : Address;
      Sect_Last_Addr : Address;
      Next_Unit_Addr : Address;

      Abbrev : Address;

      Unit_Len : Unsigned_32;
      Ver : Unsigned_16;
      Abbrev_Off : Unsigned_32;
      Ptr_Sz : Unsigned_8;
      Num : Unsigned_32;

      Tag : Unsigned_32;
      Abbrev_Name : Unsigned_32;
      Abbrev_Form : Unsigned_32;

      Level : Unsigned_8;

      Stmt_List : Unsigned_32;
      Low_Pc : Address;
      High_Pc : Address;
      Name : Address;
      Sibling : Address;
   begin
      --  Initialize result.
      Res := (Filename => Null_Address,
              Line => 0,
              Subprg_Name => Null_Address);

      Addr := Sections.Debug_Info.Vaddr;
      Sect_Last_Addr := Addr + Sections.Debug_Info.Size;

      while Addr < Sect_Last_Addr loop
         --  Read unit length.
         Base := Addr;
         Read_Word4 (Addr, Unit_Len);
         Next_Unit_Addr := Addr + Storage_Offset (Unit_Len);
         Read_Word2 (Addr, Ver);
         Read_Word4 (Addr, Abbrev_Off);
         Read_Byte (Addr, Ptr_Sz);
         Level := 0;

         Init_Abbrev (Abbrevs, Sections, Storage_Offset (Abbrev_Off));
         Unit_Stmt_List := Unsigned_32'Last;

         loop
            << Again >> null;
            exit when Addr >= Next_Unit_Addr;
            --  Read abbrev number.
            Read_ULEB128 (Addr, Num);

            --  End of children.
            if Num = 0 then
               Level := Level - 1;
               goto Again;
            end if;

            Find_Abbrev (Abbrevs, Num, Abbrev);
            if Abbrev = Null_Address then
               --  Not found...
               return;
            end if;

            Read_ULEB128 (Abbrev, Tag);
            if Read_Byte (Abbrev) /= 0 then
               Level := Level + 1;
            end if;

            --  skip child.
            Abbrev := Abbrev + 1;

            --  We are only interested in a few attributes.
            Stmt_List := Unsigned_32'Last;
            Low_Pc := Null_Address;
            High_Pc := Null_Address;
            Name := Null_Address;
            Sibling := Null_Address;

            loop
               Read_ULEB128 (Abbrev, Abbrev_Name);
               Read_ULEB128 (Abbrev, Abbrev_Form);
               exit when Abbrev_Name = 0 and Abbrev_Form = 0;
               case Abbrev_Name is
                  when DW_AT_Stmt_List =>
                     Read_Uns32 (Addr, Abbrev_Form, Stmt_List);
                  when DW_AT_Low_Pc =>
                     Read_Addr (Addr, Abbrev_Form, Low_Pc);
                  when DW_AT_High_Pc =>
                     Read_Addr (Addr, Abbrev_Form, High_Pc);
                  when DW_AT_Name =>
                     Read_Addr (Addr, Abbrev_Form, Name);
                  when DW_AT_Sibling =>
                     Read_Ref (Addr, Abbrev_Form, Base, Sibling);
                  when others =>
                     Skip_Form (Addr, Abbrev_Form);
               end case;
            end loop;

            case Tag is
               when DW_TAG_Compile_Unit =>
                  if Low_Pc /= Null_Address
                    and then High_Pc /= Null_Address
                    and then (Pc < Low_Pc or Pc > High_Pc)
                  then
                     --  Out of this compile unit.
                     Addr := Next_Unit_Addr;
                     exit;
                  end if;
                  Unit_Stmt_List := Stmt_List;
               when DW_TAG_Subprogram =>
                  if Low_Pc /= Null_Address
                    and then High_Pc /= Null_Address
                    and then (Pc >= Low_Pc and Pc <= High_Pc)
                  then
                     --  Found!
                     Res.Subprg_Name := Name;
                     return;
                  end if;
               when DW_TAG_Structure_Type
                 | DW_TAG_Enumeration_Type =>
                  if Sibling /= Null_Address then
                     Addr := Sibling;
                     Level := Level - 1;
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      end loop;
   end Find_Subprogram;

   procedure Skip_Filename (Addr : in out Address)
   is
      File_Dir : Unsigned_32;
      File_Time : Unsigned_32;
      File_Len : Unsigned_32;
   begin
      Skip_String (Addr);
      Read_ULEB128 (Addr, File_Dir);
      Read_ULEB128 (Addr, File_Time);
      Read_ULEB128 (Addr, File_Len);
   end Skip_Filename;

   procedure Find_Lineno (Pc_Addr : Address;
                          Sections : Dwarf_Sections;
                          Res : in out Symbolize_Result;
                          Stmt_List : Storage_Offset)
   is
      Addr : Address;
      Last_Addr : Address;
      Next_Addr : Address;

      --  Opcode length.  Use a fixed bound.
      Opc_Length : array (Unsigned_8 range 1 .. 32) of Unsigned_8;

      Total_Len : Unsigned_32;
      Version : Unsigned_16;
      Prolog_Len : Unsigned_32;
      Min_Insn_Len : Unsigned_8;
      Dflt_Is_Stmt : Unsigned_8;
      Line_Base : Unsigned_8;
      Line_Range : Unsigned_8;
      Opc_Base : Unsigned_8;

      B : Unsigned_8;
      Arg : Unsigned_32;

      File_Names : Address;

      Ext_Len : Unsigned_32;
      Ext_Opc : Unsigned_8;

      Last : Address;

      Pc : Address;
      Line : Unsigned_32;
      Line_Base2 : Unsigned_32;
      New_Row : Boolean;

      File_Id : Unsigned_32;
      Prev_File_Id : Unsigned_32;
      Prev_Pc : Address;
      Prev_Line : Unsigned_32;
   begin
      if Stmt_List >= Sections.Debug_Line.Size then
         --  Invalid stmt list.
         return;
      end if;
      Addr := Sections.Debug_Line.Vaddr + Stmt_List;
      Last_Addr := Addr + Sections.Debug_Line.Size - Stmt_List;

      while Addr < Last_Addr loop
         --  Read header.
         Read_Word4 (Addr, Total_Len);
         Last := Addr + Storage_Offset (Total_Len);
         Read_Word2 (Addr, Version);
         Read_Word4 (Addr, Prolog_Len);
         Read_Byte (Addr, Min_Insn_Len);
         Read_Byte (Addr, Dflt_Is_Stmt);
         Read_Byte (Addr, Line_Base);
         Read_Byte (Addr, Line_Range);
         Read_Byte (Addr, Opc_Base);

         Prev_Pc := Null_Address;
         Prev_Line := 0;
         Prev_File_Id := 0;
         File_Id := 0;
         New_Row := False;
         Pc := Null_Address;
         Line := 1;

         --  Sign extend line base.
         Line_Base2 := Unsigned_32 (Line_Base);
         if (Line_Base and 16#80#) /= 0 then
            Line_Base2 := Line_Base2 or 16#Ff_Ff_Ff_00#;
         end if;

         --  Read opcodes length.
         if Opc_Base > Opc_Length'Last then
            raise Program_Error;
         end if;
         for I in 1 .. Opc_Base - 1 loop
            Read_Byte (Addr, B);
            Opc_Length (I) := B;
         end loop;

         --  Include directories.
         loop
            B := Read_Byte (Addr);
            exit when B = 0;
            Skip_String (Addr);
         end loop;
         Addr := Addr + 1;

         --  Filenames.
         File_Names := Addr;
         loop
            B := Read_Byte (Addr);
            exit when B = 0;
            Skip_Filename (Addr);
         end loop;
         Addr := Addr + 1;

         --  The debug_line 'program'.
         while Addr < Last loop
            --  Read opcode.
            Read_Byte (Addr, B);

            if B = 0 then
               --  Extended opcode.
               Read_ULEB128 (Addr, Ext_Len);
               Next_Addr := Addr;
               Read_Byte (Addr, Ext_Opc);
               Next_Addr := Next_Addr + Storage_Offset (Ext_Len);
               case Ext_Opc is
                  when DW_LNE_End_Sequence =>
                     New_Row := True;
                  when DW_LNE_Set_Address =>
                     Read_Addr (Addr, Pc);
                  when others =>
                     raise Program_Error;
               end case;
               pragma Assert (Addr = Next_Addr);
            elsif B < Opc_Base then
               --  Standard opcode.
               case B is
                  when DW_LNS_Copy =>
                     New_Row := True;
                  when DW_LNS_Advance_Pc =>
                     Read_ULEB128 (Addr, Arg);
                     Pc := Pc
                       + Storage_Offset (Arg * Unsigned_32 (Min_Insn_Len));
                  when DW_LNS_Advance_Line =>
                     Read_SLEB128 (Addr, Arg);
                     Line := Line + Arg;
                  when DW_LNS_Const_Add_Pc =>
                     Pc := Pc + Storage_Offset
                       (Unsigned_32 ((255 - Opc_Base) / Line_Range)
                          * Unsigned_32 (Min_Insn_Len));
                  when DW_LNS_Set_File =>
                     Read_ULEB128 (Addr, File_Id);
                  when others =>
                     for J in 1 .. Opc_Length (B) loop
                        Read_ULEB128 (Addr, Arg);
                     end loop;
                     raise Program_Error;
               end case;
            else
               --  Special opcode.
               B := B - Opc_Base;
               Pc := Pc + Storage_Offset
                 (Unsigned_32 (B / Line_Range) * Unsigned_32 (Min_Insn_Len));
               Line := Line + Line_Base2 + Unsigned_32 (B mod Line_Range);
               New_Row := True;
            end if;

            if New_Row then
               New_Row := False;
               if Pc_Addr >= Prev_Pc and then Pc_Addr < Pc then
                  Res.Line := Natural (Prev_Line);

                  --  Search for filename.
                  if Prev_File_Id = 0 then
                     Addr := Null_Address;
                  else
                     Addr := File_Names;
                     while Prev_File_Id > 1 loop
                        exit when Read_Byte (Addr) = 0;
                        Skip_Filename (Addr);
                        Prev_File_Id := Prev_File_Id - 1;
                     end loop;
                  end if;
                  Res.Filename := Addr;

                  return;
               end if;
               Prev_Pc := Pc;
               Prev_Line := Line;
               Prev_File_Id := File_Id;
            end if;
         end loop;
      end loop;
   end Find_Lineno;

   procedure Symbolize_Address (Pc : Address;
                                Sections : Dwarf_Sections;
                                Res : out Symbolize_Result)
   is
      Abbrevs : Abbrev_Data;
      Unit_Stmt_List : Unsigned_32;
   begin
      Find_Subprogram (Pc, Sections, Res, Abbrevs, Unit_Stmt_List);

      if Unit_Stmt_List /= Unsigned_32'Last then
         Find_Lineno (Pc, Sections, Res, Storage_Offset (Unit_Stmt_List));
      end if;
   end Symbolize_Address;
end Symbolizer;
