--  Mcode back-end for ortho - Dwarf generator.
--  Copyright (C) 2006 Tristan Gingold
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

with Ada.Text_IO;
with GNAT.Directory_Operations;

with Tables;

with Dwarf; use Dwarf;
with Ortho_Code.Flags; use Ortho_Code.Flags;
with Ortho_Code.Decls;
with Ortho_Code.Types;
with Ortho_Code.Consts;
with Ortho_Ident;
with Ortho_Code.Binary;

package body Ortho_Code.Dwarf is
   --  Dwarf debugging format.
   --  Debugging.
   Line1_Sect : Section_Acc := null;
   Line_Last : Int32 := 0;
   Line_Pc : Pc_Type := 0;

   --  Constant.
   Min_Insn_Len : constant := 1;
   Line_Base : constant := 1;
   Line_Range : constant := 4;
   Line_Opcode_Base : constant := 13;
   Line_Max_Addr : constant := (255 - Line_Opcode_Base) / Line_Range;
   -- + Line_Base;

   Cur_File : Natural := 0;
   Last_File : Natural := 0;

   Orig_Sym : Symbol;
   End_Sym : Symbol;
   Abbrev_Sym : Symbol;
   Info_Sym : Symbol;
   Line_Sym : Symbol;

   Abbrev_Last : Unsigned_32;

   procedure Gen_String_Nul (Str : String)
   is
   begin
      Prealloc (Str'Length + 1);
      for I in Str'Range loop
         Gen_8 (Character'Pos (Str (I)));
      end loop;
      Gen_8 (0);
   end Gen_String_Nul;

   procedure Gen_Sleb128 (V : Int32)
   is
      V1 : Uns32 := To_Uns32 (V);
      V2 : Uns32;
      B : Byte;
      function Shift_Right_Arithmetic (Value : Uns32; Amount : Natural)
                                      return Uns32;
      pragma Import (Intrinsic, Shift_Right_Arithmetic);
   begin
      loop
         B := Byte (V1 and 16#7F#);
         V2 := Shift_Right_Arithmetic (V1, 7);
         if (V2 = 0 and (B and 16#40#) = 0)
           or (V2 = -1 and (B and 16#40#) /= 0)
         then
            Gen_8 (B);
            exit;
         else
            Gen_8 (B or 16#80#);
            V1 := V2;
         end if;
      end loop;
   end Gen_Sleb128;

   procedure Gen_Uleb128 (V : Unsigned_32)
   is
      V1 : Unsigned_32 := V;
      B : Byte;
   begin
      loop
         B := Byte (V1 and 16#7f#);
         V1 := Shift_Right (V1, 7);
         if V1 /= 0 then
            Gen_8 (B or 16#80#);
         else
            Gen_8 (B);
            exit;
         end if;
      end loop;
   end Gen_Uleb128;

   procedure Set_Line_Stmt (Line : Int32)
   is
      Pc : Pc_Type;
      D_Pc : Pc_Type;
      D_Ln : Int32;
   begin
      if Line = Line_Last then
         return;
      end if;
      Pc := Get_Current_Pc;

      D_Pc := (Pc - Line_Pc) / Min_Insn_Len;
      D_Ln := Line - Line_Last;

      --  Always emit line information, since missing info can distrub the
      --  user.
      --  As an optimization, we could try to emit the highest line for the
      --  same PC, since GDB seems to handle this way.
      if False and D_Pc = 0 then
         return;
      end if;

      Set_Current_Section (Line1_Sect);
      Prealloc (32);

      if Cur_File /= Last_File then
         Gen_8 (Byte (DW_LNS_Set_File));
         Gen_Uleb128 (Unsigned_32 (Cur_File));
         Last_File := Cur_File;
      elsif Cur_File = 0 then
         --  No file yet.
         return;
      end if;

      if D_Ln < Line_Base or D_Ln >= (Line_Base + Line_Range) then
         --  Emit an advance line.
         Gen_8 (Byte (DW_LNS_Advance_Line));
         Gen_Sleb128 (Int32 (D_Ln - Line_Base));
         D_Ln := Line_Base;
      end if;
      if D_Pc >= Line_Max_Addr then
         --  Emit an advance addr.
         Gen_8 (Byte (DW_LNS_Advance_Pc));
         Gen_Uleb128 (Unsigned_32 (D_Pc));
         D_Pc := 0;
      end if;
      Gen_8 (Line_Opcode_Base
              + Byte (D_Pc) * Line_Range
              + Byte (D_Ln - Line_Base));

      Line_Pc := Pc;
      Line_Last := Line;
   end Set_Line_Stmt;


   type String_Acc is access constant String;

   type Dir_Chain;
   type Dir_Chain_Acc is access Dir_Chain;
   type Dir_Chain is record
      Name : String_Acc;
      Next : Dir_Chain_Acc;
   end record;

   type File_Chain;
   type File_Chain_Acc is access File_Chain;
   type File_Chain is record
      Name : String_Acc;
      Dir : Natural;
      Next : File_Chain_Acc;
   end record;

   Dirs : Dir_Chain_Acc := null;
   Files : File_Chain_Acc := null;

   procedure Set_Filename (Dir : String; File : String)
   is
      D : Natural;
      F : Natural;
      D_C : Dir_Chain_Acc;
      F_C : File_Chain_Acc;
   begin
      --  Find directory.
      if Dir = "" then
         --  Current directory.
         D := 0;
      elsif Dirs = null then
         --  First directory.
         Dirs := new Dir_Chain'(Name => new String'(Dir),
                                Next => null);
         D := 1;
      else
         --  Find a directory.
         D_C := Dirs;
         D := 1;
         loop
            exit when D_C.Name.all = Dir;
            D := D + 1;
            if D_C.Next = null then
               D_C.Next := new Dir_Chain'(Name => new String'(Dir),
                                          Next => null);
               exit;
            else
               D_C := D_C.Next;
            end if;
         end loop;
      end if;

      --  Find file.
      F := 1;
      if Files = null then
         --  first file.
         Files := new File_Chain'(Name => new String'(File),
                                  Dir => D,
                                  Next => null);
      else
         F_C := Files;
         loop
            exit when F_C.Name.all = File and F_C.Dir = D;
            F := F + 1;
            if F_C.Next = null then
               F_C.Next := new File_Chain'(Name => new String'(File),
                                           Dir => D,
                                           Next => null);
               exit;
            else
               F_C := F_C.Next;
            end if;
         end loop;
      end if;
      Cur_File := F;
   end Set_Filename;

   procedure Gen_Abbrev_Header (Tag : Unsigned_32; Child : Byte) is
   begin
      Gen_Uleb128 (Tag);
      Gen_8 (Child);
   end Gen_Abbrev_Header;

   procedure Gen_Abbrev_Tuple (Attr : Unsigned_32; Form : Unsigned_32) is
   begin
      Gen_Uleb128 (Attr);
      Gen_Uleb128 (Form);
   end Gen_Abbrev_Tuple;

   procedure Init is
   begin
      --  Generate type names.
      Flags.Flag_Type_Name := True;

      Orig_Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Orig_Sym, False);
      End_Sym := Create_Local_Symbol;

      Create_Section (Line1_Sect, ".debug_line-1", Section_Debug);
      Set_Current_Section (Line1_Sect);

      --  Write Address.
      Gen_8 (0); -- extended opcode
      Gen_8 (1 + Pc_Type_Sizeof); -- length
      Gen_8 (Byte (DW_LNE_Set_Address));
      Gen_Ua_Addr (Orig_Sym, 0);

      Line_Last := 1;

      Create_Section (Line_Sect, ".debug_line", Section_Debug);
      Set_Section_Info (Line_Sect, null, 0, 0);
      Set_Current_Section (Line_Sect);
      Line_Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Line_Sym, False);

      --  Abbrevs.
      Create_Section (Abbrev_Sect, ".debug_abbrev", Section_Debug);
      Set_Section_Info (Abbrev_Sect, null, 0, 0);
      Set_Current_Section (Abbrev_Sect);

      Abbrev_Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Abbrev_Sym, False);

      Gen_Uleb128 (1);
      Gen_Abbrev_Header (DW_TAG_Compile_Unit, DW_CHILDREN_Yes);

      Gen_Abbrev_Tuple (DW_AT_Stmt_List, DW_FORM_Data4);
      Gen_Abbrev_Tuple (DW_AT_Low_Pc, DW_FORM_Addr);
      Gen_Abbrev_Tuple (DW_AT_High_Pc, DW_FORM_Addr);
      Gen_Abbrev_Tuple (DW_AT_Producer, DW_FORM_String);
      Gen_Abbrev_Tuple (DW_AT_Comp_Dir, DW_FORM_String);
      Gen_Abbrev_Tuple (0, 0);

      Abbrev_Last := 1;

      --  Info.
      Create_Section (Info_Sect, ".debug_info", Section_Debug);
      Set_Section_Info (Info_Sect, null, 0, 0);
      Set_Current_Section (Info_Sect);
      Info_Sym := Create_Local_Symbol;
      Set_Symbol_Pc (Info_Sym, False);

      Gen_32 (7);  --  Length: to be patched.
      Gen_16 (2);  --  version
      Gen_Ua_32 (Abbrev_Sym); --  Abbrev offset
      Gen_8 (Pc_Type_Sizeof);  --  Ptr size.

      --  Compile_unit.
      Gen_Uleb128 (1);
      Gen_Ua_32 (Line_Sym);
      Gen_Ua_Addr (Orig_Sym, 0);
      Gen_Ua_Addr (End_Sym, 0);
      Gen_String_Nul ("T.Gingold ortho_mcode (2004)");
      Gen_String_Nul (GNAT.Directory_Operations.Get_Current_Dir);
   end Init;

   procedure Emit_Decl (Decl : O_Dnode);

   --  Next node to be emitted.
   Last_Decl : O_Dnode := O_Dnode_First;

   procedure Emit_Decls_Until (Last : O_Dnode)
   is
      use Ortho_Code.Decls;
   begin
      while Last_Decl < Last loop
         Emit_Decl (Last_Decl);
         Last_Decl := Get_Decl_Chain (Last_Decl);
      end loop;
   end Emit_Decls_Until;

   procedure Finish
   is
      Length : Pc_Type;
      Last : O_Dnode;
   begin
      Set_Symbol_Pc (End_Sym, False);
      Length := Get_Current_Pc;

      Last := Decls.Get_Decl_Last;
      Emit_Decls_Until (Last);
      if Last_Decl <= Last then
         Emit_Decl (Last);
      end if;

      --  Finish abbrevs.
      Set_Current_Section (Abbrev_Sect);
      Gen_Uleb128 (0);

      --  Emit header.
      Set_Current_Section (Line_Sect);
      Prealloc (32);

      --  Unit_Length (to be patched).
      Gen_32 (0);
      --  version
      Gen_16 (2);
      --  header_length (to be patched).
      Gen_32 (5 + 12 + 1);
      --  minimum_instruction_length.
      Gen_8 (Min_Insn_Len);
      --  default_is_stmt
      Gen_8 (1);
      --  line base
      Gen_8 (Line_Base);
      --  line range
      Gen_8 (Line_Range);
      --  opcode base
      Gen_8 (Line_Opcode_Base);
      --  standard_opcode_length.
      Gen_8 (0); --  copy
      Gen_8 (1); --  advance pc
      Gen_8 (1); --  advance line
      Gen_8 (1); --  set file
      Gen_8 (1); --  set column
      Gen_8 (0); --  negate stmt
      Gen_8 (0); --  set basic block
      Gen_8 (0); --  const add pc
      Gen_8 (1); --  fixed advance pc
      Gen_8 (0); --  set prologue end
      Gen_8 (0); --  set epilogue begin
      Gen_8 (1); --  set isa
      --if Line_Opcode_Base /= 13 then
      --   raise Program_Error;
      --end if;

      --  include directories
      declare
         D : Dir_Chain_Acc;
      begin
         D := Dirs;
         while D /= null loop
            Gen_String_Nul (D.Name.all);
            D := D.Next;
         end loop;
         Prealloc (1);
         Gen_8 (0); -- last entry.
      end;

      --  file_names.
      declare
         F : File_Chain_Acc;
      begin
         F := Files;
         while F /= null loop
            Gen_String_Nul (F.Name.all);
            Prealloc (8);
            Gen_Uleb128 (Unsigned_32 (F.Dir));
            Gen_8 (0);  --  time
            Gen_8 (0);  --  length
            F := F.Next;
         end loop;
         Gen_8 (0);  --  last entry.
      end;

      --  Set prolog length
      Patch_32 (6, Unsigned_32 (Get_Current_Pc - 6));

      Merge_Section (Line_Sect, Line1_Sect);
      Prealloc (4);

      --  Emit end of sequence.
      Gen_8 (0); -- extended opcode
      Gen_8 (1); -- length: 1
      Gen_8 (Byte (DW_LNE_End_Sequence));

      --  Set total length.
      Patch_32 (0, Unsigned_32 (Get_Current_Pc - 4));

      --  Info.
      Set_Current_Section (Info_Sect);
      Prealloc (8);
      --  Finish child.
      Gen_Uleb128 (0);
      --  Set total length.
      Patch_32 (0, Unsigned_32 (Get_Current_Pc - 4));

      --  Aranges
      Create_Section (Aranges_Sect, ".debug_aranges", Section_Debug);
      Set_Section_Info (Aranges_Sect, null, 0, 0);
      Set_Current_Section (Aranges_Sect);

      Prealloc (32);
      Gen_32 (24 + Pc_Type_Sizeof);  --  Length.
      Gen_16 (2);  --  version
      Gen_Ua_32 (Info_Sym); --  info offset
      Gen_8 (Pc_Type_Sizeof);  --  Ptr size.
      Gen_8 (0);  --  seg desc size.
      Gen_32 (0);  --  pad
      Gen_Ua_Addr (Orig_Sym, 0); --  text offset
      Gen_32 (Unsigned_32 (Length));
      Gen_32 (0); --  End
      Gen_32 (0);
   end Finish;

   procedure Generate_Abbrev (Abbrev : out Unsigned_32) is
   begin
      Abbrev_Last := Abbrev_Last + 1;
      Abbrev := Abbrev_Last;

      Set_Current_Section (Abbrev_Sect);
      --  FIXME: should be enough ?
      Prealloc (128);
      Gen_Uleb128 (Abbrev);
   end Generate_Abbrev;

   procedure Gen_Info_Header (Abbrev : Unsigned_32) is
   begin
      Set_Current_Section (Info_Sect);
      Gen_Uleb128 (Abbrev);
   end Gen_Info_Header;

   function Gen_Info_Sibling return Pc_Type
   is
      Pc : Pc_Type;
   begin
      Pc := Get_Current_Pc;
      Gen_32 (0);
      return Pc;
   end Gen_Info_Sibling;

   procedure Patch_Info_Sibling (Pc : Pc_Type) is
   begin
      Patch_32 (Pc, Unsigned_32 (Get_Current_Pc));
   end Patch_Info_Sibling;

   Abbrev_Base_Type : Unsigned_32 := 0;
   Abbrev_Base_Type_Name : Unsigned_32 := 0;
   Abbrev_Pointer : Unsigned_32 := 0;
   Abbrev_Pointer_Name : Unsigned_32 := 0;
   Abbrev_Uncomplete_Pointer : Unsigned_32 := 0;
   Abbrev_Uncomplete_Pointer_Name : Unsigned_32 := 0;
   Abbrev_Ucarray : Unsigned_32 := 0;
   Abbrev_Ucarray_Name : Unsigned_32 := 0;
   Abbrev_Uc_Subrange : Unsigned_32 := 0;
   Abbrev_Subarray : Unsigned_32 := 0;
   Abbrev_Subarray_Name : Unsigned_32 := 0;
   Abbrev_Subrange : Unsigned_32 := 0;
   Abbrev_Struct : Unsigned_32 := 0;
   Abbrev_Struct_Name : Unsigned_32 := 0;
   Abbrev_Union : Unsigned_32 := 0;
   Abbrev_Union_Name : Unsigned_32 := 0;
   Abbrev_Member : Unsigned_32 := 0;
   Abbrev_Enum : Unsigned_32 := 0;
   Abbrev_Enum_Name : Unsigned_32 := 0;
   Abbrev_Enumerator : Unsigned_32 := 0;

   package TOnodes is new Tables
     (Table_Component_Type => Pc_Type,
      Table_Index_Type => O_Tnode,
      Table_Low_Bound => O_Tnode_First,
      Table_Initial => 16);

   procedure Emit_Type_Ref (Atype : O_Tnode)
   is
      Off : Pc_Type;
   begin
      pragma Assert (Flag_Debug >= Debug_Dwarf);
      Off := TOnodes.Table (Atype);
      pragma Assert (Off /= Null_Pc);
      Gen_32 (Unsigned_32 (Off));
   end Emit_Type_Ref;

   procedure Emit_Ident (Id : O_Ident)
   is
      use Ortho_Ident;
      L : Natural;
   begin
      L := Get_String_Length (Id);
      Prealloc (Pc_Type (L) + 128);
      Gen_String_Nul (Get_String (Id));
   end Emit_Ident;

   procedure Add_Type_Ref (Atype : O_Tnode; Pc : Pc_Type)
   is
      Prev : O_Tnode;
   begin
      if Atype > TOnodes.Last then
         --  Expand.
         Prev := TOnodes.Last;
         TOnodes.Set_Last (Atype);
         TOnodes.Table (Prev + 1 .. Atype - 1) := (others => Null_Pc);
      end if;
      TOnodes.Table (Atype) := Pc;
   end Add_Type_Ref;

   procedure Emit_Decl_Ident (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
   begin
      Emit_Ident (Get_Decl_Ident (Decl));
   end Emit_Decl_Ident;

   procedure Emit_Decl_Ident_If_Set (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
   begin
      if Decl /= O_Dnode_Null then
         Emit_Ident (Get_Decl_Ident (Decl));
      end if;
   end Emit_Decl_Ident_If_Set;

   procedure Emit_Type (Atype : O_Tnode);

   procedure Emit_Base_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Encoding, DW_FORM_Data1);
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Data1);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;
   begin
      if Decl = O_Dnode_Null then
         if Abbrev_Base_Type = 0 then
            Generate_Abbrev (Abbrev_Base_Type);
            Gen_Abbrev_Header (DW_TAG_Base_Type, DW_CHILDREN_No);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Base_Type);
      else
         if Abbrev_Base_Type_Name = 0 then
            Generate_Abbrev (Abbrev_Base_Type_Name);
            Gen_Abbrev_Header (DW_TAG_Base_Type, DW_CHILDREN_No);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Base_Type_Name);
         Emit_Decl_Ident (Decl);
      end if;

      case Get_Type_Kind (Atype) is
         when OT_Signed =>
            Gen_8 (DW_ATE_Signed);
         when OT_Unsigned =>
            Gen_8 (DW_ATE_Unsigned);
         when OT_Float =>
            Gen_8 (DW_ATE_Float);
         when others =>
            raise Program_Error;
      end case;
      Gen_8 (Byte (Get_Type_Size (Atype)));
   end Emit_Base_Type;

   procedure Emit_Access_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Data1);
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;

      procedure Finish_Gen_Abbrev_Uncomplete is
      begin
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Data1);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev_Uncomplete;

      Dtype : O_Tnode;
      D_Pc : Pc_Type;
   begin
      Dtype := Get_Type_Access_Type (Atype);

      if Dtype = O_Tnode_Null then
         if Decl = O_Dnode_Null then
            if Abbrev_Uncomplete_Pointer = 0 then
               Generate_Abbrev (Abbrev_Uncomplete_Pointer);
               Gen_Abbrev_Header (DW_TAG_Pointer_Type, DW_CHILDREN_No);
               Finish_Gen_Abbrev_Uncomplete;
            end if;
            Gen_Info_Header (Abbrev_Uncomplete_Pointer);
         else
            if Abbrev_Uncomplete_Pointer_Name = 0 then
               Generate_Abbrev (Abbrev_Uncomplete_Pointer_Name);
               Gen_Abbrev_Header (DW_TAG_Pointer_Type, DW_CHILDREN_No);
               Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
               Finish_Gen_Abbrev_Uncomplete;
            end if;
            Gen_Info_Header (Abbrev_Uncomplete_Pointer_Name);
            Emit_Decl_Ident (Decl);
         end if;
         Gen_8 (Byte (Get_Type_Size (Atype)));
      else
         if Decl = O_Dnode_Null then
            if Abbrev_Pointer = 0 then
               Generate_Abbrev (Abbrev_Pointer);
               Gen_Abbrev_Header (DW_TAG_Pointer_Type, DW_CHILDREN_No);
               Finish_Gen_Abbrev;
            end if;
            Gen_Info_Header (Abbrev_Pointer);
         else
            if Abbrev_Pointer_Name = 0 then
               Generate_Abbrev (Abbrev_Pointer_Name);
               Gen_Abbrev_Header (DW_TAG_Pointer_Type, DW_CHILDREN_No);
               Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
               Finish_Gen_Abbrev;
            end if;
            Gen_Info_Header (Abbrev_Pointer_Name);
            Emit_Decl_Ident (Decl);
         end if;
         Gen_8 (Byte (Get_Type_Size (Atype)));
         --  Break possible loops: generate the access entry...
         D_Pc := Get_Current_Pc;
         Gen_32 (0);
         --  ... generate the designated type ...
         Emit_Type (Dtype);
         --  ... and write its reference.
         Patch_32 (D_Pc, Unsigned_32 (TOnodes.Table (Dtype)));
      end if;
   end Emit_Access_Type;

   procedure Emit_Array_Type
     (Decl : O_Dnode; El_Type : O_Tnode; Idx_Type : O_Tnode)
   is
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;
   begin
      if Decl = O_Dnode_Null then
         if Abbrev_Ucarray = 0 then
            Generate_Abbrev (Abbrev_Ucarray);
            Gen_Abbrev_Header (DW_TAG_Array_Type, DW_CHILDREN_Yes);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Ucarray);
      else
         if Abbrev_Ucarray_Name = 0 then
            Generate_Abbrev (Abbrev_Ucarray_Name);
            Gen_Abbrev_Header (DW_TAG_Array_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Ucarray_Name);
         Emit_Decl_Ident (Decl);
      end if;
      Emit_Type_Ref (El_Type);

      if Abbrev_Uc_Subrange = 0 then
         Generate_Abbrev (Abbrev_Uc_Subrange);
         Gen_Abbrev_Header (DW_TAG_Subrange_Type, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (0, 0);
      end if;

      Gen_Info_Header (Abbrev_Uc_Subrange);
      Emit_Type_Ref (Idx_Type);

      Gen_Uleb128 (0);
   end Emit_Array_Type;

   procedure Emit_Ucarray_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
   begin
      Emit_Array_Type (Decl,
                       Get_Type_Ucarray_Element (Atype),
                       Get_Type_Ucarray_Index (Atype));
   end Emit_Ucarray_Type;

   procedure Emit_Subarray_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Udata);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;

      Base : O_Tnode;
   begin
      if Decl = O_Dnode_Null then
         if Abbrev_Subarray = 0 then
            Generate_Abbrev (Abbrev_Subarray);
            Gen_Abbrev_Header (DW_TAG_Array_Type, DW_CHILDREN_Yes);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Subarray);
      else
         if Abbrev_Subarray_Name = 0 then
            Generate_Abbrev (Abbrev_Subarray_Name);
            Gen_Abbrev_Header (DW_TAG_Array_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Subarray_Name);
         Emit_Decl_Ident (Decl);
      end if;


      Emit_Type_Ref (Get_Type_Subarray_Element (Atype));
      Gen_Uleb128 (Unsigned_32 (Get_Type_Size (Atype)));

      if Abbrev_Subrange = 0 then
         Generate_Abbrev (Abbrev_Subrange);
         Gen_Abbrev_Header (DW_TAG_Subrange_Type, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (DW_AT_Lower_Bound, DW_FORM_Data1);
         Gen_Abbrev_Tuple (DW_AT_Count, DW_FORM_Udata);
         Gen_Abbrev_Tuple (0, 0);
      end if;

      Gen_Info_Header (Abbrev_Subrange);
      Base := Get_Type_Subarray_Base (Atype);
      Emit_Type_Ref (Get_Type_Ucarray_Index (Base));
      Gen_8 (0);
      Gen_Uleb128 (Unsigned_32 (Get_Type_Subarray_Length (Atype)));

      Gen_Uleb128 (0);
   end Emit_Subarray_Type;

   procedure Emit_Members (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
      Nbr : Uns32;
      F : O_Fnode;
      Loc_Pc : Pc_Type;
      Sibling_Pc : Pc_Type;
      Sz : Uns32;
   begin
      if Abbrev_Member = 0 then
         Generate_Abbrev (Abbrev_Member);

         Gen_Abbrev_Header (DW_TAG_Member, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (DW_AT_Data_Member_Location, DW_FORM_Block1);
         Gen_Abbrev_Tuple (0, 0);
      end if;

      Set_Current_Section (Info_Sect);
      Sibling_Pc := Gen_Info_Sibling;
      Emit_Decl_Ident_If_Set (Decl);
      if Get_Type_Sized (Atype) then
         Sz := Get_Type_Size (Atype);
      else
         Sz := Get_Type_Record_Size (Atype);
      end if;
      Gen_Uleb128 (Unsigned_32 (Sz));

      Nbr := Get_Type_Record_Nbr_Fields (Atype);
      F := Get_Type_Record_Fields (Atype);
      while Nbr > 0 loop
         Gen_Uleb128 (Abbrev_Member);
         Emit_Ident (Get_Field_Ident (F));
         Emit_Type_Ref (Get_Field_Type (F));

         --  Location.
         Loc_Pc := Get_Current_Pc;
         Gen_8 (3);
         Gen_8 (DW_OP_Plus_Uconst);
         Gen_Uleb128 (Unsigned_32 (Get_Field_Offset (F)));
         Patch_8 (Loc_Pc, Unsigned_8 (Get_Current_Pc - (Loc_Pc + 1)));

         F := Get_Field_Chain (F);
         Nbr := Nbr - 1;
      end loop;

      --  end of children.
      Gen_Uleb128 (0);
      Patch_Info_Sibling (Sibling_Pc);
   end Emit_Members;

   procedure Emit_Record_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Udata);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;
   begin
      if Decl = O_Dnode_Null then
         if Abbrev_Struct = 0 then
            Generate_Abbrev (Abbrev_Struct);

            Gen_Abbrev_Header (DW_TAG_Structure_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Struct);
      else
         if Abbrev_Struct_Name = 0 then
            Generate_Abbrev (Abbrev_Struct_Name);

            Gen_Abbrev_Header (DW_TAG_Structure_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Struct_Name);
      end if;
      Emit_Members (Atype, Decl);
   end Emit_Record_Type;

   procedure Emit_Union_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Udata);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;
   begin
      if Decl = O_Dnode_Null then
         if Abbrev_Union = 0 then
            Generate_Abbrev (Abbrev_Union);

            Gen_Abbrev_Header (DW_TAG_Union_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Union);
      else
         if Abbrev_Union_Name = 0 then
            Generate_Abbrev (Abbrev_Union_Name);

            Gen_Abbrev_Header (DW_TAG_Union_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Union_Name);
      end if;
      Emit_Members (Atype, Decl);
   end Emit_Union_Type;

   procedure Emit_Enum_Type (Atype : O_Tnode; Decl : O_Dnode)
   is
      use Ortho_Code.Types;
      use Ortho_Code.Consts;
      procedure Finish_Gen_Abbrev is
      begin
         Gen_Abbrev_Tuple (DW_AT_Byte_Size, DW_FORM_Data1);
         Gen_Abbrev_Tuple (0, 0);
      end Finish_Gen_Abbrev;

      procedure Emit_Enumerator (L : O_Cnode) is
      begin
         Gen_Uleb128 (Abbrev_Enumerator);
         Emit_Ident (Get_Lit_Ident (L));
         Gen_Uleb128 (Unsigned_32 (Get_Lit_Value (L)));
      end Emit_Enumerator;

      Nbr : Uns32;
      L : O_Cnode;
      Sibling_Pc : Pc_Type;
   begin
      if Abbrev_Enumerator = 0 then
         Generate_Abbrev (Abbrev_Enumerator);

         Gen_Abbrev_Header (DW_TAG_Enumerator, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
         Gen_Abbrev_Tuple (DW_AT_Const_Value, DW_FORM_Udata);
         Gen_Abbrev_Tuple (0, 0);
      end if;
      if Decl = O_Dnode_Null then
         if Abbrev_Enum = 0 then
            Generate_Abbrev (Abbrev_Enum);
            Gen_Abbrev_Header (DW_TAG_Enumeration_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Enum);
      else
         if Abbrev_Enum_Name = 0 then
            Generate_Abbrev (Abbrev_Enum_Name);
            Gen_Abbrev_Header (DW_TAG_Enumeration_Type, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Finish_Gen_Abbrev;
         end if;
         Gen_Info_Header (Abbrev_Enum_Name);
      end if;

      Sibling_Pc := Gen_Info_Sibling;
      Emit_Decl_Ident_If_Set (Decl);
      Gen_8 (Byte (Get_Type_Size (Atype)));
      case Get_Type_Kind (Atype) is
         when OT_Enum =>
            Nbr := Get_Type_Enum_Nbr_Lits (Atype);
            L := Get_Type_Enum_Lits (Atype);
            while Nbr > 0 loop
               Emit_Enumerator (L);

               L := Get_Lit_Chain (L);
               Nbr := Nbr - 1;
            end loop;
         when OT_Boolean =>
            Emit_Enumerator (Get_Type_Bool_False (Atype));
            Emit_Enumerator (Get_Type_Bool_True (Atype));
         when others =>
            raise Program_Error;
      end case;

      --  End of children.
      Gen_Uleb128 (0);
      Patch_Info_Sibling (Sibling_Pc);
   end Emit_Enum_Type;

   procedure Emit_Type (Atype : O_Tnode)
   is
      use Ortho_Code.Types;
      Kind : OT_Kind;
      Decl : O_Dnode;
   begin
      if Flag_Debug < Debug_Dwarf then
         return;
      end if;

      --  If already emitted, then return.
      if Atype <= TOnodes.Last
        and then TOnodes.Table (Atype) /= Null_Pc
      then
         return;
      end if;

      Kind := Get_Type_Kind (Atype);

      --  First step: emit inner types (if any).
      case Kind is
         when OT_Signed
            | OT_Unsigned
            | OT_Float
            | OT_Boolean
            | OT_Enum =>
            null;
         when OT_Access =>
            null;
         when OT_Ucarray =>
            Emit_Type (Get_Type_Ucarray_Index (Atype));
            Emit_Type (Get_Type_Ucarray_Element (Atype));
         when OT_Subarray =>
            Emit_Type (Get_Type_Subarray_Base (Atype));
         when OT_Record
            | OT_Subrecord
            | OT_Union =>
            declare
               Nbr : Uns32;
               F : O_Fnode;
            begin
               Nbr := Get_Type_Record_Nbr_Fields (Atype);
               F := Get_Type_Record_Fields (Atype);
               while Nbr > 0 loop
                  Emit_Type (Get_Field_Type (F));
                  F := Get_Field_Chain (F);
                  Nbr := Nbr - 1;
               end loop;
            end;
         when OT_Complete =>
            null;
      end case;

      Set_Current_Section (Info_Sect);
      Add_Type_Ref (Atype, Get_Current_Pc);

      Decl := Decls.Get_Type_Decl (Atype);

      --  Second step: emit info.
      case Kind is
         when OT_Signed
            | OT_Unsigned
            | OT_Float =>
            Emit_Base_Type (Atype, Decl);
            -- base types.
         when OT_Access =>
            Emit_Access_Type (Atype, Decl);
         when OT_Ucarray =>
            Emit_Ucarray_Type (Atype, Decl);
         when OT_Subarray =>
            Emit_Subarray_Type (Atype, Decl);
         when OT_Record
            | OT_Subrecord =>
            Emit_Record_Type (Atype, Decl);
         when OT_Union =>
            Emit_Union_Type (Atype, Decl);
         when OT_Enum
            | OT_Boolean =>
            Emit_Enum_Type (Atype, Decl);
         when OT_Complete =>
            null;
      end case;
   end Emit_Type;

   procedure Emit_Decl_Type (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
   begin
      Emit_Type_Ref (Get_Decl_Type (Decl));
   end Emit_Decl_Type;

   Abbrev_Variable : Unsigned_32 := 0;
   Abbrev_Const : Unsigned_32 := 0;

   procedure Emit_Local_Location (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
      Pc : Pc_Type;
   begin
      Pc := Get_Current_Pc;
      Gen_8 (2);
      Gen_8 (DW_OP_Fbreg);
      Gen_Sleb128 (Get_Decl_Info (Decl));
      Patch_8 (Pc, Unsigned_8 (Get_Current_Pc - (Pc + 1)));
   end Emit_Local_Location;

   procedure Emit_Global_Location (Decl : O_Dnode)
   is
      use Ortho_Code.Binary;
   begin
      Gen_8 (1 + Pc_Type_Sizeof);
      Gen_8 (DW_OP_Addr);
      Gen_Ua_Addr (Get_Decl_Symbol (Decl), 0);
   end Emit_Global_Location;

   procedure Emit_Variable (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
      Dtype : O_Tnode;
   begin
      if Get_Decl_Ident (Decl) = O_Ident_Nul then
         return;
      end if;

      if Abbrev_Variable = 0 then
         Generate_Abbrev (Abbrev_Variable);
         Gen_Abbrev_Header (DW_TAG_Variable, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (DW_AT_Location, DW_FORM_Block1);
         Gen_Abbrev_Tuple (0, 0);
      end if;

      Dtype := Get_Decl_Type (Decl);
      Emit_Type (Dtype);

      Gen_Info_Header (Abbrev_Variable);
      Emit_Decl_Ident (Decl);
      Emit_Type_Ref (Dtype);
      case Get_Decl_Kind (Decl) is
         when OD_Local =>
            Emit_Local_Location (Decl);
         when OD_Var =>
            Emit_Global_Location (Decl);
         when others =>
            raise Program_Error;
      end case;
   end Emit_Variable;

   procedure Emit_Const (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
      Dtype : O_Tnode;
   begin
      if Abbrev_Const = 0 then
         Generate_Abbrev (Abbrev_Const);
         --  FIXME: should be a TAG_Constant, however, GDB does not support it.
         --  work-around: could use a const_type.
         Gen_Abbrev_Header (DW_TAG_Variable, DW_CHILDREN_No);

         Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
         Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
         Gen_Abbrev_Tuple (DW_AT_Location, DW_FORM_Block1);
         Gen_Abbrev_Tuple (0, 0);
      end if;

      Dtype := Get_Decl_Type (Decl);
      Emit_Type (Dtype);
      Gen_Info_Header (Abbrev_Const);
      Emit_Decl_Ident (Decl);
      Emit_Type_Ref (Dtype);
      Emit_Global_Location (Decl);
   end Emit_Const;

   procedure Emit_Type_Decl (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
   begin
      Emit_Type (Get_Decl_Type (Decl));
   end Emit_Type_Decl;

   Subprg_Sym : Symbol;

   Abbrev_Block : Unsigned_32 := 0;

   procedure Emit_Block_Decl (Decl : O_Dnode)
   is
      use Ortho_Code.Decls;
      Last : O_Dnode;
      Sdecl : O_Dnode;
      Sibling_Pc : Pc_Type;
   begin
      if Flag_Debug >= Debug_Dwarf then
         if Abbrev_Block = 0 then
            Generate_Abbrev (Abbrev_Block);

            Gen_Abbrev_Header (DW_TAG_Lexical_Block, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
            Gen_Abbrev_Tuple (DW_AT_Low_Pc, DW_FORM_Addr);
            Gen_Abbrev_Tuple (DW_AT_High_Pc, DW_FORM_Addr);
            Gen_Abbrev_Tuple (0, 0);
         end if;

         Gen_Info_Header (Abbrev_Block);
         Sibling_Pc := Gen_Info_Sibling;

         Gen_Ua_Addr (Subprg_Sym, Integer_32 (Get_Block_Info1 (Decl)));
         Gen_Ua_Addr (Subprg_Sym, Integer_32 (Get_Block_Info2 (Decl)));
      end if;

      --  Emit decls for children.
      Last := Get_Block_Last (Decl);
      Sdecl := Decl + 1;
      while Sdecl <= Last loop
         Emit_Decl (Sdecl);
         Sdecl := Get_Decl_Chain (Sdecl);
      end loop;

      if Flag_Debug >= Debug_Dwarf then
         --  End of children.
         Set_Current_Section (Info_Sect);
         Gen_Uleb128 (0);

         Patch_Info_Sibling (Sibling_Pc);
      end if;
   end Emit_Block_Decl;

   Abbrev_Function : Unsigned_32 := 0;
   Abbrev_Procedure : Unsigned_32 := 0;
   Abbrev_Interface : Unsigned_32 := 0;

   procedure Emit_Subprg_Body (Bod : O_Dnode)
   is
      use Ortho_Code.Decls;
      Decl : constant O_Dnode := Get_Body_Decl (Bod);
      Kind : constant OD_Kind := Get_Decl_Kind (Decl);
      Idecl : O_Dnode;
      Prev_Subprg_Sym : Symbol;
      Sibling_Pc : Pc_Type;
   begin
      --  Emit interfaces type.
      Idecl := Get_Subprg_Interfaces (Decl);
      while Idecl /= O_Dnode_Null loop
         Emit_Type (Get_Decl_Type (Idecl));
         Idecl := Get_Interface_Chain (Idecl);
      end loop;

      if Kind = OD_Function then
         Emit_Type (Get_Decl_Type (Decl));
         if Abbrev_Function = 0 then
            Generate_Abbrev (Abbrev_Function);

            Gen_Abbrev_Header (DW_TAG_Subprogram, DW_CHILDREN_Yes);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Gen_Abbrev_Tuple (DW_AT_Low_Pc, DW_FORM_Addr);
            Gen_Abbrev_Tuple (DW_AT_High_Pc, DW_FORM_Addr);

            if Flag_Debug >= Debug_Dwarf then
               Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
               Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
               Gen_Abbrev_Tuple (DW_AT_Frame_Base, DW_FORM_Block1);
            end if;
            --Gen_Abbrev_Tuple (DW_AT_Return_Addr, DW_FORM_Block1);
            Gen_Abbrev_Tuple (0, 0);
         end if;
         Gen_Info_Header (Abbrev_Function);
      else
         if Abbrev_Procedure = 0 then
            Generate_Abbrev (Abbrev_Procedure);

            Gen_Abbrev_Header (DW_TAG_Subprogram, DW_CHILDREN_Yes);

            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Gen_Abbrev_Tuple (DW_AT_Low_Pc, DW_FORM_Addr);
            Gen_Abbrev_Tuple (DW_AT_High_Pc, DW_FORM_Addr);
            if Flag_Debug >= Debug_Dwarf then
               Gen_Abbrev_Tuple (DW_AT_Sibling, DW_FORM_Ref4);
               Gen_Abbrev_Tuple (DW_AT_Frame_Base, DW_FORM_Block1);
            end if;
            --Gen_Abbrev_Tuple (DW_AT_Return_Addr, DW_FORM_Block1);
            Gen_Abbrev_Tuple (0, 0);
         end if;
         Gen_Info_Header (Abbrev_Procedure);
      end if;

      --  Name.
      Emit_Decl_Ident (Decl);

      --  Low, High.
      Prev_Subprg_Sym := Subprg_Sym;
      Subprg_Sym := Binary.Get_Decl_Symbol (Decl);
      Gen_Ua_Addr (Subprg_Sym, 0);
      Gen_Ua_Addr (Subprg_Sym, Integer_32 (Get_Body_Info (Bod)));

      if Flag_Debug >= Debug_Dwarf then
         --  Type.
         if Kind = OD_Function then
            Emit_Decl_Type (Decl);
         end if;

         --  Sibling.
         Sibling_Pc := Gen_Info_Sibling;

         --  Frame base.
         Gen_8 (1);
         case Arch is
            when Arch_X86 =>
               Gen_8 (DW_OP_Reg5); --  ebp
            when Arch_X86_64 =>
               Gen_8 (DW_OP_Reg6); --  rbp
            when others =>
               raise Program_Error;
         end case;
      end if;

      --  Interfaces.
      Idecl := Get_Subprg_Interfaces (Decl);
      if Idecl /= O_Dnode_Null
        and then Flag_Debug >= Debug_Dwarf
      then
         if Abbrev_Interface = 0 then
            Generate_Abbrev (Abbrev_Interface);

            Gen_Abbrev_Header (DW_TAG_Formal_Parameter, DW_CHILDREN_No);
            Gen_Abbrev_Tuple (DW_AT_Type, DW_FORM_Ref4);
            Gen_Abbrev_Tuple (DW_AT_Name, DW_FORM_String);
            Gen_Abbrev_Tuple (DW_AT_Location, DW_FORM_Block1);
            Gen_Abbrev_Tuple (0, 0);
         end if;

         loop
            Gen_Info_Header (Abbrev_Interface);
            Emit_Decl_Type (Idecl);
            Emit_Decl_Ident (Idecl);

            Emit_Local_Location (Idecl);

            Idecl := Get_Interface_Chain (Idecl);
            exit when Idecl = O_Dnode_Null;
         end loop;
      end if;

      --  Internal declarations.
      Emit_Block_Decl (Bod + 1);

      --  End of children.
      Gen_Uleb128 (0);

      if Flag_Debug >= Debug_Dwarf then
         Patch_Info_Sibling (Sibling_Pc);
      end if;

      Subprg_Sym := Prev_Subprg_Sym;
   end Emit_Subprg_Body;

   procedure Emit_Decl (Decl : O_Dnode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Decls;
   begin
      if Flag_Debug = Debug_Dwarf then
         case Get_Decl_Kind (Decl) is
            when OD_Type =>
               Emit_Type_Decl (Decl);
            when OD_Local
              | OD_Var =>
               Emit_Variable (Decl);
            when OD_Const =>
               Emit_Const (Decl);
            when OD_Function
              | OD_Procedure
              | OD_Interface =>
               null;
            when OD_Body =>
               Emit_Subprg_Body (Decl);
            when OD_Block =>
               Emit_Block_Decl (Decl);
            when others =>
               Put_Line ("dwarf.emit_decl: emit "
                           & OD_Kind'Image (Get_Decl_Kind (Decl)));
         end case;
      elsif Flag_Debug = Debug_Line then
         if Get_Decl_Kind (Decl) = OD_Body then
            Emit_Subprg_Body (Decl);
         end if;
      end if;
   end Emit_Decl;

   procedure Emit_Subprg (Bod : O_Dnode) is
   begin
      Emit_Decls_Until (Bod);
      Emit_Decl (Bod);
      Last_Decl := Decls.Get_Decl_Chain (Bod);
   end Emit_Subprg;

   procedure Mark (M : out Mark_Type) is
   begin
      M.Last_Decl := Last_Decl;
      M.Last_Tnode := TOnodes.Last;
   end Mark;

   procedure Release (M : Mark_Type) is
   begin
      Last_Decl := M.Last_Decl;
      TOnodes.Set_Last (M.Last_Tnode);
   end Release;

end Ortho_Code.Dwarf;
