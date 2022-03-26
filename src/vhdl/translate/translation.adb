--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with Interfaces; use Interfaces;
with Ortho_Nodes; use Ortho_Nodes;
with Ortho_Ident; use Ortho_Ident;
with Flags; use Flags;
with Types; use Types;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Name_Table; -- use Name_Table;
with Str_Table;
with Files_Map;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Sem_Specs;
with Libraries;
with Std_Names;
with Vhdl.Canon;
with Trans;
with Trans_Decls; use Trans_Decls;
with Trans.Chap1;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap7;
with Trans.Chap12;
with Trans.Rtis;
with Trans.Helpers2;

package body Translation is
   use Trans;
   use Trans.Chap10;
   use Trans.Helpers;
   use Trans.Helpers2;

   function Get_Ortho_Decl (Subprg : Iir) return O_Dnode is
   begin
      return Get_Info (Subprg).Subprg_Node;
   end Get_Ortho_Decl;

   function Get_Resolv_Ortho_Decl (Func : Iir) return O_Dnode
   is
      Info : Subprg_Resolv_Info_Acc;
   begin
      Info := Get_Info (Func).Subprg_Resolv;
      if Info = null then
         --  Maybe the resolver is not used.
         return O_Dnode_Null;
      else
         return Info.Resolv_Func;
      end if;
   end Get_Resolv_Ortho_Decl;

   function Get_String_As_String (Expr : Iir) return String is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_String_Literal8 =>
            declare
               Len : constant Natural := Natural (Get_String_Length (Expr));
               Id : constant String8_Id := Get_String8_Id (Expr);
               Res : String (1 .. Len);
            begin
               for I in 1 .. Len loop
                  Res (I) := Str_Table.Char_String8 (Id, Pos32 (I));
               end loop;
               return Res;
            end;
         when Iir_Kind_Simple_Aggregate =>
            declare
               List : constant Iir_Flist := Get_Simple_Aggregate_List (Expr);
               Len : constant Natural := Get_Nbr_Elements (List);
               Res : String (1 .. Len);
               El : Iir;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  pragma Assert (Get_Kind (El) = Iir_Kind_Enumeration_Literal);
                  Res (I - Flist_First + 1) :=
                    Character'Val (Get_Enum_Pos (El));
               end loop;
               return Res;
            end;
         when others =>
            if Get_Expr_Staticness (Expr) /= Locally then
               Error_Msg_Sem
                 (+Expr, "value of FOREIGN attribute must be locally static");
               return "";
            else
               raise Internal_Error;
            end if;
      end case;
   end Get_String_As_String;

   function Translate_Foreign_Id (Decl : Iir) return Foreign_Info_Type
   is
      --  Look for 'FOREIGN.
      Attr : constant Iir_Attribute_Value :=
        Vhdl.Sem_Specs.Find_Attribute_Value (Decl, Std_Names.Name_Foreign);
      pragma Assert (Attr /= Null_Iir);
      Spec : constant Iir_Attribute_Specification :=
        Get_Attribute_Specification (Attr);
      Name : constant String := Get_String_As_String (Get_Expression (Spec));
      Length : constant Natural := Name'Length;
   begin
      if Length = 0 then
         return Foreign_Bad;
      end if;

      pragma Assert (Name'First = 1);

      --  Only 'VHPIDIRECT' is recognized.
      if Length >= 10 and then Name (1 .. 10) = "VHPIDIRECT" then
         declare
            Info : Foreign_Info_Type (Foreign_Vhpidirect);
            P : Natural;
            Sf, Sl : Natural;
            Lf, Ll : Natural;
         begin
            P := 11;

            --  Skip spaces.
            while P <= Length and then Name (P) = ' ' loop
               P := P + 1;
            end loop;
            if P > Length then
               Error_Msg_Sem
                 (+Spec, "missing subprogram/library name after VHPIDIRECT");
               Info.Lib_Len := 0;
               Info.Subprg_Len := 0;
               return Info;
            end if;
            --  Extract library.
            Lf := P;
            while P <= Length and then Name (P) /= ' ' loop
               P := P + 1;
            end loop;
            Ll := P - 1;
            --  Extract subprogram.
            while P <= Length and then Name (P) = ' ' loop
               P := P + 1;
            end loop;
            Sf := P;
            while P <= Length and then Name (P) /= ' ' loop
               P := P + 1;
            end loop;
            Sl := P - 1;
            if P <= Length then
               Error_Msg_Sem (+Spec, "garbage at end of VHPIDIRECT");
            end if;

            --  Accept empty library.
            if Sf > Length then
               Sf := Lf;
               Sl := Ll;
               Lf := 1;
               Ll := 0;
            end if;

            Info.Lib_Len := Ll - Lf + 1;
            Info.Lib_Name (1 .. Info.Lib_Len) := Name (Lf .. Ll);

            Info.Subprg_Len := Sl - Sf + 1;
            Info.Subprg_Name (1 .. Info.Subprg_Len) := Name (Sf .. Sl);
            return Info;
         end;
      elsif Length = 14
        and then Name (1 .. 14) = "GHDL intrinsic"
      then
         return Foreign_Info_Type'(Kind => Foreign_Intrinsic);
      else
         Error_Msg_Sem
           (+Spec,
            "value of 'FOREIGN attribute does not begin with VHPIDIRECT");
         return Foreign_Bad;
      end if;
   end Translate_Foreign_Id;

   procedure Gen_Filename (Design_File : Iir)
   is
      Info : Design_File_Info_Acc;
   begin
      pragma Assert (Current_Filename_Node = O_Dnode_Null);

      Info := Get_Info (Design_File);
      if Info = null then
         Info := Add_Info (Design_File, Kind_Design_File);
         Info.Design_Filename := Create_String
           (Get_Design_File_Filename (Design_File),
            Create_Uniq_Identifier, O_Storage_Private);
      end if;
      Current_Filename_Node := Info.Design_Filename;
   end Gen_Filename;

   --  Decorate the tree in order to be usable with the internal simulator.
   procedure Translate (Unit : Iir_Design_Unit; Main : Boolean)
   is
      Design_File : constant Iir_Design_File := Get_Design_File (Unit);
      Lib_Unit : constant Iir := Get_Library_Unit (Unit);
      Lib : Iir_Library_Declaration;
      Lib_Mark, Ent_Mark, Sep_Mark, Unit_Mark : Id_Mark_Type;
      Id : Name_Id;
   begin
      Update_Node_Infos;

      if False then
         --  No translation for context items.
         declare
            El : Iir;
         begin
            El := Get_Context_Items (Unit);
            while El /= Null_Iir loop
               case Get_Kind (El) is
                  when Iir_Kind_Use_Clause =>
                     null;
                  when Iir_Kind_Library_Clause =>
                     null;
                  when others =>
                     Error_Kind ("translate1", El);
               end case;
               El := Get_Chain (El);
            end loop;
         end;
      end if;

      if Flags.Verbose then
         if Main then
            Report_Msg (Msgid_Note, Semantic, +Unit,
                        "translating (with code generation) %n",
                        (1 => +Lib_Unit));
         else
            Report_Msg (Msgid_Note, Semantic, +Unit,
                        "translating %n", (1 => +Lib_Unit));
         end if;
      end if;

      --  Create the prefix for identifiers.
      Lib := Get_Library (Get_Design_File (Unit));
      Reset_Identifier_Prefix;
      if Lib = Libraries.Work_Library then
         Id := Libraries.Work_Library_Name;
      else
         Id := Get_Identifier (Lib);
      end if;
      Push_Identifier_Prefix (Lib_Mark, Id);

      if Get_Kind (Lib_Unit) = Iir_Kind_Architecture_Body then
         --  Put 'ARCH' between the entity name and the architecture name, to
         --  avoid a name clash with names from entity (eg an entity port with
         --  the same name as an architecture).
         Push_Identifier_Prefix (Ent_Mark,
                                 Get_Identifier (Get_Entity (Lib_Unit)));
         Push_Identifier_Prefix (Sep_Mark, "ARCH");
      end if;
      Id := Get_Identifier (Lib_Unit);
      if Id /= Null_Identifier then
         Push_Identifier_Prefix (Unit_Mark, Id);
      end if;

      if Main then
         Set_Global_Storage (O_Storage_Public);
         --  Create the variable containing the current file name.
         Gen_Filename (Get_Design_File (Unit));
      else
         Set_Global_Storage (O_Storage_External);
      end if;

      declare
         Pathname : constant String := Files_Map.Get_Pathname
           (Get_Design_File_Directory (Design_File),
            Get_Design_File_Filename (Design_File));
      begin
         New_Debug_Filename_Decl (Pathname);
      end;

      Current_Library_Unit := Lib_Unit;

      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Package_Declaration =>
            New_Debug_Comment_Decl
              ("package declaration " & Image_Identifier (Lib_Unit));
            Chap2.Translate_Package_Declaration (Lib_Unit);
            if Get_Package_Origin (Lib_Unit) /= Null_Iir
              and then Get_Package_Body (Lib_Unit) /= Null_Iir
            then
               --  Corresponding body for package instantiation.
               Chap2.Translate_Package_Body (Get_Package_Body (Lib_Unit));
            end if;
         when Iir_Kind_Package_Body =>
            New_Debug_Comment_Decl
              ("package body " & Image_Identifier (Lib_Unit));
            Chap2.Translate_Package_Body (Lib_Unit);
         when Iir_Kind_Package_Instantiation_Declaration =>
            New_Debug_Comment_Decl
              ("package instantiation " & Image_Identifier (Lib_Unit));
            Chap2.Translate_Package_Instantiation_Declaration (Lib_Unit);
         when Iir_Kind_Entity_Declaration =>
            New_Debug_Comment_Decl ("entity " & Image_Identifier (Lib_Unit));
            Chap1.Translate_Entity_Declaration (Lib_Unit);
         when Iir_Kind_Architecture_Body =>
            New_Debug_Comment_Decl
              ("architecture " & Image_Identifier (Lib_Unit));
            Chap1.Translate_Architecture_Body (Lib_Unit);
         when Iir_Kind_Configuration_Declaration =>
            New_Debug_Comment_Decl
              ("configuration " & Image_Identifier (Lib_Unit));
            if Id = Null_Identifier then
               --  Default configuration.
               declare
                  Mark : Id_Mark_Type;
                  Mark_Entity : Id_Mark_Type;
                  Mark_Arch : Id_Mark_Type;
                  Mark_Sep : Id_Mark_Type;
                  Arch : Iir;
                  Entity : constant Iir := Get_Entity (Lib_Unit);
               begin
                  --  Note: this is done inside the architecture identifier.
                  Push_Identifier_Prefix
                    (Mark_Entity, Get_Identifier (Entity));
                  Arch := Get_Block_Specification
                    (Get_Block_Configuration (Lib_Unit));
                  Push_Identifier_Prefix (Mark_Sep, "ARCH");
                  Push_Identifier_Prefix (Mark_Arch, Get_Identifier (Arch));
                  Push_Identifier_Prefix
                    (Mark, Name_Table.Get_Identifier ("DEFAULT_CONFIG"));
                  --  Spec is built during translation of architecture.
                  Chap1.Translate_Configuration_Declaration_Body (Lib_Unit);
                  Pop_Identifier_Prefix (Mark);
                  Pop_Identifier_Prefix (Mark_Arch);
                  Pop_Identifier_Prefix (Mark_Sep);
                  Pop_Identifier_Prefix (Mark_Entity);
               end;
            else
               Chap1.Translate_Configuration_Declaration_Decl (Lib_Unit);
               Chap1.Translate_Configuration_Declaration_Body (Lib_Unit);
            end if;
         when Iir_Kind_Context_Declaration =>
            New_Debug_Comment_Decl ("context " & Image_Identifier (Lib_Unit));
            null;
         when others =>
            Error_Kind ("translate", Lib_Unit);
      end case;

      Current_Filename_Node := O_Dnode_Null;
      Current_Library_Unit := Null_Iir;

      if Id /= Null_Identifier then
         Pop_Identifier_Prefix (Unit_Mark);
      end if;
      if Get_Kind (Lib_Unit) = Iir_Kind_Architecture_Body then
         Pop_Identifier_Prefix (Sep_Mark);
         Pop_Identifier_Prefix (Ent_Mark);
      end if;
      Pop_Identifier_Prefix (Lib_Mark);
   end Translate;

   procedure Initialize
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      Init_Node_Infos;

      --  Set flags for canon.
      Vhdl.Canon.Canon_Flag_Add_Labels := True;

      --  Force to unnest subprograms is the code generator doesn't support
      --  nested subprograms.
      if not Ortho_Nodes.Has_Nested_Subprograms then
         Flag_Unnest_Subprograms := True;
      end if;

      New_Debug_Comment_Decl ("internal declarations, part 1");

      -- Create well known identifiers.
      Wki_This := Get_Identifier ("this");
      Wki_Size := Get_Identifier ("size");
      Wki_Res := Get_Identifier ("res");
      Wki_Dir_To := Get_Identifier ("dir_to");
      Wki_Dir_Downto := Get_Identifier ("dir_downto");
      Wki_Left := Get_Identifier ("left");
      Wki_Right := Get_Identifier ("right");
      Wki_Dir := Get_Identifier ("dir");
      Wki_Length := Get_Identifier ("length");
      Wki_I := Get_Identifier ("I");
      Wki_Instance := Get_Identifier ("INSTANCE");
      Wki_Arch_Instance := Get_Identifier ("ARCH_INSTANCE");
      Wki_Name := Get_Identifier ("NAME");
      Wki_Sig := Get_Identifier ("sig");
      Wki_Obj := Get_Identifier ("OBJ");
      Wki_Rti := Get_Identifier ("RTI");
      Wki_Parent := Get_Identifier ("parent");
      Wki_Filename := Get_Identifier ("filename");
      Wki_Line := Get_Identifier ("line");
      Wki_Lo := Get_Identifier ("lo");
      Wki_Hi := Get_Identifier ("hi");
      Wki_Mid := Get_Identifier ("mid");
      Wki_Cmp := Get_Identifier ("cmp");
      Wki_Upframe := Get_Identifier ("UPFRAME");
      Wki_Frame := Get_Identifier ("FRAME");
      Wki_Val := Get_Identifier ("val");
      Wki_L_Len := Get_Identifier ("l_len");
      Wki_R_Len := Get_Identifier ("r_len");
      Wki_Base := Get_Identifier ("BASE");
      Wki_Bounds := Get_Identifier ("BOUNDS");
      Wki_Locvars := Get_Identifier ("LOCVARS");
      Wki_Flag := Get_Identifier ("FLAG");

      Sizetype := New_Unsigned_Type (32);
      New_Type_Decl (Get_Identifier ("__ghdl_size_type"), Sizetype);

      --  Create __ghdl_index_type, which is the type for *all* array index.
      Ghdl_Index_Type := New_Unsigned_Type (32);
      New_Type_Decl (Get_Identifier ("__ghdl_index_type"), Ghdl_Index_Type);

      Ghdl_Index_0 := New_Unsigned_Literal (Ghdl_Index_Type, 0);
      Ghdl_Index_1 := New_Unsigned_Literal (Ghdl_Index_Type, 1);
      Ghdl_Index_2 := New_Unsigned_Literal (Ghdl_Index_Type, 2);
      Ghdl_Index_4 := New_Unsigned_Literal (Ghdl_Index_Type, 4);
      Ghdl_Index_8 := New_Unsigned_Literal (Ghdl_Index_Type, 8);

      Ghdl_I32_Type := New_Signed_Type (32);
      New_Type_Decl (Get_Identifier ("__ghdl_i32"), Ghdl_I32_Type);

      Ghdl_Real_Type := New_Float_Type;
      New_Type_Decl (Get_Identifier ("__ghdl_real"), Ghdl_Real_Type);

      Ghdl_I64_Type := New_Signed_Type (64);
      New_Type_Decl (Get_Identifier ("__ghdl_i64"), Ghdl_I64_Type);

      --  File index for elaborated file object.
      Ghdl_File_Index_Type := New_Unsigned_Type (32);
      New_Type_Decl (Get_Identifier ("__ghdl_file_index"),
                     Ghdl_File_Index_Type);
      Ghdl_File_Index_Ptr_Type := New_Access_Type (Ghdl_File_Index_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_file_index_ptr"),
                     Ghdl_File_Index_Ptr_Type);

      --  Create char, char [] and char *.
      Char_Type_Node := New_Unsigned_Type (8);
      New_Type_Decl (Get_Identifier ("__ghdl_char"), Char_Type_Node);

      Chararray_Type := New_Array_Type (Char_Type_Node, Ghdl_Index_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_chararray"), Chararray_Type);

      Char_Ptr_Type := New_Access_Type (Chararray_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_char_ptr"), Char_Ptr_Type);

      Ghdl_Index_Ptr_Align := New_Alignof (Char_Ptr_Type, Ghdl_Index_Type);

      Char_Ptr_Array_Type := New_Array_Type (Char_Ptr_Type, Ghdl_Index_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_char_ptr_array"),
                     Char_Ptr_Array_Type);

      Char_Ptr_Array_Ptr_Type := New_Access_Type (Char_Ptr_Array_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_char_ptr_array_ptr"),
                     Char_Ptr_Array_Ptr_Type);

      --  Generic pointer.
      Ghdl_Ptr_Type := New_Access_Type (Char_Type_Node);
      New_Type_Decl (Get_Identifier ("__ghdl_ptr"), Ghdl_Ptr_Type);

      --  Create record
      --     len : natural;
      --     str : C_String;
      --  end record;
      declare
         Constr : O_Element_List;
      begin
         Start_Record_Type (Constr);
         New_Record_Field (Constr, Ghdl_Str_Len_Type_Len_Field,
                           Get_Identifier ("len"), Ghdl_Index_Type);
         New_Record_Field
           (Constr, Ghdl_Str_Len_Type_Str_Field,
            Get_Identifier ("str"), Char_Ptr_Type);
         Finish_Record_Type (Constr, Ghdl_Str_Len_Type_Node);
         New_Type_Decl (Get_Identifier ("__ghdl_str_len"),
                        Ghdl_Str_Len_Type_Node);
      end;

      Ghdl_Str_Len_Array_Type_Node := New_Array_Type
        (Ghdl_Str_Len_Type_Node, Ghdl_Index_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_str_len_array"),
                     Ghdl_Str_Len_Array_Type_Node);

      -- Create type __ghdl_str_len_ptr is access all __ghdl_str_len
      Ghdl_Str_Len_Ptr_Node := New_Access_Type (Ghdl_Str_Len_Type_Node);
      New_Type_Decl (Get_Identifier ("__ghdl_str_len_ptr"),
                     Ghdl_Str_Len_Ptr_Node);

      -- Create type __ghdl_bool_type is (false, true)
      New_Boolean_Type (Ghdl_Bool_Type,
                        Get_Identifier ("false"),
                        Ghdl_Bool_False_Node,
                        Get_Identifier ("true"),
                        Ghdl_Bool_True_Node);
      New_Type_Decl (Get_Identifier ("__ghdl_bool_type"),
                     Ghdl_Bool_Type);

      --  __ghdl_bool_array is array (ghdl_index_type) of ghdl_bool_type
      Ghdl_Bool_Array_Type :=
        New_Array_Type (Ghdl_Bool_Type, Ghdl_Index_Type);
      New_Type_Decl
        (Get_Identifier ("__ghdl_bool_array_type"), Ghdl_Bool_Array_Type);

      --  __ghdl_bool_array_ptr is access __ghdl_bool_array;
      Ghdl_Bool_Array_Ptr := New_Access_Type (Ghdl_Bool_Array_Type);
      New_Type_Decl
        (Get_Identifier ("__ghdl_bool_array_ptr"), Ghdl_Bool_Array_Ptr);

      --  Create:
      --  type __ghdl_sizes_type is record
      --     size_val : ghdl_index_type;
      --     size_sig : ghdl_index_type;
      --  end record;
      declare
         Constr : O_Element_List;
      begin
         Start_Record_Type (Constr);
         New_Record_Field (Constr, Ghdl_Sizes_Val,
                           Get_Identifier ("size_val"), Ghdl_Index_Type);
         New_Record_Field (Constr, Ghdl_Sizes_Sig,
                           Get_Identifier ("size_sig"), Ghdl_Index_Type);
         Finish_Record_Type (Constr, Ghdl_Sizes_Type);
         New_Type_Decl (Get_Identifier ("__ghdl_sizes_type"),
                        Ghdl_Sizes_Type);
      end;

      --  __ghdl_sizes_ptr is access __ghdl_sizes_type;
      Ghdl_Sizes_Ptr := New_Access_Type (Ghdl_Sizes_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_sizes_ptr"), Ghdl_Sizes_Ptr);

      --  Create type ghdl_compare_type is (lt, eq, ge);
      declare
         Constr : O_Enum_List;
      begin
         Start_Enum_Type  (Constr, 8);
         New_Enum_Literal (Constr, Get_Identifier ("lt"), Ghdl_Compare_Lt);
         New_Enum_Literal (Constr, Get_Identifier ("eq"), Ghdl_Compare_Eq);
         New_Enum_Literal (Constr, Get_Identifier ("gt"), Ghdl_Compare_Gt);
         Finish_Enum_Type (Constr, Ghdl_Compare_Type);
         New_Type_Decl (Get_Identifier ("__ghdl_compare_type"),
                        Ghdl_Compare_Type);
      end;

      --  Create:
      --  type __ghdl_location is record
      --     file : char_ptr_type;
      --     line : ghdl_i32;
      --     col : ghdl_i32;
      --  end record;
      declare
         Constr : O_Element_List;
      begin
         Start_Record_Type (Constr);
         New_Record_Field
           (Constr, Ghdl_Location_Filename_Node, Wki_Filename, Char_Ptr_Type);
         New_Record_Field
           (Constr, Ghdl_Location_Line_Node, Wki_Line, Ghdl_I32_Type);
         New_Record_Field (Constr, Ghdl_Location_Col_Node,
                           Get_Identifier ("col"),
                           Ghdl_I32_Type);
         Finish_Record_Type (Constr, Ghdl_Location_Type_Node);
         New_Type_Decl (Get_Identifier ("__ghdl_location"),
                        Ghdl_Location_Type_Node);
      end;
      -- Create type __ghdl_location_ptr is access __ghdl_location;
      Ghdl_Location_Ptr_Node := New_Access_Type (Ghdl_Location_Type_Node);
      New_Type_Decl (Get_Identifier ("__ghdl_location_ptr"),
                     Ghdl_Location_Ptr_Node);

      --  Create type ghdl_dir_type is (dir_to, dir_downto);
      declare
         Constr : O_Enum_List;
      begin
         Start_Enum_Type (Constr, 8);
         New_Enum_Literal (Constr, Wki_Dir_To, Ghdl_Dir_To_Node);
         New_Enum_Literal (Constr, Wki_Dir_Downto, Ghdl_Dir_Downto_Node);
         Finish_Enum_Type (Constr, Ghdl_Dir_Type_Node);
         New_Type_Decl (Get_Identifier ("__ghdl_dir_type"),
                        Ghdl_Dir_Type_Node);
      end;

      --  Create __ghdl_signal_ptr (incomplete type).
      New_Uncomplete_Record_Type (Ghdl_Signal_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_signal"), Ghdl_Signal_Type);

      Ghdl_Signal_Ptr := New_Access_Type (Ghdl_Signal_Type);
      New_Type_Decl (Get_Identifier ("__ghdl_signal_ptr"), Ghdl_Signal_Ptr);

      --  Create void* __ghdl_alloc (unsigned size);
      Start_Function_Decl (Interfaces, Get_Identifier ("__ghdl_alloc"),
                           O_Storage_External, Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Size, Sizetype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Alloc_Ptr);

      --  procedure __ghdl_program_error (filename : char_ptr_type;
      --                                  line : ghdl_i32;
      --                                  code : ghdl_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_program_error"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("code"), Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Program_Error);

      --  procedure __ghdl_bound_check_failed (filename : char_ptr_type;
      --                                       line : ghdl_i32);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_bound_check_failed"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Bound_Check_Failed);

      --  procedure __ghdl_direction_check_failed (filename : char_ptr_type;
      --                                           line : ghdl_i32);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_direction_check_failed"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Direction_Check_Failed);

      --  procedure __ghdl_access_check_failed ();
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_access_check_failed"),
         O_Storage_External);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Access_Check_Failed);

      --  Secondary stack subprograms.
      --  function __ghdl_stack2_allocate (size : ghdl_index_type)
      --    return ghdl_ptr_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_stack2_allocate"),
         O_Storage_External, Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Size, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Stack2_Allocate);

      --  function __ghdl_stack2_mark return ghdl_ptr_type;
      Start_Function_Decl (Interfaces, Get_Identifier ("__ghdl_stack2_mark"),
                           O_Storage_External, Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Stack2_Mark);

      --  procedure __ghdl_stack2_release (mark : ghdl_ptr_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_stack2_release"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("mark"),
                          Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Stack2_Release);

      --  procedure __ghdl_memcpy (dest : ghdl_ptr_type;
      --                           src  : ghdl_ptr_type;
      --                           length : ghdl_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_memcpy"), O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("dest"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("src"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Length, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Memcpy);

      --  procedure __ghdl_deallocate (ptr : ghdl_ptr_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_deallocate"), O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Obj, Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Deallocate);

      -- function __ghdl_malloc (length : ghdl_index_type)
      --    return ghdl_ptr_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_malloc"), O_Storage_External,
         Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Length, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Malloc);

      -- function __ghdl_malloc0 (length : ghdl_index_type)
      --    return ghdl_ptr_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_malloc0"), O_Storage_External,
         Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Length, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Malloc0);

      --  function __ghdl_text_file_elaborate return file_index_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_file_elaborate"),
         O_Storage_External, Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_File_Elaborate);

      --  function __ghdl_file_elaborate (name : char_ptr_type)
      --                                 return file_index_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_file_elaborate"),
         O_Storage_External, Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Name, Char_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Elaborate);

      --  procedure __ghdl_file_finalize (file : file_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_file_finalize"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Finalize);

      --  procedure __ghdl_text_file_finalize (file : file_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_file_finalize"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_File_Finalize);

      declare
         procedure Create_Protected_Subprg
           (Name : String; Subprg : out O_Dnode)
         is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl (Interfaces, Param, Wki_Obj, Ghdl_Ptr_Type);
            Finish_Subprogram_Decl (Interfaces, Subprg);
         end Create_Protected_Subprg;
      begin
         --  procedure __ghdl_protected_enter (obj : ghdl_ptr_type);
         Create_Protected_Subprg
           ("__ghdl_protected_enter", Ghdl_Protected_Enter);

         --  procedure __ghdl_protected_leave (obj : ghdl_ptr_type);
         Create_Protected_Subprg
           ("__ghdl_protected_leave", Ghdl_Protected_Leave);

         Create_Protected_Subprg
           ("__ghdl_protected_init", Ghdl_Protected_Init);

         Create_Protected_Subprg
           ("__ghdl_protected_fini", Ghdl_Protected_Fini);
      end;

      if Flag_Rti then
         Rtis.Rti_Initialize;
      end if;

      --  procedure __ghdl_signal_name_rti
      --       (obj : ghdl_rti_access;
      --        ctxt : ghdl_rti_access;
      --        addr : ghdl_ptr_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_name_rti"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Obj, Rtis.Ghdl_Rti_Access);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("ctxt"),
                          Rtis.Ghdl_Rti_Access);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("addr"),
                          Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Name_Rti);

      declare
         --  procedure NAME (this : ghdl_ptr_type;
         --                  proc : ghdl_ptr_type;
         --                  ctxt : ghdl_rti_access;
         --                  addr : ghdl_ptr_type);
         procedure Create_Process_Register (Name : String; Res : out O_Dnode)
         is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl
              (Interfaces, Param, Wki_This, Ghdl_Ptr_Type);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("proc"), Ghdl_Ptr_Type);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("ctxt"),
                                Rtis.Ghdl_Rti_Access);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("addr"),
                                Ghdl_Ptr_Type);
            Finish_Subprogram_Decl (Interfaces, Res);
         end Create_Process_Register;
      begin
         Create_Process_Register ("__ghdl_process_register",
                                  Ghdl_Process_Register);
         Create_Process_Register ("__ghdl_sensitized_process_register",
                                  Ghdl_Sensitized_Process_Register);
         Create_Process_Register ("__ghdl_postponed_process_register",
                                  Ghdl_Postponed_Process_Register);
         Create_Process_Register
           ("__ghdl_postponed_sensitized_process_register",
            Ghdl_Postponed_Sensitized_Process_Register);
      end;

      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_finalize_register"),
         O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Wki_This, Ghdl_Ptr_Type);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("proc"), Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Finalize_Register);
   end Initialize;

   procedure Create_Signal_Subprograms (Suffix          : String;
                                        Val_Type        : O_Tnode;
                                        Create_Signal   : out O_Dnode;
                                        Init_Signal     : out O_Dnode;
                                        Simple_Assign   : out O_Dnode;
                                        Start_Assign    : out O_Dnode;
                                        Next_Assign     : out O_Dnode;
                                        Associate_Value : out O_Dnode;
                                        Add_Port_Driver : out O_Dnode;
                                        Driving_Value   : out O_Dnode;
                                        Force_Drv       : out O_Dnode;
                                        Force_Eff       : out O_Dnode)
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      --  function __ghdl_create_signal_XXX (val_ptr : ghdl_ptr_type;
      --                                     resolv_func : ghdl_ptr_type;
      --                                     resolv_inst : ghdl_ptr_type)
      --                                     return __ghdl_signal_ptr;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_create_signal_" & Suffix),
         O_Storage_External, Ghdl_Signal_Ptr);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("val_ptr"), Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("resolv_func"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("resolv_inst"),
                          Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Create_Signal);

      --  procedure __ghdl_signal_init_XXX (sign : __ghdl_signal_ptr;
      --                                    val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_init_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Init_Signal);

      --  procedure __ghdl_signal_simple_assign_XXX (sign : __ghdl_signal_ptr;
      --                                             val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_simple_assign_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Simple_Assign);

      --  procedure __ghdl_signal_start_assign_XXX (sign : __ghdl_signal_ptr;
      --                                            reject : std_time;
      --                                            val : VAL_TYPE;
      --                                            after : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_start_assign_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("reject"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Start_Assign);

      --  procedure __ghdl_signal_next_assign_XXX (sign : __ghdl_signal_ptr;
      --                                            val : VAL_TYPE;
      --                                            after : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_next_assign_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Next_Assign);

      --  procedure __ghdl_signal_associate_XXX (sign : __ghdl_signal_ptr;
      --                                        val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_associate_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Associate_Value);

      --  procedure __ghdl_signal_add_port_driver_XX (sign : __ghdl_signal_ptr;
      --                                              val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces,
         Get_Identifier ("__ghdl_signal_add_port_driver_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Add_Port_Driver);

      --  function __ghdl_signal_driving_value_XXX (sign : __ghdl_signal_ptr)
      --     return VAL_TYPE;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_driving_value_" & Suffix),
         O_Storage_External, Val_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Driving_Value);

      --  procedure __ghdl_signal_force_drv_XXX (sign : __ghdl_signal_ptr;
      --                                         val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_force_drv_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Force_Drv);

      --  procedure __ghdl_signal_force_eff_XXX (sign : __ghdl_signal_ptr;
      --                                         val : VAL_TYPE);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_force_eff_" & Suffix),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Val_Type);
      Finish_Subprogram_Decl (Interfaces, Force_Eff);
   end Create_Signal_Subprograms;

   --  procedure __ghdl_image_NAME (res : std_string_ptr_node;
   --                               val : VAL_TYPE;
   --                               rti : ghdl_rti_access);
   --
   --  function __ghdl_value_NAME (val : std_string_ptr_node;
   --                              rti : ghdl_rti_access);
   --      return VAL_TYPE;
   procedure Create_Image_Value_Subprograms (Name : String;
                                             Val_Type : O_Tnode;
                                             Has_Td : Boolean;
                                             Image_Subprg : out O_Dnode;
                                             Value_Subprg : out O_Dnode)
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_image_" & Name),
         O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("res"), Std_String_Ptr_Node);
      New_Interface_Decl
        (Interfaces, Param, Wki_Val, Val_Type);
      if Has_Td then
         New_Interface_Decl
           (Interfaces, Param, Wki_Rti, Rtis.Ghdl_Rti_Access);
      end if;
      Finish_Subprogram_Decl (Interfaces, Image_Subprg);

      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_value_" & Name),
         O_Storage_External, Val_Type);
      New_Interface_Decl
        (Interfaces, Param, Wki_Val, Std_String_Ptr_Node);
      if Has_Td then
         New_Interface_Decl
           (Interfaces, Param, Get_Identifier ("rti"), Rtis.Ghdl_Rti_Access);
      end if;
      Finish_Subprogram_Decl (Interfaces, Value_Subprg);
   end Create_Image_Value_Subprograms;

   --  function __ghdl_std_ulogic_match_NAME (l : __ghdl_e8; r : __ghdl_e8)
   --    return __ghdl_e8;
   procedure Create_Std_Ulogic_Match_Subprogram (Name : String;
                                                 Subprg : out O_Dnode)
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_std_ulogic_match_" & Name),
         O_Storage_External, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Left, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Right, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Subprg);
   end Create_Std_Ulogic_Match_Subprogram;

   --  function __ghdl_std_ulogic_array_match_NAME
   --    (l : __ghdl_ptr; l_len : ghdl_index_type;
   --     r : __ghdl_ptr; r_len : ghdl_index_type)
   --    return __ghdl_i32;
   procedure Create_Std_Ulogic_Array_Match_Subprogram (Name : String;
                                                       Subprg : out O_Dnode)
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_std_ulogic_array_match_" & Name),
         O_Storage_External, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Left, Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_L_Len, Ghdl_Index_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Right, Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_R_Len, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Subprg);
   end Create_Std_Ulogic_Array_Match_Subprogram;

   --  procedure NAME (res : std_string_ptr_node;
   --                  val : VAL_TYPE;
   --                  ARG2_NAME : ARG2_TYPE);
   procedure Create_To_String_Subprogram (Name : String;
                                          Subprg : out O_Dnode;
                                          Val_Type : O_Tnode;
                                          Arg2_Type : O_Tnode := O_Tnode_Null;
                                          Arg2_Id : O_Ident := O_Ident_Nul;
                                          Arg3_Type : O_Tnode := O_Tnode_Null;
                                          Arg3_Id : O_Ident := O_Ident_Nul)
   is
      Interfaces : O_Inter_List;
      Param : O_Dnode;
   begin
      Start_Procedure_Decl
        (Interfaces, Get_Identifier (Name), O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Wki_Res, Std_String_Ptr_Node);
      New_Interface_Decl
        (Interfaces, Param, Wki_Val, Val_Type);
      if Arg2_Type /= O_Tnode_Null then
         New_Interface_Decl
           (Interfaces, Param, Arg2_Id, Arg2_Type);
         if Arg3_Type /= O_Tnode_Null then
            New_Interface_Decl
              (Interfaces, Param, Arg3_Id, Arg3_Type);
         end if;
      end if;
      Finish_Subprogram_Decl (Interfaces, Subprg);
   end Create_To_String_Subprogram;

   --  Do internal declarations that need std.standard declarations.
   procedure Post_Initialize
   is
      Interfaces : O_Inter_List;
      Rec : O_Element_List;
      Param : O_Dnode;
      Info : Type_Info_Acc;
   begin
      New_Debug_Comment_Decl ("internal declarations, part 2");

      --  Remember some pervasive types.
      Info := Get_Info (String_Type_Definition);
      Std_String_Node := Info.Ortho_Type (Mode_Value);
      Std_String_Ptr_Node := Info.Ortho_Ptr_Type (Mode_Value);

      Std_Integer_Otype :=
        Get_Ortho_Type (Integer_Type_Definition, Mode_Value);
      Std_Real_Otype :=
        Get_Ortho_Type (Real_Type_Definition, Mode_Value);
      Std_Time_Otype := Get_Ortho_Type (Time_Type_Definition, Mode_Value);

      --  __ghdl_now : time;
      --  ??? maybe this should be a function ?
      New_Var_Decl (Ghdl_Now, Get_Identifier ("__ghdl_now"),
                    O_Storage_External, Std_Time_Otype);

      --  procedure __ghdl_assert_failed (str : __ghdl_array_template;
      --                                  severity : ghdl_int);
      --                                  loc : __ghdl_location_acc);

      --  procedure __ghdl_report (str : __ghdl_array_template;
      --                                  severity : ghdl_int);
      --                                  loc : __ghdl_location_acc);
      declare
         procedure Create_Report_Subprg (Name : String; Subprg : out O_Dnode)
         is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("msg"), Std_String_Ptr_Node);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("severity"),
               Get_Ortho_Type (Severity_Level_Type_Definition, Mode_Value));
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("location"),
                                Ghdl_Location_Ptr_Node);
            Finish_Subprogram_Decl (Interfaces, Subprg);
         end Create_Report_Subprg;

         procedure Create_Fail_Subprg (Name : String; Subprg : out O_Dnode) is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("location"),
                                Ghdl_Location_Ptr_Node);
            Finish_Subprogram_Decl (Interfaces, Subprg);
         end Create_Fail_Subprg;
      begin
         Create_Report_Subprg
           ("__ghdl_assert_failed", Ghdl_Assert_Failed);
         Create_Report_Subprg
           ("__ghdl_ieee_assert_failed", Ghdl_Ieee_Assert_Failed);
         Create_Report_Subprg ("__ghdl_psl_assert_failed",
                               Ghdl_Psl_Assert_Failed);
         Create_Report_Subprg ("__ghdl_psl_cover", Ghdl_Psl_Cover);
         Create_Report_Subprg ("__ghdl_psl_cover_failed",
                               Ghdl_Psl_Cover_Failed);
         Create_Report_Subprg ("__ghdl_report", Ghdl_Report);

         Create_Fail_Subprg ("__ghdl_psl_assume_failed",
                             Ghdl_Psl_Assume_Failed);
      end;

      --  procedure __ghdl_check_stack_allocation (size : __ghdl_index_type)
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_check_stack_allocation"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Check_Stack_Allocation);

      if Flag_Check_Stack_Allocation > 0 then
         Check_Stack_Allocation_Threshold :=
           New_Index_Lit (Unsigned_64 (Flag_Check_Stack_Allocation));
      else
         Check_Stack_Allocation_Threshold := O_Cnode_Null;
      end if;

      --  procedure __ghdl_integer_indexed_check_failed
      --   (filename : char_ptr_type;
      --    line : ghdl_i32;
      --    val : standard_integer;
      --    rng : integer_range_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_integer_index_check_failed"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Std_Integer_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("rng"),
                          Get_Info (Integer_Type_Definition).B.Range_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Integer_Index_Check_Failed);

      --  procedure __ghdl_text_write (file : __ghdl_file_index;
      --                               str  : std_string_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_write"), O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_Write);

      --  function __ghdl_text_read_length (file : __ghdl_file_index;
      --                                    str : std_string_ptr)
      --     return std__standard_integer;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_read_length"),
         O_Storage_External, Std_Integer_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_Read_Length);

      --  procedure __ghdl_write_scalar (file : __ghdl_file_index;
      --                                 ptr : __ghdl_ptr_type;
      --                                 length : __ghdl_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_write_scalar"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("ptr"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Length, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Write_Scalar);

      --  procedure __ghdl_read_scalar (file : __ghdl_file_index;
      --                                ptr : __ghdl_ptr_type;
      --                                length : __ghdl_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_read_scalar"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("ptr"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Length, Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Read_Scalar);

      --  function __ghdl_real_exp (left : std__standard__real;
      --                            right : std__standard__integer)
      --   return std__standard__real;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_real_exp"), O_Storage_External,
         Std_Real_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("left"),
                          Std_Real_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("right"),
                          Std_Integer_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Real_Exp);

      --  function __ghdl_i32_exp (left : ghdl_i32;
      --                           right : std__standard__integer)
      --   return ghdl_i32;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_i32_exp"), O_Storage_External,
         Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Left, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Right, Std_Integer_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_I32_Exp);

      --  function __ghdl_i64_exp (left : ghdl_i64;
      --                           right : std__standard__integer)
      --   return ghdl_i64;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_i64_exp"), O_Storage_External,
         Ghdl_I64_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Left, Ghdl_I64_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Right, Std_Integer_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_I64_Exp);

      --  procedure __ghdl_image_b1 (res : std_string_ptr_node;
      --                             val : ghdl_bool_type;
      --                             rti : ghdl_rti_access);
      Create_Image_Value_Subprograms
        ("b1", Ghdl_Bool_Type, True, Ghdl_Image_B1, Ghdl_Value_B1);

      --  procedure __ghdl_image_e8 (res : std_string_ptr_node;
      --                             val : ghdl_i32_type;
      --                             rti : ghdl_rti_access);
      Create_Image_Value_Subprograms
        ("e8", Ghdl_I32_Type, True, Ghdl_Image_E8, Ghdl_Value_E8);

      --  procedure __ghdl_image_e32 (res : std_string_ptr_node;
      --                             val : ghdl_i32_type;
      --                             rti : ghdl_rti_access);
      Create_Image_Value_Subprograms
        ("e32", Ghdl_I32_Type, True, Ghdl_Image_E32, Ghdl_Value_E32);

      --  procedure __ghdl_image_i32 (res : std_string_ptr_node;
      --                              val : ghdl_i32_type);
      Create_Image_Value_Subprograms
        ("i32", Ghdl_I32_Type, False, Ghdl_Image_I32, Ghdl_Value_I32);

      --  procedure __ghdl_image_i64 (res : std_string_ptr_node;
      --                              val : ghdl_i64_type);
      Create_Image_Value_Subprograms
        ("i64", Ghdl_I64_Type, False, Ghdl_Image_I64, Ghdl_Value_I64);

      --  procedure __ghdl_image_p32 (res : std_string_ptr_node;
      --                              val : ghdl_i32_type;
      --                             rti : ghdl_rti_access);
      Create_Image_Value_Subprograms
        ("p32", Ghdl_I32_Type, True, Ghdl_Image_P32, Ghdl_Value_P32);

      --  procedure __ghdl_image_p64 (res : std_string_ptr_node;
      --                              val : ghdl_i64_type;
      --                             rti : ghdl_rti_access);
      Create_Image_Value_Subprograms
        ("p64", Ghdl_I64_Type, True, Ghdl_Image_P64, Ghdl_Value_P64);

      --  procedure __ghdl_image_f64 (res : std_string_ptr_node;
      --                              val : ghdl_real_type);
      Create_Image_Value_Subprograms
        ("f64", Ghdl_Real_Type, False, Ghdl_Image_F64, Ghdl_Value_F64);

      -------------
      --  files  --
      -------------

      --  procedure __ghdl_text_file_open (file : file_index_type;
      --                                   mode : Ghdl_I32_Type;
      --                                   str : std__standard__string_PTR);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_file_open"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("mode"),
                          Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_File_Open);

      --  procedure __ghdl_file_open (file : file_index_type;
      --                              mode : Ghdl_I32_Type;
      --                              str : std__standard__string_PTR);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_file_open"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("mode"),
                          Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Open);

      --  function __ghdl_text_file_open_status
      --    (file : file_index_type;
      --     mode : Ghdl_I32_Type;
      --     str : std__standard__string_PTR)
      --     return ghdl_i32_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_file_open_status"),
         O_Storage_External, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("mode"),
                          Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_File_Open_Status);

      --  function __ghdl_file_open_status (file : file_index_type;
      --                                    mode : Ghdl_I32_Type;
      --                                    str : std__standard__string_PTR)
      --     return ghdl_i32_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_file_open_status"),
         O_Storage_External, Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("mode"),
                          Ghdl_I32_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("str"),
                          Std_String_Ptr_Node);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Open_Status);

      --  function __ghdl_file_endfile (file : file_index_type)
      --    return std_boolean_type_node;
      Start_Function_Decl (Interfaces, Get_Identifier ("__ghdl_file_endfile"),
                           O_Storage_External, Std_Boolean_Type_Node);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Endfile);

      --  procedure __ghdl_text_file_close (file : file_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_text_file_close"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Text_File_Close);

      --  procedure __ghdl_file_close (file : file_index_type);
      Start_Procedure_Decl (Interfaces, Get_Identifier ("__ghdl_file_close"),
                            O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Close);

      --  procedure __ghdl_file_flush (file : file_index_type);
      Start_Procedure_Decl (Interfaces, Get_Identifier ("__ghdl_file_flush"),
                            O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("file"),
                          Ghdl_File_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_File_Flush);

      ---------------
      --  signals  --
      ---------------

      --  procedure __ghdl_signal_create_resolution
      --    (func : ghdl_ptr_type;
      --     instance : ghdl_ptr_type;
      --     sig : ghdl_ptr_type;
      --     nbr_sig : ghdl_index_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_create_resolution"),
         O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("func"), Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Instance, Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Ptr_Type);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("nbr_sig"), Ghdl_Index_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Create_Resolution);

      --  Declarations for signals.
      --  Max length of a scalar type.
      --  Note: this type is not correctly aligned.  Restricted use only.
      --  type __ghdl_scalar_bytes is __ghdl_chararray (0 .. 8);
      Ghdl_Scalar_Bytes := New_Array_Subtype
        (Chararray_Type,
         Char_Type_Node,
         New_Unsigned_Literal (Ghdl_Index_Type, 8));
      New_Type_Decl (Get_Identifier ("__ghdl_scalar_bytes"),
                     Ghdl_Scalar_Bytes);

      --  Type __signal_signal is record
      Start_Uncomplete_Record_Type (Ghdl_Signal_Type, Rec);
      New_Record_Field (Rec, Ghdl_Signal_Driving_Value_Field,
                        Get_Identifier ("driving_value"),
                        Ghdl_Scalar_Bytes);
      New_Record_Field (Rec, Ghdl_Signal_Last_Value_Field,
                        Get_Identifier ("last_value"),
                        Ghdl_Scalar_Bytes);
      New_Record_Field (Rec, Ghdl_Signal_Last_Event_Field,
                        Get_Identifier ("last_event"),
                        Std_Time_Otype);
      New_Record_Field (Rec, Ghdl_Signal_Last_Active_Field,
                        Get_Identifier ("last_active"),
                        Std_Time_Otype);
      New_Record_Field (Rec, Ghdl_Signal_Value_Field,
                        Get_Identifier ("value"),
                        Ghdl_Ptr_Type);
      New_Record_Field (Rec, Ghdl_Signal_Event_Field,
                        Get_Identifier ("event"),
                        Std_Boolean_Type_Node);
      New_Record_Field (Rec, Ghdl_Signal_Active_Field,
                        Get_Identifier ("active"),
                        Std_Boolean_Type_Node);
      New_Record_Field (Rec, Ghdl_Signal_Has_Active_Field,
                        Get_Identifier ("has_active"),
                        Ghdl_Bool_Type);
      Finish_Record_Type (Rec, Ghdl_Signal_Type);

      Ghdl_Signal_Ptr_Ptr := New_Access_Type (Ghdl_Signal_Ptr);
      New_Type_Decl (Get_Identifier ("__ghdl_signal_ptr_ptr"),
                     Ghdl_Signal_Ptr_Ptr);

      --  procedure __ghdl_signal_merge_rti
      --       (sig : ghdl_signal_ptr; rti : ghdl_rti_access)
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_merge_rti"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Rti, Rtis.Ghdl_Rti_Access);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Merge_Rti);

      --  procedure __ghdl_signal_add_source (targ : __ghdl_signal_ptr;
      --                                      src : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_add_source"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("targ"),
                          Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("src"),
                          Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Add_Source);

      --  procedure __ghdl_signal_effective_value (targ : __ghdl_signal_ptr;
      --                                           src : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_effective_value"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("targ"),
                          Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("src"),
                          Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Effective_Value);

      --  procedure __ghdl_signal_set_disconnect (sig : __ghdl_signal_ptr;
      --                                          val : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_set_disconnect"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("time"), Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Set_Disconnect);

      --  procedure __ghdl_signal_disconnect (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_disconnect"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Disconnect);

      --  function __ghdl_signal_get_nbr_drivers (sig : __ghdl_signal_ptr)
      --                                          return ghdl_index_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_get_nbr_drivers"),
         O_Storage_External, Ghdl_Index_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Get_Nbr_Drivers);

      --  function __ghdl_signal_get_nbr_sources (sig : __ghdl_signal_ptr)
      --                                          return ghdl_index_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_get_nbr_ports"),
         O_Storage_External, Ghdl_Index_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Get_Nbr_Ports);

      --  function __ghdl_signal_read_driver (sig : __ghdl_signal_ptr;
      --                                      num : ghdl_index_type)
      --                                     return ghdl_ptr_type;
      declare
         procedure Create_Signal_Read (Name : String; Subprg : out O_Dnode) is
         begin
            Start_Function_Decl
              (Interfaces, Get_Identifier (Name),
               O_Storage_External, Ghdl_Ptr_Type);
            New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("num"), Ghdl_Index_Type);
            Finish_Subprogram_Decl (Interfaces, Subprg);
         end Create_Signal_Read;
      begin
         Create_Signal_Read
           ("__ghdl_signal_read_driver", Ghdl_Signal_Read_Driver);
         Create_Signal_Read
           ("__ghdl_signal_read_port", Ghdl_Signal_Read_Port);
      end;

      --  function __ghdl_signal_driving (sig : __ghdl_signal_ptr)
      --                                 return std_boolean;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_driving"),
         O_Storage_External, Std_Boolean_Type_Node);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Driving);

      --  procedure __ghdl_signal_simple_assign_error
      --              (sig : __ghdl_signal_ptr;
      --               filename : char_ptr_type;
      --               line : ghdl_i32);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_simple_assign_error"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Simple_Assign_Error);

      --  procedure __ghdl_signal_start_assign_error (sign : __ghdl_signal_ptr;
      --                                              reject : std_time;
      --                                              after : std_time;
      --                                              filename : char_ptr_type;
      --                                              line : ghdl_i32);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_start_assign_error"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("reject"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Start_Assign_Error);

      --  procedure __ghdl_signal_next_assign_error (sig : __ghdl_signal_ptr;
      --                                             after : std_time;
      --                                             filename : char_ptr_type;
      --                                             line : ghdl_i32);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_next_assign_error"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Next_Assign_Error);

      --  procedure __ghdl_signal_start_assign_null (sig : __ghdl_signal_ptr;
      --                                             reject : std_time;
      --                                             after : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_start_assign_null"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("reject"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Start_Assign_Null);

      --  procedure __ghdl_signal_next_assign_null (sig : __ghdl_signal_ptr;
      --                                            after : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_next_assign_null"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("after"),
                          Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Next_Assign_Null);

      --  function __ghdl_create_signal_e8 (init_val : ghdl_i32_type)
      --                                    return __ghdl_signal_ptr;
      --  procedure __ghdl_signal_simple_assign_e8 (sign : __ghdl_signal_ptr;
      --                                            val : __ghdl_integer);
      Create_Signal_Subprograms ("e8", Ghdl_I32_Type,
                                 Ghdl_Create_Signal_E8,
                                 Ghdl_Signal_Init_E8,
                                 Ghdl_Signal_Simple_Assign_E8,
                                 Ghdl_Signal_Start_Assign_E8,
                                 Ghdl_Signal_Next_Assign_E8,
                                 Ghdl_Signal_Associate_E8,
                                 Ghdl_Signal_Add_Port_Driver_E8,
                                 Ghdl_Signal_Driving_Value_E8,
                                 Ghdl_Signal_Force_Drv_E8,
                                 Ghdl_Signal_Force_Eff_E8);

      --  function __ghdl_create_signal_e32 (init_val : ghdl_i32_type)
      --                                     return __ghdl_signal_ptr;
      --  procedure __ghdl_signal_simple_assign_e32 (sign : __ghdl_signal_ptr;
      --                                             val : __ghdl_integer);
      Create_Signal_Subprograms ("e32", Ghdl_I32_Type,
                                 Ghdl_Create_Signal_E32,
                                 Ghdl_Signal_Init_E32,
                                 Ghdl_Signal_Simple_Assign_E32,
                                 Ghdl_Signal_Start_Assign_E32,
                                 Ghdl_Signal_Next_Assign_E32,
                                 Ghdl_Signal_Associate_E32,
                                 Ghdl_Signal_Add_Port_Driver_E32,
                                 Ghdl_Signal_Driving_Value_E32,
                                 Ghdl_Signal_Force_Drv_E32,
                                 Ghdl_Signal_Force_Eff_E32);

      --  function __ghdl_create_signal_b1 (init_val : ghdl_bool_type)
      --                                    return __ghdl_signal_ptr;
      --  procedure __ghdl_signal_simple_assign_b1 (sign : __ghdl_signal_ptr;
      --                                            val : ghdl_bool_type);
      Create_Signal_Subprograms ("b1", Ghdl_Bool_Type,
                                 Ghdl_Create_Signal_B1,
                                 Ghdl_Signal_Init_B1,
                                 Ghdl_Signal_Simple_Assign_B1,
                                 Ghdl_Signal_Start_Assign_B1,
                                 Ghdl_Signal_Next_Assign_B1,
                                 Ghdl_Signal_Associate_B1,
                                 Ghdl_Signal_Add_Port_Driver_B1,
                                 Ghdl_Signal_Driving_Value_B1,
                                 Ghdl_Signal_Force_Drv_B1,
                                 Ghdl_Signal_Force_Eff_B1);

      Create_Signal_Subprograms ("i32", Ghdl_I32_Type,
                                 Ghdl_Create_Signal_I32,
                                 Ghdl_Signal_Init_I32,
                                 Ghdl_Signal_Simple_Assign_I32,
                                 Ghdl_Signal_Start_Assign_I32,
                                 Ghdl_Signal_Next_Assign_I32,
                                 Ghdl_Signal_Associate_I32,
                                 Ghdl_Signal_Add_Port_Driver_I32,
                                 Ghdl_Signal_Driving_Value_I32,
                                 Ghdl_Signal_Force_Drv_I32,
                                 Ghdl_Signal_Force_Eff_I32);

      Create_Signal_Subprograms ("f64", Ghdl_Real_Type,
                                 Ghdl_Create_Signal_F64,
                                 Ghdl_Signal_Init_F64,
                                 Ghdl_Signal_Simple_Assign_F64,
                                 Ghdl_Signal_Start_Assign_F64,
                                 Ghdl_Signal_Next_Assign_F64,
                                 Ghdl_Signal_Associate_F64,
                                 Ghdl_Signal_Add_Port_Driver_F64,
                                 Ghdl_Signal_Driving_Value_F64,
                                 Ghdl_Signal_Force_Drv_F64,
                                 Ghdl_Signal_Force_Eff_F64);

      Create_Signal_Subprograms ("i64", Ghdl_I64_Type,
                                 Ghdl_Create_Signal_I64,
                                 Ghdl_Signal_Init_I64,
                                 Ghdl_Signal_Simple_Assign_I64,
                                 Ghdl_Signal_Start_Assign_I64,
                                 Ghdl_Signal_Next_Assign_I64,
                                 Ghdl_Signal_Associate_I64,
                                 Ghdl_Signal_Add_Port_Driver_I64,
                                 Ghdl_Signal_Driving_Value_I64,
                                 Ghdl_Signal_Force_Drv_I64,
                                 Ghdl_Signal_Force_Eff_I64);

      --  procedure __ghdl_signal_release_drv (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_release_drv"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Release_Drv);

      --  procedure __ghdl_signal_release_eff (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_release_eff"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Release_Eff);

      --  procedure __ghdl_process_add_sensitivity (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_add_sensitivity"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Add_Sensitivity);

      --  procedure __ghdl_process_add_driver (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_add_driver"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Add_Driver);

      --  procedure __ghdl_signal_add_direct_driver (sig : __ghdl_signal_ptr;
      --                                             Drv : Ghdl_Ptr_type);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_add_direct_driver"),
         O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      New_Interface_Decl
        (Interfaces, Param, Get_Identifier ("drv"), Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Add_Direct_Driver);

      --  procedure __ghdl_signal_direct_assign (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_direct_assign"),
         O_Storage_External);
      New_Interface_Decl
        (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Direct_Assign);

      declare
         procedure Create_Signal_Conversion (Name : String; Res : out O_Dnode)
         is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("func"), Ghdl_Ptr_Type);
            New_Interface_Decl
              (Interfaces, Param, Wki_Instance, Ghdl_Ptr_Type);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("src"), Ghdl_Signal_Ptr);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("src_len"), Ghdl_Index_Type);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("dst"), Ghdl_Signal_Ptr);
            New_Interface_Decl
              (Interfaces, Param, Get_Identifier ("dst_len"), Ghdl_Index_Type);
            Finish_Subprogram_Decl (Interfaces, Res);
         end Create_Signal_Conversion;
      begin
         --  procedure __ghdl_signal_in_conversion (func : ghdl_ptr_type;
         --                                         instance : ghdl_ptr_type;
         --                                         src : ghdl_signal_ptr;
         --                                         src_len : ghdl_index_type;
         --                                         dst : ghdl_signal_ptr;
         --                                         dst_len : ghdl_index_type);
         Create_Signal_Conversion
           ("__ghdl_signal_in_conversion", Ghdl_Signal_In_Conversion);
         Create_Signal_Conversion
           ("__ghdl_signal_out_conversion", Ghdl_Signal_Out_Conversion);
      end;

      declare
         --  function __ghdl_create_XXX_signal (val_ptr : ghdl_ptr_type;
         --                                     val : std_time)
         --    return __ghdl_signal_ptr;
         procedure Create_Signal_Attribute (Name : String; Res : out O_Dnode)
         is
         begin
            Start_Function_Decl (Interfaces, Get_Identifier (Name),
                                 O_Storage_External, Ghdl_Signal_Ptr);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("val_ptr"),
                                Ghdl_Ptr_Type);
            New_Interface_Decl (Interfaces, Param, Wki_Val, Std_Time_Otype);
            Finish_Subprogram_Decl (Interfaces, Res);
         end Create_Signal_Attribute;
      begin
         --  function __ghdl_create_stable_signal (val_ptr : ghdl_ptr_type;
         --                                        val : std_time)
         --    return __ghdl_signal_ptr;
         Create_Signal_Attribute
           ("__ghdl_create_stable_signal", Ghdl_Create_Stable_Signal);

         --  function __ghdl_create_quiet_signal (val_ptr : ghdl_ptr_type;
         --                                       val : std_time)
         --    return __ghdl_signal_ptr;
         Create_Signal_Attribute
           ("__ghdl_create_quiet_signal", Ghdl_Create_Quiet_Signal);

         --  function __ghdl_create_transaction_signal
         --     (val_ptr : ghdl_ptr_type)
         --    return __ghdl_signal_ptr;
         Start_Function_Decl
           (Interfaces, Get_Identifier ("__ghdl_create_transaction_signal"),
            O_Storage_External, Ghdl_Signal_Ptr);
         New_Interface_Decl (Interfaces, Param, Get_Identifier ("val_ptr"),
                             Ghdl_Ptr_Type);
         Finish_Subprogram_Decl (Interfaces, Ghdl_Create_Transaction_Signal);
      end;

      --  procedure __ghdl_signal_attribute_register_prefix
      --    (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces,
         Get_Identifier ("__ghdl_signal_attribute_register_prefix"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl
        (Interfaces, Ghdl_Signal_Attribute_Register_Prefix);

      --  function __ghdl_create_delayed_signal (sig : __ghdl_signal_ptr;
      --                                         val_ptr : ghdl_ptr_type;
      --                                         val : std_time)
      --    return __ghdl_signal_ptr;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_create_delayed_signal"),
         O_Storage_External, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("sig"),
                          Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("val_ptr"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Val, Std_Time_Otype);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Create_Delayed_Signal);

      --  function __ghdl_signal_create_guard
      --    (val_ptr : Ghdl_Ptr_type;
      --     this : ghdl_ptr_type;
      --     proc : ghdl_ptr_type;
      --     instance_name : __ghdl_instance_name_acc)
      --    return __ghdl_signal_ptr;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_create_guard"),
         O_Storage_External, Ghdl_Signal_Ptr);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("val_ptr"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("this"),
                          Ghdl_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("proc"),
                          Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Create_Guard);

      --  procedure __ghdl_signal_guard_dependence (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_signal_guard_dependence"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Signal_Guard_Dependence);

      --  procedure __ghdl_process_wait_exit (void);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_exit"),
         O_Storage_External);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Exit);

      --  void __ghdl_process_wait_timeout (time : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_timeout"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("time"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Timeout);

      --  void __ghdl_process_wait_set_timeout (time : std_time);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_set_timeout"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("time"),
                          Std_Time_Otype);
      New_Interface_Decl (Interfaces, Param, Wki_Filename, Char_Ptr_Type);
      New_Interface_Decl (Interfaces, Param, Wki_Line, Ghdl_I32_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Set_Timeout);

      --  void __ghdl_process_wait_add_sensitivity (sig : __ghdl_signal_ptr);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_add_sensitivity"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Sig, Ghdl_Signal_Ptr);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Add_Sensitivity);

      --  procedure __ghdl_process_wait_suspend (void);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_suspend"),
         O_Storage_External);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Suspend);

      --  function __ghdl_process_wait_timed_out return __ghdl_bool_type;
      Start_Function_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_timed_out"),
         O_Storage_External, Ghdl_Bool_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Timed_Out);

      --  void __ghdl_process_wait_close (void);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_process_wait_close"),
         O_Storage_External);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Process_Wait_Close);

      declare
         procedure Create_Get_Name (Name : String; Res : out O_Dnode)
         is
         begin
            Start_Procedure_Decl
              (Interfaces, Get_Identifier (Name), O_Storage_External);
            New_Interface_Decl
              (Interfaces, Param, Wki_Res, Std_String_Ptr_Node);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("ctxt"),
                                Rtis.Ghdl_Rti_Access);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("addr"),
                                Ghdl_Ptr_Type);
            New_Interface_Decl (Interfaces, Param, Get_Identifier ("name"),
                                Ghdl_Str_Len_Ptr_Node);
            Finish_Subprogram_Decl (Interfaces, Res);
         end Create_Get_Name;
      begin
         -- procedure __ghdl_get_path_name (res : std_string_ptr_node;
         --                                 ctxt : ghdl_rti_access;
         --                                 addr : ghdl_ptr_type;
         --                                 name : __ghdl_str_len_ptr);
         Create_Get_Name ("__ghdl_get_path_name", Ghdl_Get_Path_Name);

         -- procedure __ghdl_get_instance_name (res : std_string_ptr_node;
         --                                     ctxt : ghdl_rti_access;
         --                                     addr : ghdl_ptr_type;
         --                                     name : __ghdl_str_len_ptr);
         Create_Get_Name ("__ghdl_get_instance_name", Ghdl_Get_Instance_Name);
      end;

      --  procedure __ghdl_rti_add_package (rti : ghdl_rti_access)
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_rti_add_package"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Wki_Rti, Rtis.Ghdl_Rti_Access);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Rti_Add_Package);

      --  procedure __ghdl_rti_add_top (max_pkgs : ghdl_index_type;
      --                                pkgs : ghdl_rti_arr_acc);
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_rti_add_top"),
         O_Storage_External);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("max_pkgs"),
                          Ghdl_Index_Type);
      New_Interface_Decl (Interfaces, Param, Get_Identifier ("pkgs"),
                          Rtis.Ghdl_Rti_Arr_Acc);
      New_Interface_Decl (Interfaces, Param, Wki_Rti, Rtis.Ghdl_Rti_Access);
      New_Interface_Decl
        (Interfaces, Param, Wki_Instance, Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Rti_Add_Top);

      --  procedure __ghdl_init_top_generics();
      Start_Procedure_Decl
        (Interfaces, Get_Identifier ("__ghdl_init_top_generics"),
         O_Storage_External);
      Finish_Subprogram_Decl (Interfaces, Ghdl_Init_Top_Generics);

      --  Create match subprograms for std_ulogic type.
      Create_Std_Ulogic_Match_Subprogram ("eq", Ghdl_Std_Ulogic_Match_Eq);
      Create_Std_Ulogic_Match_Subprogram ("ne", Ghdl_Std_Ulogic_Match_Ne);
      Create_Std_Ulogic_Match_Subprogram ("lt", Ghdl_Std_Ulogic_Match_Lt);
      Create_Std_Ulogic_Match_Subprogram ("le", Ghdl_Std_Ulogic_Match_Le);
      Create_Std_Ulogic_Match_Subprogram ("ge", Ghdl_Std_Ulogic_Match_Ge);
      Create_Std_Ulogic_Match_Subprogram ("gt", Ghdl_Std_Ulogic_Match_Gt);

      Create_Std_Ulogic_Array_Match_Subprogram
        ("eq", Ghdl_Std_Ulogic_Array_Match_Eq);
      Create_Std_Ulogic_Array_Match_Subprogram
        ("ne", Ghdl_Std_Ulogic_Array_Match_Ne);

      --  Create To_String subprograms.
      Create_To_String_Subprogram
        ("__ghdl_to_string_i32", Ghdl_To_String_I32, Ghdl_I32_Type);
      Create_To_String_Subprogram
        ("__ghdl_to_string_i64", Ghdl_To_String_I64, Ghdl_I64_Type);
      Create_To_String_Subprogram
        ("__ghdl_to_string_f64", Ghdl_To_String_F64, Ghdl_Real_Type);
      Create_To_String_Subprogram
        ("__ghdl_to_string_f64_digits", Ghdl_To_String_F64_Digits,
         Ghdl_Real_Type, Ghdl_I32_Type, Get_Identifier ("nbr_digits"));
      Create_To_String_Subprogram
        ("__ghdl_to_string_f64_format", Ghdl_To_String_F64_Format,
         Ghdl_Real_Type, Std_String_Ptr_Node, Get_Identifier ("format"));
      declare
         Bv_Base_Ptr : constant O_Tnode :=
           Get_Info (Bit_Vector_Type_Definition).B.Base_Ptr_Type (Mode_Value);
      begin
         Create_To_String_Subprogram
           ("__ghdl_bv_to_ostring", Ghdl_BV_To_Ostring,
            Bv_Base_Ptr, Ghdl_Index_Type, Wki_Length);
         Create_To_String_Subprogram
           ("__ghdl_bv_to_hstring", Ghdl_BV_To_Hstring,
            Bv_Base_Ptr, Ghdl_Index_Type, Wki_Length);
      end;
      Create_To_String_Subprogram
        ("__ghdl_to_string_b1", Ghdl_To_String_B1, Ghdl_Bool_Type,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_to_string_e8", Ghdl_To_String_E8, Ghdl_I32_Type,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_to_string_char", Ghdl_To_String_Char,
         Get_Ortho_Type (Character_Type_Definition, Mode_Value));
      Create_To_String_Subprogram
        ("__ghdl_to_string_e32", Ghdl_To_String_E32, Ghdl_I32_Type,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_to_string_p32", Ghdl_To_String_P32, Ghdl_I32_Type,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_to_string_p64", Ghdl_To_String_P64, Ghdl_I64_Type,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_time_to_string_unit", Ghdl_Time_To_String_Unit,
         Std_Time_Otype, Std_Time_Otype, Get_Identifier ("unit"),
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_array_char_to_string_b1", Ghdl_Array_Char_To_String_B1,
         Ghdl_Ptr_Type, Ghdl_Index_Type, Wki_Length,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_array_char_to_string_e8", Ghdl_Array_Char_To_String_E8,
         Ghdl_Ptr_Type, Ghdl_Index_Type, Wki_Length,
         Rtis.Ghdl_Rti_Access, Wki_Rti);
      Create_To_String_Subprogram
        ("__ghdl_array_char_to_string_e32", Ghdl_Array_Char_To_String_E32,
         Ghdl_Ptr_Type, Ghdl_Index_Type, Wki_Length,
         Rtis.Ghdl_Rti_Access, Wki_Rti);

   end Post_Initialize;

   procedure Translate_Type_Implicit_Subprograms
     (Decl : in out Iir; Main : Boolean)
   is
      Infos : Chap7.Implicit_Subprogram_Infos;
      Subprg_Kind : Subprg_Translate_Kind;
   begin
      pragma Assert (Get_Kind (Decl) in Iir_Kinds_Type_Declaration);

      if Main then
         Subprg_Kind := Subprg_Translate_Spec_And_Body;
      else
         Subprg_Kind := Subprg_Translate_Only_Spec;
      end if;
      Chap3.Translate_Type_Subprograms (Decl, Subprg_Kind);

      --  Skip type declaration.
      Decl := Get_Chain (Decl);

      --  Implicit subprograms are immediately follow the type declaration.
      Chap7.Init_Implicit_Subprogram_Infos (Infos);
      while Decl /= Null_Iir loop
         if Get_Kind (Decl) in Iir_Kinds_Subprogram_Declaration
           and then Is_Implicit_Subprogram (Decl)
         then
            Chap7.Translate_Implicit_Subprogram_Spec (Decl, Infos);
            Chap7.Translate_Implicit_Subprogram_Body (Decl);
            Decl := Get_Chain (Decl);
         else
            exit;
         end if;
      end loop;
   end Translate_Type_Implicit_Subprograms;

   procedure Translate_Standard (Main : Boolean)
   is
      Lib_Mark, Unit_Mark : Id_Mark_Type;
      Info : Ortho_Info_Acc;
      pragma Unreferenced (Info);
      Decl : Iir;
      Time_Type_Staticness : Iir_Staticness;
      Time_Subtype_Staticness : Iir_Staticness;
   begin
      Update_Node_Infos;

      New_Debug_Comment_Decl ("package std.standard");
      if Main then
         Gen_Filename (Std_Standard_File);
         Set_Global_Storage (O_Storage_Public);
      else
         Set_Global_Storage (O_Storage_External);
      end if;

      Info := Add_Info (Standard_Package, Kind_Package);

      Reset_Identifier_Prefix;
      Push_Identifier_Prefix
        (Lib_Mark, Get_Identifier (Libraries.Std_Library));
      Push_Identifier_Prefix
        (Unit_Mark, Get_Identifier (Standard_Package));

      --  With VHDL93 and later, time type is globally static.  As a result,
      --  it will be elaborated at run-time (and not statically).
      --  However, there is no elaboration of std.standard.  Furthermore,
      --  time type can be pre-elaborated without any difficulties.
      --  There is a kludge here:  set type staticess of time type locally
      --  and then revert it just after its translation.
      Time_Type_Staticness := Get_Type_Staticness (Time_Type_Definition);
      Time_Subtype_Staticness := Get_Type_Staticness (Time_Subtype_Definition);
      if Flags.Flag_Time_64 then
         Set_Type_Staticness (Time_Type_Definition, Locally);
      end if;
      Set_Type_Staticness (Time_Subtype_Definition, Locally);
      if Flags.Vhdl_Std > Vhdl_87 then
         Set_Type_Staticness (Delay_Length_Subtype_Definition, Locally);
      end if;

      Decl := Get_Declaration_Chain (Standard_Package);

      --  The first (and one of the most important) declaration is the
      --  boolean type declaration.
      pragma Assert (Decl = Boolean_Type_Declaration);
      Chap4.Translate_Bool_Type_Declaration (Boolean_Type_Declaration);
      --  We need this type very early, for predefined functions.
      Std_Boolean_Type_Node :=
        Get_Ortho_Type (Boolean_Type_Definition, Mode_Value);
      Std_Boolean_True_Node := Get_Ortho_Literal (Boolean_True);
      Std_Boolean_False_Node := Get_Ortho_Literal (Boolean_False);

      Std_Boolean_Array_Type :=
        New_Array_Type (Std_Boolean_Type_Node, Ghdl_Index_Type);
      New_Type_Decl (Create_Identifier ("BOOLEAN_ARRAY"),
                     Std_Boolean_Array_Type);
      Translate_Type_Implicit_Subprograms (Decl, Main);

      --  Second declaration: bit.
      pragma Assert (Decl = Bit_Type_Declaration);
      Chap4.Translate_Bool_Type_Declaration (Bit_Type_Declaration);
      Translate_Type_Implicit_Subprograms (Decl, Main);

      --  Nothing special for other declarations.
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Type_Declaration =>
               Chap4.Translate_Type_Declaration (Decl);
               Translate_Type_Implicit_Subprograms (Decl, Main);
            when Iir_Kind_Anonymous_Type_Declaration =>
               Chap4.Translate_Anonymous_Type_Declaration (Decl);
               Translate_Type_Implicit_Subprograms (Decl, Main);
            when Iir_Kind_Subtype_Declaration =>
               Chap4.Translate_Subtype_Declaration (Decl);
               Decl := Get_Chain (Decl);
            when Iir_Kind_Attribute_Declaration =>
               Decl := Get_Chain (Decl);
            when Iir_Kind_Function_Declaration =>
               case Get_Implicit_Definition (Decl) is
                  when Iir_Predefined_Now_Function =>
                     null;
                  when Iir_Predefined_Enum_To_String
                    | Iir_Predefined_Integer_To_String
                    | Iir_Predefined_Floating_To_String
                    | Iir_Predefined_Real_To_String_Digits
                    | Iir_Predefined_Real_To_String_Format
                    | Iir_Predefined_Physical_To_String
                    | Iir_Predefined_Time_To_String_Unit =>
                     --  These are defined after the types.
                     null;
                  when others =>
                     Error_Kind
                       ("translate_standard ("
                          & Iir_Predefined_Functions'Image
                          (Get_Implicit_Definition (Decl)) & ")",
                        Decl);
               end case;
               Decl := Get_Chain (Decl);
            when others =>
               Error_Kind ("translate_standard", Decl);
         end case;
         --  DECL was updated by Translate_Type_Implicit_Subprograms or
         --  explicitly in other branches.
      end loop;

      --  These types don't appear in std.standard.
      Chap4.Translate_Anonymous_Type_Declaration
        (Convertible_Integer_Type_Declaration);
      Chap4.Translate_Anonymous_Type_Declaration
        (Convertible_Real_Type_Declaration);

      --  Restore time type staticness.

      if Flags.Vhdl_Std > Vhdl_87 then
         Set_Type_Staticness (Delay_Length_Subtype_Definition,
                              Time_Subtype_Staticness);
      end if;
      Set_Type_Staticness (Time_Type_Definition, Time_Type_Staticness);
      Set_Type_Staticness (Time_Subtype_Definition, Time_Subtype_Staticness);

      if Flag_Rti then
         Rtis.Generate_Unit (Standard_Package);
         Std_Standard_Boolean_Rti
           := Get_Info (Boolean_Type_Definition).Type_Rti;
         Std_Standard_Bit_Rti
           := Get_Info (Bit_Type_Definition).Type_Rti;
      end if;

      --  Std_Ulogic indexed array of STD.Boolean.
      --  Used by PSL to convert Std_Ulogic to boolean.
      Std_Ulogic_Boolean_Array_Type := New_Array_Subtype
        (Std_Boolean_Array_Type, Std_Boolean_Type_Node, New_Index_Lit (9));
      New_Type_Decl (Get_Identifier ("__ghdl_std_ulogic_boolean_array_type"),
                     Std_Ulogic_Boolean_Array_Type);
      New_Const_Decl (Ghdl_Std_Ulogic_To_Boolean_Array,
                      Get_Identifier ("__ghdl_std_ulogic_to_boolean_array"),
                      O_Storage_External, Std_Ulogic_Boolean_Array_Type);

      Pop_Identifier_Prefix (Unit_Mark);
      Pop_Identifier_Prefix (Lib_Mark);

      Post_Initialize;
      Current_Filename_Node := O_Dnode_Null;
      --Pop_Global_Factory;
   end Translate_Standard;

   procedure Finalize is
   begin
      Free_Node_Infos;
      Free_Old_Temp;
   end Finalize;

   procedure Elaborate (Config : Iir; Whole : Boolean)
     renames Trans.Chap12.Elaborate;

end Translation;
