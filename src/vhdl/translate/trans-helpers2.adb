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

with Name_Table;
with Trans.Chap3;
with Trans.Chap6;
with Trans_Decls; use Trans_Decls;
with Files_Map;
with Trans.Foreach_Non_Composite;

package body Trans.Helpers2 is
   use Trans.Helpers;

   procedure Copy_Fat_Pointer (D : Mnode; S: Mnode) is
   begin
      New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Base (D)),
                       M2Addr (Chap3.Get_Composite_Base (S)));
      New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (D)),
                       M2Addr (Chap3.Get_Composite_Bounds (S)));
   end Copy_Fat_Pointer;

   --  Convert NAME into a STRING_CST.
   --  Append a NUL terminator (to make interfaces with C easier).
   function Create_String_Type (Str : String) return O_Tnode is
   begin
      return New_Array_Subtype
        (Chararray_Type,
         Char_Type_Node,
         New_Index_Lit (Str'Length + 1));
   end Create_String_Type;

   procedure Create_String_Value
     (Const : in out O_Dnode; Const_Type : O_Tnode; Str : String)
   is
      Res  : O_Cnode;
      List : O_Array_Aggr_List;
   begin
      Start_Init_Value (Const);
      Start_Array_Aggr (List, Const_Type, Str'Length + 1);
      for I in Str'Range loop
         New_Array_Aggr_El
           (List,
            New_Unsigned_Literal (Char_Type_Node, Character'Pos (Str (I))));
      end loop;
      New_Array_Aggr_El (List, New_Unsigned_Literal (Char_Type_Node, 0));
      Finish_Array_Aggr (List, Res);
      Finish_Init_Value (Const, Res);
   end Create_String_Value;

   function Create_String (Str : String; Id : O_Ident) return O_Dnode
   is
      Const : O_Dnode;
      Stype : O_Tnode;
   begin
      Stype := Create_String_Type (Str);
      New_Const_Decl (Const, Id, O_Storage_Private, Stype);
      Create_String_Value (Const, Stype, Str);
      return Const;
   end Create_String;

   function Create_String (Str : String; Id : O_Ident; Storage : O_Storage)
                              return O_Dnode
   is
      Atype : O_Tnode;
      Const : O_Dnode;
   begin
      Atype := Create_String_Type (Str);
      New_Const_Decl (Const, Id, Storage, Atype);
      if Storage /= O_Storage_External then
         Create_String_Value (Const, Atype, Str);
      end if;
      return Const;
   end Create_String;

   function Create_String (Str : Name_Id; Id : O_Ident; Storage : O_Storage)
                          return O_Dnode
   is
      use Name_Table;
   begin
      pragma Assert (not Name_Table.Is_Character (Str));
      return Create_String (Image (Str), Id, Storage);
   end Create_String;

   function Create_String_Len (Str : String; Id : O_Ident) return O_Cnode
   is
      Str_Cst : O_Dnode;
      Str_Len : O_Cnode;
      List    : O_Record_Aggr_List;
      Res     : O_Cnode;
   begin
      Str_Cst := Create_String (Str, Id);
      Str_Len := New_Unsigned_Literal (Ghdl_Index_Type,
                                       Unsigned_64 (Str'Length));
      Start_Record_Aggr (List, Ghdl_Str_Len_Type_Node);
      New_Record_Aggr_El (List, Str_Len);
      New_Record_Aggr_El (List, New_Global_Address (New_Global (Str_Cst),
                                                    Char_Ptr_Type));
      Finish_Record_Aggr (List, Res);
      return Res;
   end Create_String_Len;

   procedure Gen_Memcpy (Dest : O_Enode; Src : O_Enode; Length : O_Enode)
   is
      Constr : O_Assoc_List;
   begin
      Start_Association (Constr, Ghdl_Memcpy);
      New_Association (Constr, New_Convert_Ov (Dest, Ghdl_Ptr_Type));
      New_Association (Constr, New_Convert_Ov (Src, Ghdl_Ptr_Type));
      New_Association (Constr, Length);
      New_Procedure_Call (Constr);
   end Gen_Memcpy;

   function Gen_Alloc
     (Kind : Allocation_Kind; Size : O_Enode; Ptype : O_Tnode) return O_Enode
   is
      Constr : O_Assoc_List;
   begin
      case Kind is
         when Alloc_Heap =>
            Start_Association (Constr, Ghdl_Malloc);
            New_Association (Constr, Size);
            return New_Convert_Ov (New_Function_Call (Constr), Ptype);
         when Alloc_System =>
            Start_Association (Constr, Ghdl_Malloc0);
            New_Association (Constr, Size);
            return New_Convert_Ov (New_Function_Call (Constr), Ptype);
         when Alloc_Stack =>
            return New_Alloca (Ptype, Size);
         when Alloc_Return =>
            Start_Association (Constr, Ghdl_Stack2_Allocate);
            New_Association (Constr, Size);
            return New_Convert_Ov (New_Function_Call (Constr), Ptype);
      end case;
   end Gen_Alloc;

   procedure Register_Non_Composite_Signal (Targ      : Mnode;
                                            Targ_Type : Iir;
                                            Proc      : O_Dnode)
   is
      pragma Unreferenced (Targ_Type);
      Constr : O_Assoc_List;
   begin
      Start_Association (Constr, Proc);
      New_Association
        (Constr, New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      New_Procedure_Call (Constr);
   end Register_Non_Composite_Signal;

   function Register_Update_Data_Array
     (Data : O_Dnode; Targ_Type : Iir; Index : O_Dnode)
         return O_Dnode
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (Index);
   begin
      return Data;
   end Register_Update_Data_Array;

   function Register_Prepare_Data_Composite (Targ      : Mnode;
                                             Targ_Type : Iir;
                                             Data      : O_Dnode)
                                            return O_Dnode
   is
      pragma Unreferenced (Targ);
      pragma Unreferenced (Targ_Type);
   begin
      return Data;
   end Register_Prepare_Data_Composite;

   function Register_Update_Data_Record
     (Data : O_Dnode; Targ_Type : Iir; El : Iir_Element_Declaration)
         return O_Dnode
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (El);
   begin
      return Data;
   end Register_Update_Data_Record;

   procedure Register_Signal_1 is new Foreach_Non_Composite
     (Data_Type => O_Dnode,
      Composite_Data_Type => O_Dnode,
      Do_Non_Composite => Register_Non_Composite_Signal,
      Prepare_Data_Array => Register_Prepare_Data_Composite,
      Update_Data_Array => Register_Update_Data_Array,
      Prepare_Data_Record => Register_Prepare_Data_Composite,
      Update_Data_Record => Register_Update_Data_Record);

   procedure Register_Signal (Targ      : Mnode;
                              Targ_Type : Iir;
                              Proc      : O_Dnode)
                                 renames Register_Signal_1;

   procedure Register_Signal_List (List : Iir_List; Proc : O_Dnode)
   is
      It : List_Iterator;
      El  : Iir;
      Sig : Mnode;
   begin
      It := List_Iterate_Safe (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         Open_Temp;
         Sig := Chap6.Translate_Name (El, Mode_Signal);
         Register_Signal (Sig, Get_Type (El), Proc);
         Close_Temp;
         Next (It);
      end loop;
   end Register_Signal_List;

   function Gen_Oenode_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Val : O_Enode) return Mnode
   is
      pragma Unreferenced (Targ);
      Res       : Mnode;
      Type_Info : Type_Info_Acc;
   begin
      Type_Info := Get_Info (Targ_Type);
      Res := E2M (Val, Type_Info, Mode_Value);
      case Type_Info.Type_Mode is
         when Type_Mode_Arrays =>
            null;
            --  Res := Chap3.Get_Composite_Base (Res);
            -- Res := Chap3.Convert_Array_Base (Res);
         when Type_Mode_Records =>
            Res := Stabilize (Res);
         when others =>
            --  Not a composite type!
            raise Internal_Error;
      end case;
      return Res;
   end Gen_Oenode_Prepare_Data_Composite;

   function Gen_Oenode_Update_Data_Array (Val       : Mnode;
                                          Targ_Type : Iir;
                                          Index     : O_Dnode)
                                         return O_Enode is
   begin
      return M2E (Chap6.Translate_Indexed_Name_By_Offset
                    (Val, Targ_Type, Index));
   end Gen_Oenode_Update_Data_Array;

   function Gen_Oenode_Update_Data_Record
     (Val : Mnode; Targ_Type : Iir; El : Iir_Element_Declaration)
     return O_Enode
   is
      pragma Unreferenced (Targ_Type);
   begin
      return M2E (Chap6.Translate_Selected_Element (Val, El));
   end Gen_Oenode_Update_Data_Record;

   procedure Gen_Oenode_Finish_Data_Composite (Data : in out Mnode)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Gen_Oenode_Finish_Data_Composite;

   function Get_Line_Number (Target: Iir) return Natural
   is
      Line, Col: Natural;
      Name     : Name_Id;
   begin
      Files_Map.Location_To_Position
        (Get_Location (Target), Name, Line, Col);
      return Line;
   end Get_Line_Number;

   procedure Assoc_Filename_Line (Assoc : in out O_Assoc_List;
                                  Line  : Natural) is
   begin
      New_Association
        (Assoc, New_Address (New_Obj (Current_Filename_Node),
                             Char_Ptr_Type));
      New_Association
        (Assoc, New_Lit (New_Signed_Literal (Ghdl_I32_Type,
                                             Integer_64 (Line))));
   end Assoc_Filename_Line;
end Trans.Helpers2;
