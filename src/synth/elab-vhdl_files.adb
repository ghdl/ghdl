--  Create declarations for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Types; use Types;
with Files_Map;
with Name_Table;

with Vhdl.Errors; use Vhdl.Errors;

with Grt.Types; use Grt.Types;
with Grt.Files_Operations; use Grt.Files_Operations;
with Grt.Stdio;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;

package body Elab.Vhdl_Files is

   --  Variables to store the search path.
   Current_Unit : Node := Null_Node;
   Current_Pfx_Len : Integer := -1;
   Current_Pfx_Id : Name_Id := No_Name_Id;

   --  Representation of file name compatible with C (so NUL terminated).
   subtype C_File_Name is String (1 .. 1025);

   procedure File_Error (Loc : Node; Status : Op_Status);
   pragma No_Return (File_Error);

   procedure File_Error (Loc : Node; Status : Op_Status) is
   begin
      pragma Assert (Status /= Op_Ok);
      Error_Msg_Elab (+Loc, "file operation failed");
      raise File_Execution_Error;
   end File_Error;

   --  VAL represents a string, so an array of characters.
   procedure Convert_String (Val : Valtyp; Res : out String)
   is
      Vtyp : constant Type_Acc := Val.Typ;
      Vlen : constant Uns32 := Vtyp.Abounds.D (1).Len;
   begin
      pragma Assert (Vtyp.Kind = Type_Array);
      pragma Assert (Vtyp.Arr_El.Kind = Type_Discrete);
      pragma Assert (Vtyp.Arr_El.W in 7 .. 8); --  Could be 7 in vhdl87
      pragma Assert (Vtyp.Abounds.Ndim = 1);
      pragma Assert (Vtyp.Abounds.D (1).Len = Res'Length);

      for I in 1 .. Vlen loop
         Res (Res'First + Natural (I - 1)) :=
           Character'Val (Read_U8 (Val.Val.Mem + Size_Type (I - 1)));
      end loop;
   end Convert_String;

   --  Convert filename VAL to RES + LEN.
   procedure Convert_File_Name (Val : Valtyp;
                                Res : out C_File_Name;
                                Len : out Natural;
                                Status : out Op_Status)
   is
      Name : constant Valtyp := Strip_Alias_Const (Val);
      pragma Unreferenced (Val);
   begin
      Len := Natural (Name.Typ.Abounds.D (1).Len);

      if Len >= Res'Length - 1 then
         Status := Op_Filename_Error;
         return;
      end if;

      Convert_String (Name, Res (1 .. Len));
      Res (Len + 1) := Grt.Types.NUL;

      Status := Op_Ok;
   end Convert_File_Name;

   procedure Set_Design_Unit (Unit : Node) is
   begin
      Current_Unit := Unit;
      Current_Pfx_Id := No_Name_Id;
   end Set_Design_Unit;

   function Synth_Open (Name : Ghdl_C_String; Mode : Ghdl_C_String)
                       return Grt.Stdio.FILEs
   is
      use Grt.Stdio;
      Res : FILEs;
   begin
      --  Try to open the file using the name given.
      Res := fopen (To_Address (Name), To_Address (Mode));
      if Res /= NULL_Stream then
         --  File found.
         return Res;
      end if;

      --  Return now if the search path is not used:
      --  mode is not read, or
      --  no search path given.
      if Mode (1) /= 'r' then
         return Res;
      end if;
      if Current_Unit = Null_Node then
         return NULL_Stream;
      end if;

      --  The search path is given by the current unit.  Extract it from the
      --  filename (and cache the result).
      if Current_Pfx_Id = No_Name_Id then
         declare
            use Files_Map;
            use Name_Table;

            Loc : Location_Type;
            Sfe : Source_File_Entry;
            Name_Len : Natural;
            Name_Ptr : Thin_String_Ptr;
         begin
            Loc := Get_Location (Current_Unit);
            Sfe := Location_To_File (Loc);
            Current_Pfx_Id := Get_File_Name (Sfe);
            Name_Len := Get_Name_Length (Current_Pfx_Id);
            Name_Ptr := Get_Name_Ptr (Current_Pfx_Id);
            Current_Pfx_Len := 0;
            for I in reverse 1 .. Name_Len loop
               if Name_Ptr (I) = '/' or else Name_Ptr (I) = '\' then
                  Current_Pfx_Len := I;
                  exit;
               end if;
            end loop;
         end;
      end if;

      --  No prefix.
      if Current_Pfx_Len = 0 then
         return NULL_Stream;
      end if;

      --  Try with prefix + name.
      declare
         use Name_Table;
         Name_Len : constant Natural := strlen (Name);
         Pfx : constant Thin_String_Ptr := Get_Name_Ptr (Current_Pfx_Id);
         Name2 : String (1 .. Name_Len + Current_Pfx_Len + 1);
      begin
         Name2 (1 .. Current_Pfx_Len) := Pfx (1 .. Current_Pfx_Len);
         Name2 (Current_Pfx_Len + 1 .. Current_Pfx_Len + Name_Len) :=
           Name (1 .. Name_Len);
         Name2 (Name2'Last) := NUL;
         Res := fopen (Name2'Address, To_Address (Mode));
      end;

      return Res;
   end Synth_Open;

   function Elaborate_File_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return File_Index
   is
      File_Type : constant Node := Get_Type (Decl);
      External_Name : constant Node := Get_File_Logical_Name (Decl);
      Open_Kind : constant Node := Get_File_Open_Kind (Decl);
      File_Name : Valtyp;
      C_Name : C_File_Name;
      C_Name_Len : Natural;
      Mode : Valtyp;
      F : File_Index;
      File_Mode : Ghdl_I32;
      Status : Op_Status;
   begin
      --  Use our own handler to open a file.
      --  We need to do this assignment only once, but it is simpler to do it
      --  here.
      Open_Handler := Synth_Open'Access;

      if Get_Text_File_Flag (File_Type) then
         F := Ghdl_Text_File_Elaborate;
      else
         declare
            File_Typ : Type_Acc;
            Cstr : Ghdl_C_String;
         begin
            File_Typ := Get_Subtype_Object (Syn_Inst, File_Type);
            if File_Typ.File_Signature = null then
               Cstr := null;
            else
               Cstr := To_Ghdl_C_String (File_Typ.File_Signature.all'Address);
            end if;
            F := Ghdl_File_Elaborate (Cstr);
         end;
      end if;

      --  LRM93 4.3.1.4
      --  If file open information is not included in a given file declaration,
      --  then the file declared by the declaration is not opened when the file
      --  declaration is elaborated.
      if External_Name = Null_Node then
         return F;
      end if;

      File_Name := Exec_Expression_With_Basetype (Syn_Inst, External_Name);

      if Open_Kind /= Null_Node then
         Mode := Exec_Expression (Syn_Inst, Open_Kind);
         File_Mode := Ghdl_I32 (Read_Discrete (Mode));
      else
         case Get_Mode (Decl) is
            when Iir_In_Mode =>
               File_Mode := Read_Mode;
            when Iir_Out_Mode =>
               File_Mode := Write_Mode;
            when others =>
               raise Internal_Error;
         end case;
      end if;

      Convert_File_Name (File_Name, C_Name, C_Name_Len, Status);
      if Status = Op_Ok then
         if Get_Text_File_Flag (File_Type) then
            Ghdl_Text_File_Open
              (F, File_Mode, To_Ghdl_C_String (C_Name'Address), Status);
         else
            Ghdl_File_Open
              (F, File_Mode, To_Ghdl_C_String (C_Name'Address), Status);
         end if;
      end if;

      if Status /= Op_Ok then
         if Status = Op_Name_Error then
            Error_Msg_Elab
              (+Decl, "cannot open file: " & C_Name (1 .. C_Name_Len));
            Set_Error (Syn_Inst);
         else
            File_Error (Decl, Status);
         end if;
      end if;

      return F;
   end Elaborate_File_Declaration;

   function Endfile (F : File_Index; Loc : Node) return Boolean
   is
      Status : Op_Status;
   begin
      Ghdl_File_Endfile (F, Status);

      if Status = Op_Ok then
         return False;
      elsif Status = Op_End_Of_File then
         return True;
      else
         File_Error (Loc, Status);
      end if;
   end Endfile;

   --  Declaration
   --  procedure FILE_OPEN (file F : FT;
   --                       External_Name : String;
   --                       Open_Kind : File_Open_Kind);
   procedure Synth_File_Open
     (Syn_Inst : Synth_Instance_Acc; Imp : Node; Loc : Node)
   is
      Inters : constant Node := Get_Interface_Declaration_Chain (Imp);
      F : constant File_Index := Get_Value (Syn_Inst, Inters).Val.File;
      Param2 : constant Node := Get_Chain (Inters);
      File_Name : constant Valtyp := Get_Value (Syn_Inst, Param2);
      Param3 : constant Node := Get_Chain (Param2);
      Open_Kind : constant Valtyp := Get_Value (Syn_Inst, Param3);
      C_Name : C_File_Name;
      C_Name_Len : Natural;
      File_Mode : Ghdl_I32;
      Status : Op_Status;
   begin
      Convert_File_Name (File_Name, C_Name, C_Name_Len, Status);
      if Status = Op_Ok then
         File_Mode := Ghdl_I32 (Read_Discrete (Open_Kind));
         if Get_Text_File_Flag (Get_Type (Inters)) then
            Ghdl_Text_File_Open
              (F, File_Mode, To_Ghdl_C_String (C_Name'Address), Status);
         else
            Ghdl_File_Open
              (F, File_Mode, To_Ghdl_C_String (C_Name'Address), Status);
         end if;
      end if;

      if Status /= Op_Ok then
         if Status = Op_Name_Error then
            Error_Msg_Elab
              (+Loc, "cannot open file: " & C_Name (1 .. C_Name_Len));
            raise File_Execution_Error;
         else
            File_Error (Loc, Status);
         end if;
      end if;
   end Synth_File_Open;

   --  Declaration
   --  procedure FILE_CLOSE (file F : FT);
   procedure Synth_File_Close
     (Syn_Inst : Synth_Instance_Acc; Imp : Node; Loc : Node)
   is
      Inters : constant Node := Get_Interface_Declaration_Chain (Imp);
      F : constant File_Index := Get_Value (Syn_Inst, Inters).Val.File;
      Status : Op_Status;
   begin
      if Get_Text_File_Flag (Get_Type (Inters)) then
         Ghdl_Text_File_Close (F, Status);
      else
         Ghdl_File_Close (F, Status);
      end if;

      if Status /= Op_Ok then
         File_Error (Loc, Status);
      end if;
   end Synth_File_Close;

   --  Declaration:
   --  procedure untruncated_text_read                              --!V87
   --    (file f : text; str : out string; len : out natural);      --!V87
   procedure Synth_Untruncated_Text_Read (Syn_Inst : Synth_Instance_Acc;
                                          Imp : Node;
                                          Loc : Node)
   is
      Inters : constant Node := Get_Interface_Declaration_Chain (Imp);
      File : constant File_Index := Get_Value (Syn_Inst, Inters).Val.File;
      Param2 : constant Node := Get_Chain (Inters);
      Str : constant Valtyp := Get_Value (Syn_Inst, Param2);
      Param3 : constant Node := Get_Chain (Param2);
      Param_Len : constant Valtyp := Get_Value (Syn_Inst, Param3);
      Buf : String (1 .. Natural (Str.Typ.Abounds.D (1).Len));
      Len : Std_Integer;
      Status : Op_Status;
   begin
      Len := Std_Integer (Buf'Last);
      Ghdl_Untruncated_Text_Read
        (File, To_Ghdl_C_String (Buf'Address), Len, Status);
      if Status /= Op_Ok then
         File_Error (Loc, Status);
      end if;

      for I in 1 .. Natural (Len) loop
         Write_U8 (Str.Val.Mem + Size_Type (I - 1), Character'Pos (Buf (I)));
      end loop;

      Write_Discrete (Param_Len, Int64 (Len));
   end Synth_Untruncated_Text_Read;

   procedure File_Read_Value (File : File_Index; Val : Memtyp; Loc : Node)
   is
      Status : Op_Status;
   begin
      case Val.Typ.Kind is
         when Type_Discrete
            | Type_Bit
            | Type_Logic
            | Type_Float =>
            Ghdl_Read_Scalar (File, Ghdl_Ptr (Val.Mem.all'Address),
                              Ghdl_Index_Type (Val.Typ.Sz), Status);
            if Status /= Op_Ok then
               File_Error (Loc, Status);
            end if;
         when Type_Vector
            | Type_Array =>
            declare
               El_Typ : constant Type_Acc := Get_Array_Element (Val.Typ);
               Off    : Size_Type;
            begin
               Off := 0;
               for I in 1 .. Get_Array_Flat_Length (Val.Typ) loop
                  File_Read_Value (File, (El_Typ, Val.Mem + Off), Loc);
                  Off := Off + El_Typ.Sz;
               end loop;
            end;
         when Type_Record =>
            for I in Val.Typ.Rec.E'Range loop
               File_Read_Value
                 (File,
                  (Val.Typ.Rec.E (I).Typ, Val.Mem + Val.Typ.Rec.E (I).Moff),
                  Loc);
            end loop;
         when Type_Unbounded_Record
            | Type_Unbounded_Array
            | Type_Unbounded_Vector
            | Type_Protected
            | Type_Slice
            | Type_File
            | Type_Access =>
            raise Internal_Error;
      end case;
   end File_Read_Value;

   procedure Synth_File_Read
     (Syn_Inst : Synth_Instance_Acc; Imp : Node; Loc : Node)
   is
      Inters : constant Node := Get_Interface_Declaration_Chain (Imp);
      File : constant File_Index := Get_Value (Syn_Inst, Inters).Val.File;
      Param2 : constant Node := Get_Chain (Inters);
      Value : constant Valtyp := Get_Value (Syn_Inst, Param2);
   begin
      File_Read_Value (File, (Value.Typ, Value.Val.Mem), Loc);
   end Synth_File_Read;

end Elab.Vhdl_Files;
