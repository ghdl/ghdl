--  File operations for interpreter
--  Copyright (C) 2014 Tristan Gingold
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
with Vhdl.Annotations; use Vhdl.Annotations;
with Simul.Execution; use Simul.Execution;
with Simul.Debugger; use Simul.Debugger;
with Simul.Grt_Interface; use Simul.Grt_Interface;
with Grt.Lib;

package body Simul.File_Operation is
   --  Open a file.
   --  See LRM93 3.4.1 for definition of arguments.
   --  IS_TEXT is true if the file format is text.
   --  The purpose of the IS_TEXT is to allow a text implementation of file
   --  type TEXT, defined in std.textio.
   procedure File_Open (Status : out Ghdl_I32;
                        File : Iir_Value_Literal_Acc;
                        External_Name : Iir_Value_Literal_Acc;
                        Mode : Ghdl_I32;
                        Is_Text : Boolean;
                        Return_Status : Boolean)
   is
      Name_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (External_Name.Bounds.D (1).Length);
      Name_Str : aliased Std_String_Uncons (1 .. Name_Len);
      Name_Bnd : aliased Std_String_Bound := Build_Bound (External_Name);
      Name : aliased Std_String := (To_Std_String_Basep (Name_Str'Address),
                                    To_Std_String_Boundp (Name_Bnd'Address));
   begin
      -- Convert the string to an Ada string.
      for I in External_Name.Val_Array.V'Range loop
         Name_Str (Name_Str'First + Ghdl_Index_Type (I - 1)) :=
           Character'Val (External_Name.Val_Array.V (I).E8);
      end loop;

      if Is_Text then
         if Return_Status then
            Status := Ghdl_Text_File_Open_Status
              (File.File, Mode, Name'Unrestricted_Access);
         else
            Ghdl_Text_File_Open (File.File, Mode, Name'Unrestricted_Access);
            Status := Open_Ok;
         end if;
      else
         if Return_Status then
            Status := Ghdl_File_Open_Status
              (File.File, Mode, Name'Unrestricted_Access);
         else
            Ghdl_File_Open (File.File, Mode, Name'Unrestricted_Access);
            Status := Open_Ok;
         end if;
      end if;
   end File_Open;

   --  Open a file.
   procedure File_Open (File : Iir_Value_Literal_Acc;
                        Name : Iir_Value_Literal_Acc;
                        Mode : Iir_Value_Literal_Acc;
                        File_Decl : Iir;
                        Stmt : Iir)
   is
      pragma Unreferenced (Stmt);
      Is_Text : constant Boolean := Get_Text_File_Flag (Get_Type (File_Decl));
      File_Mode : constant Ghdl_I32 := Ghdl_I32 (Mode.E8);
      Status : Ghdl_I32;
   begin
      File_Open (Status, File, Name, File_Mode, Is_Text, False);
      if Status /= Open_Ok then
         raise Program_Error;
      end if;
   end File_Open;

   procedure File_Open_Status (Status : Iir_Value_Literal_Acc;
                               File : Iir_Value_Literal_Acc;
                               Name : Iir_Value_Literal_Acc;
                               Mode : Iir_Value_Literal_Acc;
                               File_Decl : Iir;
                               Stmt : Iir)
   is
      pragma Unreferenced (Stmt);
      Is_Text : constant Boolean := Get_Text_File_Flag (Get_Type (File_Decl));
      File_Mode : constant Ghdl_I32 := Ghdl_I32 (Mode.E8);
      R_Status : Ghdl_I32;
   begin
      File_Open (R_Status, File, Name, File_Mode, Is_Text, True);
      Status.E8 := Ghdl_E8 (R_Status);
   end File_Open_Status;

   function Elaborate_File_Declaration
     (Instance: Block_Instance_Acc; Decl: Iir_File_Declaration)
     return Iir_Value_Literal_Acc
   is
      Def : constant Iir := Get_Type (Decl);
      External_Name : Iir;
      File_Name: Iir_Value_Literal_Acc;
      Is_Text : constant Boolean := Get_Text_File_Flag (Def);
      File_Mode : Ghdl_I32;
      Res : Iir_Value_Literal_Acc;
      Status : Ghdl_I32;
      Mode : Iir_Value_Literal_Acc;
   begin
      if Is_Text then
         Res := Create_File_Value (Ghdl_Text_File_Elaborate);
      else
         declare
            Sig : constant String_Acc := Get_Info (Def).File_Signature;
            Cstr : Ghdl_C_String;
         begin
            if Sig = null then
               Cstr := null;
            else
               Cstr := To_Ghdl_C_String (Sig.all'Address);
            end if;
            Res := Create_File_Value (Ghdl_File_Elaborate (Cstr));
         end;
      end if;

      External_Name := Get_File_Logical_Name (Decl);

      --  LRM93 4.3.1.4
      --  If file open information is not included in a given file declaration,
      --  then the file declared by the declaration is not opened when the file
      --  declaration is elaborated.
      if External_Name = Null_Iir then
         return Res;
      end if;

      File_Name := Execute_Expression (Instance, External_Name);
      if Get_File_Open_Kind (Decl) /= Null_Iir then
         Mode := Execute_Expression (Instance, Get_File_Open_Kind (Decl));
         File_Mode := Ghdl_I32 (Mode.E8);
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
      File_Open (Status, Res, File_Name, File_Mode, Is_Text, False);
      return Res;
   end Elaborate_File_Declaration;

   procedure File_Close_Text (File : Iir_Value_Literal_Acc; Stmt : Iir) is
      pragma Unreferenced (Stmt);
   begin
      Ghdl_Text_File_Close (File.File);
   end File_Close_Text;

   procedure File_Close_Binary (File : Iir_Value_Literal_Acc; Stmt : Iir) is
      pragma Unreferenced (Stmt);
   begin
      Ghdl_File_Close (File.File);
   end File_Close_Binary;

   procedure File_Destroy_Text (File : Iir_Value_Literal_Acc) is
   begin
      Ghdl_Text_File_Finalize (File.File);
   end File_Destroy_Text;

   procedure File_Destroy_Binary (File : Iir_Value_Literal_Acc) is
   begin
      Ghdl_File_Finalize (File.File);
   end File_Destroy_Binary;


   procedure Write_Binary (File: Iir_Value_Literal_Acc;
                           Value: Iir_Value_Literal_Acc) is
   begin
      case Value.Kind is
         when Iir_Value_B1 =>
            Ghdl_Write_Scalar (File.File, Ghdl_Ptr (Value.B1'Address), 1);
         when Iir_Value_I64 =>
            Ghdl_Write_Scalar (File.File, Ghdl_Ptr (Value.I64'Address), 8);
         when Iir_Value_E8 =>
            Ghdl_Write_Scalar (File.File, Ghdl_Ptr (Value.E8'Address), 1);
         when Iir_Value_E32 =>
            Ghdl_Write_Scalar (File.File, Ghdl_Ptr (Value.E32'Address), 4);
         when Iir_Value_F64 =>
            Ghdl_Write_Scalar (File.File, Ghdl_Ptr (Value.F64'Address), 8);
         when Iir_Value_Array =>
            for I in Value.Bounds.D'Range loop
               Ghdl_Write_Scalar
                 (File.File, Ghdl_Ptr (Value.Bounds.D (I).Length'Address), 4);
            end loop;
            for I in Value.Val_Array.V'Range loop
               Write_Binary (File, Value.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Value.Val_Record.V'Range loop
               Write_Binary (File, Value.Val_Record.V (I));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Write_Binary;

   procedure Write_Text (File: Iir_Value_Literal_Acc;
                         Value: Iir_Value_Literal_Acc)
   is
      Val_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (Value.Bounds.D (1).Length);
      Val_Str : aliased Std_String_Uncons (1 .. Val_Len);
      Val_Bnd : aliased Std_String_Bound := Build_Bound (Value);
      Val : aliased Std_String := (To_Std_String_Basep (Val_Str'Address),
                                    To_Std_String_Boundp (Val_Bnd'Address));
   begin
      -- Convert the string to an Ada string.
      for I in Value.Val_Array.V'Range loop
         Val_Str (Val_Str'First + Ghdl_Index_Type (I - 1)) :=
           Character'Val (Value.Val_Array.V (I).E8);
      end loop;

      Ghdl_Text_Write (File.File, Val'Unrestricted_Access);
   end Write_Text;

   function Endfile (File : Iir_Value_Literal_Acc; Stmt : Iir)
                    return Boolean
   is
      pragma Unreferenced (Stmt);
   begin
      return Grt.Files.Ghdl_File_Endfile (File.File);
   end Endfile;

   procedure Read_Length_Text (File : Iir_Value_Literal_Acc;
                               Value : Iir_Value_Literal_Acc;
                               Length : Iir_Value_Literal_Acc)
   is
      Val_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (Value.Bounds.D (1).Length);
      Val_Str : aliased Std_String_Uncons (1 .. Val_Len);
      Val_Bnd : aliased Std_String_Bound := Build_Bound (Value);
      Val : aliased Std_String := (To_Std_String_Basep (Val_Str'Address),
                                   To_Std_String_Boundp (Val_Bnd'Address));
      Len : Std_Integer;
   begin
      Len := Ghdl_Text_Read_Length (File.File, Val'Unrestricted_Access);
      for I in 1 .. Len loop
         Value.Val_Array.V (Iir_Index32 (I)).E8 :=
           Character'Pos (Val_Str (Ghdl_Index_Type (I)));
      end loop;
      Length.I64 := Ghdl_I64 (Len);
   end Read_Length_Text;

   procedure Untruncated_Text_Read (File : Iir_Value_Literal_Acc;
                                    Str : Iir_Value_Literal_Acc;
                                    Length : Iir_Value_Literal_Acc)
   is
      Len : Std_Integer;
      Val_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (Str.Bounds.D (1).Length);
      Val_Str : aliased Std_String_Uncons (1 .. Val_Len);
      Val_Bnd : aliased Std_String_Bound := Build_Bound (Str);
      Val : aliased Std_String := (To_Std_String_Basep (Val_Str'Address),
                                   To_Std_String_Boundp (Val_Bnd'Address));
   begin
      Ghdl_Untruncated_Text_Read
        (File.File, Val'Unrestricted_Access, Len'Unrestricted_Access);
      for I in 1 .. Len loop
         Str.Val_Array.V (Iir_Index32 (I)).E8 :=
           Character'Pos (Val_Str (Ghdl_Index_Type (I)));
      end loop;
      Length.I64 := Ghdl_I64 (Len);
   end Untruncated_Text_Read;

   procedure Read_Binary (File: Iir_Value_Literal_Acc;
                          Value: Iir_Value_Literal_Acc)
   is
   begin
      case Value.Kind is
         when Iir_Value_B1 =>
            Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Value.B1'Address), 1);
         when Iir_Value_I64 =>
            Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Value.I64'Address), 8);
         when Iir_Value_E8 =>
            Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Value.E8'Address), 1);
         when Iir_Value_E32 =>
            Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Value.E32'Address), 4);
         when Iir_Value_F64 =>
            Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Value.F64'Address), 8);
         when Iir_Value_Array =>
            for I in Value.Bounds.D'Range loop
               declare
                  Len : Iir_Index32;
               begin
                  Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Len'Address), 4);
                  if Len /= Value.Bounds.D (I).Length then
                     Error_Msg_Constraint (Null_Iir); --  FIXME: loc
                  end if;
               end;
            end loop;
            for I in Value.Val_Array.V'Range loop
               Read_Binary (File, Value.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Value.Val_Record.V'Range loop
               Read_Binary (File, Value.Val_Record.V (I));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Read_Binary;

   procedure Read_Length_Binary (File : Iir_Value_Literal_Acc;
                                 Value : Iir_Value_Literal_Acc;
                                 Length : Iir_Value_Literal_Acc)
   is
      Len : Iir_Index32;
   begin
      Ghdl_Read_Scalar (File.File, Ghdl_Ptr (Len'Address), 4);
      for I in 1 .. Len loop
         if I <= Value.Bounds.D (1).Length then
            Read_Binary (File, Value.Val_Array.V (I));
         else
            --  FIXME: for empty arrays ??
            --  Lose_Binary (File, Value.Val_Array (0));
            raise Internal_Error;
         end if;
      end loop;
      Length.I64 := Ghdl_I64 (Len);
   end Read_Length_Binary;

   procedure Flush (File : Iir_Value_Literal_Acc) is
   begin
      Ghdl_File_Flush (File.File);
   end Flush;

   procedure Textio_Write_Real (Str : Iir_Value_Literal_Acc;
                                Len : Iir_Value_Literal_Acc;
                                Val : Ghdl_F64;
                                Ndigits : Std_Integer)
   is
      Len_Arg : aliased Std_Integer;
      Str_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (Str.Bounds.D (1).Length);
      Str_Str : aliased Std_String_Uncons (1 .. Str_Len);
      Str_Bnd : aliased Std_String_Bound := Build_Bound (Str);
      Str_Arg : aliased Std_String := (To_Std_String_Basep (Str_Str'Address),
                                       To_Std_String_Boundp (Str_Bnd'Address));
   begin
      Grt.Lib.Textio_Write_Real
        (Str_Arg'Unrestricted_Access, Len_Arg'Unrestricted_Access,
         Val, Ndigits);
      for I in 1 .. Len_Arg loop
         Str.Val_Array.V (Iir_Index32 (I)).E8 :=
           Character'Pos (Str_Str (Ghdl_Index_Type (I)));
      end loop;
      Len.I64 := Ghdl_I64 (Len_Arg);
   end Textio_Write_Real;

   function Textio_Read_Real (Str : Iir_Value_Literal_Acc) return Ghdl_F64
   is
      Str_Len : constant Ghdl_Index_Type :=
        Ghdl_Index_Type (Str.Bounds.D (1).Length);
      Str_Str : aliased Std_String_Uncons (1 .. Str_Len);
      Str_Bnd : aliased Std_String_Bound := Build_Bound (Str);
      Str_Arg : aliased Std_String := (To_Std_String_Basep (Str_Str'Address),
                                       To_Std_String_Boundp (Str_Bnd'Address));
   begin
      for I in Str.Val_Array.V'Range loop
         Str_Str (Ghdl_Index_Type (I)) :=
           Character'Val (Str.Val_Array.V (I).E8);
      end loop;
      return Grt.Lib.Textio_Read_Real (Str_Arg'Unrestricted_Access);
   end Textio_Read_Real;
end Simul.File_Operation;
