--  Debugging during synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with System;

with Types; use Types;
with Files_Map;
with Tables;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Name_Table;
with Str_Table;
with Libraries;

with Grt.Readline;

with Vhdl.Errors;
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;
with Vhdl.Parse;

with Synth.Values; use Synth.Values;
-- with Synth.Environment; use Synth.Environment;
with Synth.Flags;

package body Synth.Debugger is
   Current_Instance : Synth_Instance_Acc;
   Current_Loc : Node;

   type Debug_Reason is
     (
      Reason_Init,
      Reason_Break,
      Reason_Error
     );

   package Breakpoints is new Tables
     (Table_Index_Type => Natural,
      Table_Component_Type => Node,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   function Is_Breakpoint_Hit return Boolean is
   begin
      for I in Breakpoints.First .. Breakpoints.Last loop
         if Breakpoints.Table (I) = Current_Loc then
            return True;
         end if;
      end loop;
      return False;
   end Is_Breakpoint_Hit;

   --  Current execution state, or reason to stop execution (set by the
   --  last debugger command).
   type Exec_State_Type is
     (--  Execution should continue until a breakpoint is reached or assertion
      --  failure.
      Exec_Run,

      --  Execution will stop at the next statement.
      Exec_Single_Step,

      --  Execution will stop at the next simple statement in the same frame.
      Exec_Next,

      --  Execution will stop at the next statement in the same frame.  In
      --  case of compound statement, stop after the compound statement.
      Exec_Next_Stmt);

   Exec_State : Exec_State_Type := Exec_Run;

   --  Current frame for next.
   Exec_Instance : Synth_Instance_Acc;

   --  Current statement for next_stmt.
   Exec_Statement : Node;

   function Is_Within_Statement (Stmt : Node; Cur : Node) return Boolean
   is
      Parent : Node;
   begin
      Parent := Cur;
      loop
         if Parent = Stmt then
            return True;
         end if;
         case Get_Kind (Parent) is
            when Iir_Kinds_Sequential_Statement =>
               Parent := Get_Parent (Parent);
            when others =>
               return False;
         end case;
      end loop;
   end Is_Within_Statement;

   Prompt_Debug : constant String := "debug> " & ASCII.NUL;
   Prompt_Error : constant String := "error> " & ASCII.NUL;
   Prompt_Init  : constant String := "init> " & ASCII.NUL;
   --  Prompt_Elab  : constant String := "elab> " & ASCII.NUL;

   procedure Disp_Iir_Location (N : Node) is
   begin
      if N = Null_Iir then
         Put_Err ("??:??:??");
      else
         Put_Err (Vhdl.Errors.Disp_Location (N));
      end if;
      Put_Err (": ");
   end Disp_Iir_Location;

   --  For the list command: current file and current line.
   List_Current_File : Source_File_Entry := No_Source_File_Entry;
   List_Current_Line : Natural := 0;
   List_Current_Line_Pos : Source_Ptr := 0;

   --  Set List_Current_* from a location.  To be called after program break
   --  to indicate current location.
   procedure Set_List_Current (Loc : Location_Type)
   is
      Offset : Natural;
   begin
      Files_Map.Location_To_Coord
        (Loc, List_Current_File, List_Current_Line_Pos,
         List_Current_Line, Offset);
   end Set_List_Current;

   procedure Disp_Current_Lines
   is
      use Files_Map;
      --  Number of lines to display before and after the current line.
      Radius : constant := 5;

      Buf : File_Buffer_Acc;

      Pos : Source_Ptr;
      Line : Natural;
      Len : Source_Ptr;
      C : Character;
   begin
      if List_Current_Line > Radius then
         Line := List_Current_Line - Radius;
      else
         Line := 1;
      end if;

      Pos := File_Line_To_Position (List_Current_File, Line);
      Buf := Get_File_Source (List_Current_File);

      while Line < List_Current_Line + Radius loop
         --  Compute line length.
         Len := 0;
         loop
            C := Buf (Pos + Len);
            exit when C = ASCII.CR or C = ASCII.LF or C = ASCII.EOT;
            Len := Len + 1;
         end loop;

         --  Disp line number.
         declare
            Str : constant String := Natural'Image (Line);
         begin
            if Line = List_Current_Line then
               Put ('*');
            else
               Put (' ');
            end if;
            Put ((Str'Length .. 5 => ' '));
            Put (Str (Str'First + 1 .. Str'Last));
            Put (' ');
         end;

         --  Disp line.
         Put_Line (String (Buf (Pos .. Pos + Len - 1)));

         --  Skip EOL.
         exit when C = ASCII.EOT;
         Pos := Pos + Len + 1;
         if C = ASCII.CR then
            if Buf (Pos) = ASCII.LF then
               Pos := Pos + 1;
            end if;
         else
            pragma Assert (C = ASCII.LF);
            if Buf (Pos) = ASCII.CR then
               Pos := Pos + 1;
            end if;
         end if;

         Line := Line + 1;
      end loop;
   end Disp_Current_Lines;

   procedure Disp_Source_Line (Loc : Location_Type)
   is
      use Files_Map;

      File : Source_File_Entry;
      Line_Pos : Source_Ptr;
      Line : Natural;
      Offset : Natural;
      Buf : File_Buffer_Acc;
      Next_Line_Pos : Source_Ptr;
   begin
      Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
      Buf := Get_File_Source (File);
      Next_Line_Pos := File_Line_To_Position (File, Line + 1);
      Put (String (Buf (Line_Pos .. Next_Line_Pos - 1)));
   end Disp_Source_Line;

   --  The status of the debugger.  This status can be modified by a command
   --  as a side effect to resume or quit the debugger.
   type Command_Status_Type is (Status_Default, Status_Quit);
   Command_Status : Command_Status_Type;

   --  This exception can be raised by a debugger command to directly return
   --  to the prompt.
   Command_Error : exception;

   type Menu_Procedure is access procedure (Line : String);

   --  If set (by commands), call this procedure on empty line to repeat
   --  last command.
   Cmd_Repeat : Menu_Procedure;

   type Menu_Kind is (Menu_Command, Menu_Submenu);
   type Menu_Entry (Kind : Menu_Kind);
   type Menu_Entry_Acc is access all Menu_Entry;

   type Cst_String_Acc is access constant String;

   type Menu_Entry (Kind : Menu_Kind) is record
      Name : Cst_String_Acc;
      Next : Menu_Entry_Acc;

      case Kind is
         when Menu_Command =>
            Proc : Menu_Procedure;
         when Menu_Submenu =>
            First, Last : Menu_Entry_Acc := null;
      end case;
   end record;

   function Is_Blank (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_Blank;

   function Skip_Blanks (S : String) return Positive
   is
      P : Positive := S'First;
   begin
      while P <= S'Last and then Is_Blank (S (P)) loop
         P := P + 1;
      end loop;
      return P;
   end Skip_Blanks;

   --  Return the position of the last character of the word (the last
   --  non-blank character).
   function Get_Word (S : String) return Positive
   is
      P : Positive := S'First;
   begin
      while P <= S'Last and then not Is_Blank (S (P)) loop
         P := P + 1;
      end loop;
      return P - 1;
   end Get_Word;

   procedure Disp_Memtyp (M : Memtyp; Vtype : Node);

   procedure Disp_Discrete_Value (Val : Int64; Btype : Node) is
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Integer_Type_Definition =>
            Put_Int64 (Val);
         when Iir_Kind_Enumeration_Type_Definition =>
            declare
               Pos : constant Natural := Natural (Val);
               Enums : constant Node_Flist :=
                 Get_Enumeration_Literal_List (Btype);
               Id : constant Name_Id :=
                 Get_Identifier (Get_Nth_Element (Enums, Pos));
            begin
               Put (Name_Table.Image (Id));
            end;
         when others =>
            Vhdl.Errors.Error_Kind ("disp_discrete_value", Btype);
      end case;
   end Disp_Discrete_Value;

   procedure Disp_Value_Vector (Mem : Memtyp; A_Type: Node; Bound : Bound_Type)
   is
      El_Type : constant Node := Get_Base_Type (Get_Element_Subtype (A_Type));
      El_Typ : constant Type_Acc := Get_Array_Element (Mem.Typ);
      type Last_Enum_Type is (None, Char, Identifier);
      Last_Enum : Last_Enum_Type;
      Enum_List : Node_Flist;
      El_Id : Name_Id;
      El_Pos : Natural;
   begin
      --  Pretty print vectors of enumerated types
      if Get_Kind (El_Type) = Iir_Kind_Enumeration_Type_Definition then
         Last_Enum := None;
         Enum_List := Get_Enumeration_Literal_List (El_Type);
         for I in 1 .. Bound.Len loop
            El_Pos := Natural
              (Read_Discrete (Mem.Mem + Size_Type (I - 1) * El_Typ.Sz,
                              El_Typ));
            El_Id := Get_Identifier (Get_Nth_Element (Enum_List, El_Pos));
            if Name_Table.Is_Character (El_Id) then
               case Last_Enum is
                  when None =>
                     Put ("""");
                  when Identifier =>
                     Put (" & """);
                  when Char =>
                     null;
               end case;
               Put (Name_Table.Get_Character (El_Id));
               Last_Enum := Char;
            else
               case Last_Enum is
                  when None =>
                     null;
                  when Identifier =>
                     Put (" & ");
                  when Char =>
                     Put (""" & ");
               end case;
               Put (Name_Table.Image (El_Id));
               Last_Enum := Identifier;
            end if;
         end loop;
         case Last_Enum is
            when None =>
               Put ("""""");  --  Simply ""
            when Identifier =>
               null;
            when Char =>
               Put ("""");
         end case;
      else
         Put ("(");
         for I in 1 .. Bound.Len loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Memtyp ((El_Typ, Mem.Mem + Size_Type (I - 1) * Mem.Typ.Sz),
                         El_Type);
         end loop;
         Put (")");
      end if;
   end Disp_Value_Vector;

   procedure Disp_Value_Array (Mem : Memtyp; A_Type: Node; Dim: Dim_Type)
   is
      Stride : Size_Type;
   begin
      if Dim = Mem.Typ.Abounds.Ndim then
         --  Last dimension
         Disp_Value_Vector (Mem, A_Type, Mem.Typ.Abounds.D (Dim));
      else
         Stride := Mem.Typ.Arr_El.Sz;
         for I in Dim + 1 .. Mem.Typ.Abounds.Ndim loop
            Stride := Stride * Size_Type (Mem.Typ.Abounds.D (I).Len);
         end loop;

         Put ("(");
         for I in 1 .. Mem.Typ.Abounds.D (Dim).Len loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Value_Array ((Mem.Typ, Mem.Mem + Stride), A_Type, Dim + 1);
         end loop;
         Put (")");
      end if;
   end Disp_Value_Array;

   procedure Disp_Memtyp (M : Memtyp; Vtype : Node) is
   begin
      if M.Mem = null then
         Put ("*NULL*");
         return;
      end if;

      case M.Typ.Kind is
         when Type_Discrete
           | Type_Bit
           | Type_Logic =>
            Disp_Discrete_Value (Read_Discrete (M.Mem, M.Typ),
                                 Get_Base_Type (Vtype));
         when Type_Vector =>
            Disp_Value_Vector (M, Vtype, M.Typ.Vbound);
         when Type_Array =>
            Disp_Value_Array (M, Vtype, 1);
         when Type_Float =>
            Put ("*float*");
         when Type_Slice =>
            Put ("*slice*");
         when Type_File =>
            Put ("*file*");
         when Type_Record =>
            Put ("*record*");
         when Type_Access =>
            Put ("*access*");
         when Type_Unbounded_Array
           | Type_Unbounded_Vector =>
            Put ("*unbounded*");
      end case;
   end Disp_Memtyp;

   procedure Disp_Value (Vt : Valtyp; Vtype : Node) is
   begin
      if Vt.Val = null then
         Put ("*NULL*");
         return;
      end if;

      case Vt.Val.Kind is
         when Value_Net =>
            Put ("net");
         when Value_Wire =>
            Put ("wire");
         when Value_Array =>
            Put ("array");
         when Value_Const_Array =>
            Put ("const_array");
         when Value_Record =>
            Put ("record");
         when Value_Const_Record =>
            Put ("const_record");
         when Value_Access =>
            Put ("access");
         when Value_File =>
            Put ("file");
         when Value_Const =>
            Put ("const: ");
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
         when Value_Alias =>
            Put ("alias");
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
         when Value_Memory =>
            Disp_Memtyp (Get_Memtyp (Vt), Vtype);
      end case;
   end Disp_Value;

   procedure Disp_Bound_Type (Bound : Bound_Type) is
   begin
      Put_Int32 (Bound.Left);
      Put (' ');
      case Bound.Dir is
         when Iir_To =>
            Put ("to");
         when Iir_Downto =>
            Put ("downto");
      end case;
      Put (' ');
      Put_Int32 (Bound.Right);
   end Disp_Bound_Type;

   procedure Disp_Type (Typ : Type_Acc; Vtype : Node)
   is
      pragma Unreferenced (Vtype);
   begin
      case Typ.Kind is
         when Type_Bit =>
            Put ("bit");
         when Type_Logic =>
            Put ("logic");
         when Type_Discrete =>
            Put ("discrete");
         when Type_Float =>
            Put ("float");
         when Type_Vector =>
            Put ("vector (");
            Disp_Bound_Type (Typ.Vbound);
            Put (')');
         when Type_Unbounded_Vector =>
            Put ("unbounded_vector");
         when Type_Array =>
            Put ("array");
         when Type_Unbounded_Array =>
            Put ("unbounded_array");
         when Type_Record =>
            Put ("record");
         when Type_Slice =>
            Put ("slice");
         when Type_Access =>
            Put ("access");
         when Type_File =>
            Put ("file");
      end case;
   end Disp_Type;

   procedure Disp_Declaration_Object
     (Instance : Synth_Instance_Acc; Decl : Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_File_Declaration =>
            declare
               Val : constant Valtyp := Get_Value (Instance, Decl);
               Dtype : constant Node := Get_Type (Decl);
            begin
               Put (Vhdl.Errors.Disp_Node (Decl));
               Put (": ");
               Disp_Type (Val.Typ, Dtype);
               Put (" = ");
               Disp_Value (Val, Dtype);
               New_Line;
            end;
         when Iir_Kinds_Signal_Attribute =>
            --  FIXME: todo ?
            null;
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration =>
            --  FIXME: disp ranges
            null;
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("disp_declaration_object", Decl);
      end case;
   end Disp_Declaration_Object;

   procedure Disp_Declaration_Objects
     (Instance : Synth_Instance_Acc; Decl_Chain : Iir)
   is
      El : Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         Disp_Declaration_Object (Instance, El);
         El := Get_Chain (El);
      end loop;
   end Disp_Declaration_Objects;

   procedure Info_Params_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Decl : Iir;
      Params : Iir;
   begin
      Decl := Get_Source_Scope (Current_Instance);
      loop
         case Get_Kind (Decl) is
            when Iir_Kind_Procedure_Body
              | Iir_Kind_Function_Body =>
               Decl := Get_Subprogram_Specification (Decl);
               exit;
            when Iir_Kind_Process_Statement
              | Iir_Kind_Sensitized_Process_Statement =>
               Put_Line ("processes have no parameters");
               return;
            when Iir_Kind_While_Loop_Statement
              | Iir_Kind_If_Statement
              | Iir_Kind_For_Loop_Statement
              | Iir_Kind_Case_Statement =>
               Decl := Get_Parent (Decl);
            when others =>
               Vhdl.Errors.Error_Kind ("info_params_proc", Decl);
         end case;
      end loop;
      Params := Get_Interface_Declaration_Chain (Decl);
      Disp_Declaration_Objects (Current_Instance, Params);
   end Info_Params_Proc;

   procedure Info_Locals_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Decl : Iir;
      Decls : Iir;
   begin
      --  From statement to declaration.
      Decl := Get_Source_Scope (Current_Instance);
      loop
         case Get_Kind (Decl) is
            when Iir_Kind_Procedure_Body
              | Iir_Kind_Function_Body =>
               Decls := Get_Declaration_Chain (Decl);
               exit;
            when Iir_Kind_Process_Statement
              | Iir_Kind_Sensitized_Process_Statement =>
               Put_Line ("processes have no parameters");
               return;
            when Iir_Kind_While_Loop_Statement
              | Iir_Kind_If_Statement
              | Iir_Kind_For_Loop_Statement
              | Iir_Kind_Case_Statement =>
               Decl := Get_Parent (Decl);
            when others =>
               Vhdl.Errors.Error_Kind ("info_params_proc", Decl);
         end case;
      end loop;
      Disp_Declaration_Objects (Current_Instance, Decls);
   end Info_Locals_Proc;

   function Walk_Files (Cb : Walk_Cb) return Walk_Status
   is
      Lib : Iir_Library_Declaration := Libraries.Get_Libraries_Chain;
      File : Iir_Design_File;
   begin
      while Lib /= Null_Iir loop
         File := Get_Design_File_Chain (Lib);
         while File /= Null_Iir loop
            case Cb.all (File) is
               when Walk_Continue =>
                  null;
               when Walk_Up =>
                  exit;
               when Walk_Abort =>
                  return Walk_Abort;
            end case;
            File := Get_Chain (File);
         end loop;
         Lib := Get_Chain (Lib);
      end loop;
      return Walk_Continue;
   end Walk_Files;

   Walk_Units_Cb : Walk_Cb;

   function Cb_Walk_Units (Design_File : Iir) return Walk_Status
   is
      Unit : Iir_Design_Unit;
   begin
      Unit := Get_First_Design_Unit (Design_File);
      while Unit /= Null_Iir loop
         case Walk_Units_Cb.all (Get_Library_Unit (Unit)) is
            when Walk_Continue =>
               null;
            when Walk_Abort =>
               return Walk_Abort;
            when Walk_Up =>
               exit;
         end case;
         Unit := Get_Chain (Unit);
      end loop;
      return Walk_Continue;
   end Cb_Walk_Units;

   function Walk_Units (Cb : Walk_Cb) return Walk_Status is
   begin
      Walk_Units_Cb := Cb;
      return Walk_Files (Cb_Walk_Units'Access);
   end Walk_Units;

   Walk_Declarations_Cb : Walk_Cb;

   function Cb_Walk_Declarations (Unit : Iir) return Walk_Status
   is
      function Walk_Decl_Chain (Chain : Iir) return Walk_Status
      is
         Decl : Iir;
      begin
         Decl := Chain;
         while Decl /= Null_Iir loop
            case Walk_Declarations_Cb.all (Decl) is
               when Walk_Abort =>
                  return Walk_Abort;
               when Walk_Up =>
                  return Walk_Continue;
               when Walk_Continue =>
                  null;
            end case;
            Decl := Get_Chain (Decl);
         end loop;
         return Walk_Continue;
      end Walk_Decl_Chain;

      function Walk_Conc_Chain (Chain : Iir) return Walk_Status;

      function Walk_Generate_Statement_Body (Bod : Iir) return Walk_Status is
      begin
         if Walk_Decl_Chain (Get_Declaration_Chain (Bod)) = Walk_Abort then
            return Walk_Abort;
         end if;
         if Walk_Conc_Chain (Get_Concurrent_Statement_Chain (Bod)) = Walk_Abort
         then
            return Walk_Abort;
         end if;
         return Walk_Continue;
      end Walk_Generate_Statement_Body;

      function Walk_Conc_Chain (Chain : Iir) return Walk_Status
      is
         Stmt : Iir := Chain;
      begin
         while Stmt /= Null_Iir loop
            case Get_Kind (Stmt) is
               when Iir_Kinds_Process_Statement =>
                  if Walk_Decl_Chain (Get_Declaration_Chain (Stmt))
                    = Walk_Abort
                  then
                     return Walk_Abort;
                  end if;
               when Iir_Kind_For_Generate_Statement =>
                  if Walk_Declarations_Cb.all
                    (Get_Parameter_Specification (Stmt)) = Walk_Abort
                    or else Walk_Generate_Statement_Body
                    (Get_Generate_Statement_Body (Stmt)) = Walk_Abort
                  then
                     return Walk_Abort;
                  end if;
               when Iir_Kind_If_Generate_Statement =>
                  declare
                     Stmt1 : Iir;
                  begin
                     Stmt1 := Stmt;
                     while Stmt1 /= Null_Iir loop
                        if Walk_Generate_Statement_Body
                          (Get_Generate_Statement_Body (Stmt)) = Walk_Abort
                        then
                           return Walk_Abort;
                        end if;
                        Stmt1 := Get_Generate_Else_Clause (Stmt1);
                     end loop;
                  end;
               when Iir_Kind_Component_Instantiation_Statement
                 | Iir_Kind_Concurrent_Simple_Signal_Assignment =>
                  null;
               when Iir_Kind_Block_Statement =>
                  --  FIXME: header
                  if (Walk_Decl_Chain
                        (Get_Declaration_Chain (Stmt)) = Walk_Abort)
                    or else
                    (Walk_Conc_Chain
                       (Get_Concurrent_Statement_Chain (Stmt)) = Walk_Abort)
                  then
                     return Walk_Abort;
                  end if;
               when others =>
                  Vhdl.Errors.Error_Kind ("walk_conc_chain", Stmt);
            end case;
            Stmt := Get_Chain (Stmt);
         end loop;
         return Walk_Continue;
      end Walk_Conc_Chain;
   begin
      case Get_Kind (Unit) is
         when Iir_Kind_Entity_Declaration =>
            if Walk_Decl_Chain (Get_Generic_Chain (Unit)) = Walk_Abort
              or else Walk_Decl_Chain (Get_Port_Chain (Unit)) = Walk_Abort
              or else (Walk_Decl_Chain
                         (Get_Declaration_Chain (Unit)) = Walk_Abort)
              or else (Walk_Conc_Chain
                         (Get_Concurrent_Statement_Chain (Unit)) = Walk_Abort)
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Architecture_Body =>
            if (Walk_Decl_Chain
                  (Get_Declaration_Chain (Unit)) = Walk_Abort)
              or else (Walk_Conc_Chain
                         (Get_Concurrent_Statement_Chain (Unit)) = Walk_Abort)
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body =>
            if Walk_Decl_Chain (Get_Declaration_Chain (Unit)) = Walk_Abort
            then
               return Walk_Abort;
            end if;
         when Iir_Kind_Configuration_Declaration =>
            if Walk_Decl_Chain (Get_Declaration_Chain (Unit)) = Walk_Abort
            then
               return Walk_Abort;
            end if;
            --  FIXME: block configuration ?
         when Iir_Kind_Context_Declaration =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("Cb_Walk_Declarations", Unit);
      end case;
      return Walk_Continue;
   end Cb_Walk_Declarations;

   function Walk_Declarations (Cb : Walk_Cb) return Walk_Status is
   begin
      Walk_Declarations_Cb := Cb;
      return Walk_Units (Cb_Walk_Declarations'Access);
   end Walk_Declarations;

   --  Next statement in the same frame, but handle compound statements as
   --  one statement.
   procedure Next_Stmt_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next_Stmt;
      Exec_Instance := Current_Instance;
      Exec_Statement := Current_Loc;
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
   end Next_Stmt_Proc;

   --  Finish parent statement.
   procedure Finish_Stmt_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next_Stmt;
      Exec_Instance := Current_Instance;
      Exec_Statement := Get_Parent (Current_Loc);
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
   end Finish_Stmt_Proc;

   procedure Next_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next;
      Exec_Instance := Current_Instance;
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
      Cmd_Repeat := Next_Proc'Access;
   end Next_Proc;

   procedure Step_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Single_Step;
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
      Cmd_Repeat := Step_Proc'Access;
   end Step_Proc;

   Break_Id : Name_Id;

   procedure Set_Breakpoint (Stmt : Iir) is
   begin
      Put_Line ("set breakpoint at: " & Files_Map.Image (Get_Location (Stmt)));
      Breakpoints.Append (Stmt);
      Flag_Need_Debug := True;
   end Set_Breakpoint;

   function Cb_Set_Break (El : Iir) return Walk_Status is
   begin
      case Get_Kind (El) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            if Get_Identifier (El) = Break_Id
              and then
              Get_Implicit_Definition (El) not in Iir_Predefined_Implicit
            then
               Set_Breakpoint
                 (Get_Sequential_Statement_Chain (Get_Subprogram_Body (El)));
            end if;
         when others =>
            null;
      end case;
      return Walk_Continue;
   end Cb_Set_Break;

   procedure Break_Proc (Line : String)
   is
      Status : Walk_Status;
      P : Natural;
   begin
      P := Skip_Blanks (Line);
      if Line (P) = '"' then
         --  An operator name.
         declare
            use Str_Table;
            Str : String8_Id;
            Len : Nat32;
         begin
            Str := Create_String8;
            Len := 0;
            P := P + 1;
            while Line (P) /= '"' loop
               Append_String8_Char (Line (P));
               Len := Len + 1;
               P := P + 1;
            end loop;
            Break_Id := Vhdl.Parse.Str_To_Operator_Name
              (Str, Len, No_Location);
            --  FIXME: free string.
            --  FIXME: catch error.
         end;
      else
         Break_Id := Name_Table.Get_Identifier (Line (P .. Line'Last));
      end if;
      Status := Walk_Declarations (Cb_Set_Break'Access);
      pragma Assert (Status = Walk_Continue);
   end Break_Proc;

   procedure Help_Proc (Line : String);

   procedure Prepare_Continue is
   begin
      Command_Status := Status_Quit;

      --  Set Flag_Need_Debug only if there is at least one enabled breakpoint.
      Flag_Need_Debug := False;
      for I in Breakpoints.First .. Breakpoints.Last loop
         Flag_Need_Debug := True;
         exit;
      end loop;
   end Prepare_Continue;

   procedure Cont_Proc (Line : String) is
      pragma Unreferenced (Line);
   begin
      Prepare_Continue;
   end Cont_Proc;

   procedure List_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Disp_Current_Lines;
   end List_Proc;

   Menu_Info_Locals : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("locals"),
      Next => null, -- Menu_Info_Tree'Access,
      Proc => Info_Locals_Proc'Access);

   Menu_Info_Params : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("param*eters"),
      Next => Menu_Info_Locals'Access, -- Menu_Info_Tree'Access,
      Proc => Info_Params_Proc'Access);

   Menu_Info : aliased Menu_Entry :=
     (Kind => Menu_Submenu,
      Name => new String'("i*nfo"),
      Next => null, -- Menu_Ps'Access,
      First | Last => Menu_Info_Params'Access); --  Menu_Info_Proc'Access);

   Menu_List : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("l*list"),
      Next => Menu_Info'Access, -- null,
      Proc => List_Proc'Access);

   Menu_Cont : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("c*ont"),
      Next => Menu_List'Access, --Menu_Print'Access,
      Proc => Cont_Proc'Access);

   Menu_Nstmt : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("ns*tmt"),
      Next => Menu_Cont'Access, -- Menu_Up'Access,
      Proc => Next_Stmt_Proc'Access);

   Menu_Fstmt : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("fs*tmt"),
      Next => Menu_Nstmt'Access,
      Proc => Finish_Stmt_Proc'Access);

   Menu_Next : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("n*ext"),
      Next => Menu_Fstmt'Access,
      Proc => Next_Proc'Access);

   Menu_Step : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("s*tep"),
      Next => Menu_Next'Access,
      Proc => Step_Proc'Access);

   Menu_Break : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("b*reak"),
      Next => Menu_Step'Access,
      Proc => Break_Proc'Access);

   Menu_Help2 : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("?"),
      Next => Menu_Break'Access, --  Menu_Help1'Access,
      Proc => Help_Proc'Access);

   Menu_Top : aliased Menu_Entry :=
     (Kind => Menu_Submenu,
      Name => null,
      Next => null,
      First | Last => Menu_Help2'Access);


   function Find_Menu (Menu : Menu_Entry_Acc; Cmd : String)
                      return Menu_Entry_Acc
   is
      function Is_Cmd (Cmd_Name : String; Str : String) return Boolean
      is
         -- Number of characters that were compared.
         P : Natural;
      begin
         P := 0;
         --  Prefix (before the '*').
         loop
            if P = Cmd_Name'Length then
               --  Full match.
               return P = Str'Length;
            end if;
            exit when Cmd_Name (Cmd_Name'First + P) = '*';
            if P = Str'Length then
               --  Command is too short
               return False;
            end if;
            if Cmd_Name (Cmd_Name'First + P) /= Str (Str'First + P) then
               return False;
            end if;
            P := P + 1;
         end loop;
         --  Suffix (after the '*')
         loop
            if P = Str'Length then
               return True;
            end if;
            if P + 1 = Cmd_Name'Length then
               --  String is too long
               return False;
            end if;
            if Cmd_Name (Cmd_Name'First + P + 1) /= Str (Str'First + P) then
               return False;
            end if;
            P := P + 1;
         end loop;
      end Is_Cmd;
      Ent : Menu_Entry_Acc;
   begin
      Ent := Menu.First;
      while Ent /= null loop
         if Is_Cmd (Ent.Name.all, Cmd) then
            return Ent;
         end if;
         Ent := Ent.Next;
      end loop;
      return null;
   end Find_Menu;

   procedure Parse_Command (Line : String;
                            P : in out Natural;
                            Menu : out Menu_Entry_Acc)
   is
      E : Natural;
   begin
      P := Skip_Blanks (Line (P .. Line'Last));
      if P > Line'Last then
         return;
      end if;
      E := Get_Word (Line (P .. Line'Last));
      Menu := Find_Menu (Menu, Line (P .. E));
      if Menu = null then
         Put_Line ("command '" & Line (P .. E) & "' not found");
      end if;
      P := E + 1;
   end Parse_Command;

   procedure Help_Proc (Line : String)
   is
      P : Natural;
      Root : Menu_Entry_Acc := Menu_Top'access;
   begin
      Put_Line ("This is the help command");
      P := Line'First;
      while P < Line'Last loop
         Parse_Command (Line, P, Root);
         if Root = null then
            return;
         elsif Root.Kind /= Menu_Submenu then
            Put_Line ("Menu entry " & Root.Name.all & " is not a submenu");
            return;
         end if;
      end loop;

      Root := Root.First;
      while Root /= null loop
         Put (Root.Name.all);
         if Root.Kind = Menu_Submenu then
            Put (" (menu)");
         end if;
         New_Line;
         Root := Root.Next;
      end loop;
   end Help_Proc;

   procedure Debug (Reason: Debug_Reason)
   is
      use Grt.Readline;
      Raw_Line : Char_Ptr;
      Prompt : System.Address;
   begin
      Prompt := Prompt_Debug'Address;

      case Reason is
         when Reason_Init =>
            Prompt := Prompt_Init'Address;
         when Reason_Error =>
            Prompt := Prompt_Error'Address;
         when Reason_Break =>
            case Exec_State is
               when Exec_Run =>
                  if not Is_Breakpoint_Hit then
                     return;
                  end if;
                  Put_Line ("breakpoint hit");
               when Exec_Single_Step =>
                  null;
               when Exec_Next =>
                  if Current_Instance /= Exec_Instance then
                     return;
                  end if;
               when Exec_Next_Stmt =>
                  if Current_Instance /= Exec_Instance
                    or else Is_Within_Statement (Exec_Statement, Current_Loc)
                  then
                     return;
                  end if;
            end case;
            --  Default state.
            Exec_State := Exec_Run;

      end case;

      case Reason is
         when Reason_Error
           | Reason_Break =>
            Put ("stopped at: ");
            Disp_Iir_Location (Current_Loc);
            New_Line;
            Disp_Source_Line (Get_Location (Current_Loc));
         when others =>
            null;
      end case;

      if Current_Loc /= Null_Node then
         Set_List_Current (Get_Location (Current_Loc));
      end if;

      Command_Status := Status_Default;

      loop
         loop
            Raw_Line := Readline (Prompt);
            --  Skip empty lines
            if Raw_Line = null or else Raw_Line (1) = ASCII.NUL then
               if Cmd_Repeat /= null then
                  Cmd_Repeat.all ("");
                  case Command_Status is
                     when Status_Default =>
                        null;
                     when Status_Quit =>
                        return;
                  end case;
               end if;
            else
               Cmd_Repeat := null;
               exit;
            end if;
         end loop;
         declare
            Line_Last : constant Natural := Strlen (Raw_Line);
            Line : String renames Raw_Line (1 .. Line_Last);
            P, E : Positive;
            Cmd : Menu_Entry_Acc := Menu_Top'Access;
         begin
            --  Find command
            P := 1;
            loop
               E := P;
               Parse_Command (Line, E, Cmd);
               exit when Cmd = null;
               case Cmd.Kind is
                  when Menu_Submenu =>
                     if E > Line_Last then
                        Put_Line ("missing command for submenu "
                                    & Line (P .. E - 1));
                        Cmd := null;
                        exit;
                     end if;
                     P := E;
                  when Menu_Command =>
                     exit;
               end case;
            end loop;

            if Cmd /= null then
               Cmd.Proc.all (Line (E .. Line_Last));

               case Command_Status is
                  when Status_Default =>
                     null;
                  when Status_Quit =>
                     exit;
               end case;
            end if;
         exception
            when Command_Error =>
               null;
         end;
      end loop;
      --  Put ("resuming");
   end Debug;

   procedure Debug_Init (Top : Node) is
   begin
      Current_Instance := null;
      Current_Loc := Top;

      --  To avoid warnings.
      Exec_Statement := Null_Node;
      Exec_Instance := null;

      Debug (Reason_Init);
   end Debug_Init;

   procedure Debug_Break (Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      Current_Instance := Inst;
      Current_Loc := Stmt;

      Debug (Reason_Break);
   end Debug_Break;

   procedure Debug_Leave (Inst : Synth_Instance_Acc) is
   begin
      if Exec_Instance = Inst then
         --  Will be destroyed.
         Exec_Instance := null;

         case Exec_State is
            when Exec_Run =>
               null;
            when Exec_Single_Step =>
               null;
            when Exec_Next
              | Exec_Next_Stmt =>
               --  Leave the frame, will stop just after.
               Exec_State := Exec_Single_Step;
         end case;
      end if;
   end Debug_Leave;

   procedure Debug_Error (Inst : Synth_Instance_Acc; Expr : Node) is
   begin
      if Flags.Flag_Debug_Enable then
         Current_Instance := Inst;
         Current_Loc := Expr;
         Debug (Reason_Error);
      end if;
   end Debug_Error;
end Synth.Debugger;
