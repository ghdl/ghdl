--  Debugging during synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Files_Map;
with Tables;
with Simple_IO; use Simple_IO;
with Name_Table;
with Str_Table;

with Grt.Types; use Grt.Types;

with Vhdl.Errors;
with Vhdl.Utils;
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;
with Vhdl.Parse;

with Elab.Vhdl_Context.Debug; use Elab.Vhdl_Context.Debug;
with Elab.Vhdl_Debug; use Elab.Vhdl_Debug;

package body Elab.Debugger is
   --  TODO:
   --  * restart
   --  * step in declarations
   --  * stop at subprogram entry
   --  * for step: stop at loop cond/for.

   --  Instance and statement at the breakpoint.
   Current_Instance : Synth_Instance_Acc;
   Current_Loc : Node;

   Current_Frame : Synth_Instance_Acc;
   Current_Frame_Stmt : Node;

   type Debug_Reason is
     (
      Reason_Init,
      Reason_Break,
      Reason_Time,
      Reason_Error
     );

   procedure Get_Debug_Frame (Inst : out Synth_Instance_Acc;
                              Stmt : out Node) is
   begin
      Inst := Current_Frame;
      Stmt := Current_Frame_Stmt;
   end Get_Debug_Frame;

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
   begin
      Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
      Put_Line (Extract_Expanded_Line (File, Line));
   end Disp_Source_Line;

   function Skip_Blanks (S : String; F : Positive) return Positive is
   begin
      return Skip_Blanks (S (F .. S'Last));
   end Skip_Blanks;

   function Get_Word (S : String; F : Positive) return Positive is
   begin
      return Get_Word (S (F .. S'Last));
   end Get_Word;

   procedure To_Num (Str : String; Res : out Uns32; Valid : out Boolean) is
   begin
      Res := 0;
      if Str'Length = 0 then
         Valid := False;
         return;
      end if;
      Valid := True;
      for P in Str'Range loop
         if Str (P) in '0' .. '9' then
            Res := Res * 10 + Character'Pos (Str (P)) - Character'Pos ('0');
         else
            Valid := False;
            return;
         end if;
      end loop;
   end To_Num;

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
              | Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Architecture_Body =>
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
               | Iir_Kind_Function_Body
               | Iir_Kind_Process_Statement
               | Iir_Kind_Sensitized_Process_Statement
               | Iir_Kind_Generate_Statement_Body
               | Iir_Kind_Architecture_Body =>
               Decls := Get_Declaration_Chain (Decl);
               exit;
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

   procedure Info_Instance_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Debug_Synth_Instance (Current_Instance);
   end Info_Instance_Proc;

   procedure Info_Files_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      use Files_Map;
   begin
      for I in No_Source_File_Entry + 1 .. Get_Last_Source_File_Entry loop
         Debug_Source_File (I);
      end loop;
   end Info_Files_Proc;

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
              and then not Vhdl.Utils.Is_Second_Subprogram_Specification (El)
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
      if P > Line'Last then
         Put_Line ("missing subprogram name");
         return;
      end if;
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

   procedure Disp_A_Frame (Inst: Synth_Instance_Acc)
   is
      use Vhdl.Utils;
      Src : Node;
   begin
      if Inst = Root_Instance then
         Put_Line ("root instance");
         return;
      end if;

      Src := Get_Source_Scope (Inst);
      case Get_Kind (Src) is
         when Iir_Kind_Procedure_Body =>
            Put ("procedure ");
            Put (Image_Identifier (Get_Subprogram_Specification (Src)));
         when Iir_Kind_Function_Body =>
            Put ("function ");
            Put (Image_Identifier (Get_Subprogram_Specification (Src)));
         when others =>
            Put (Vhdl.Errors.Disp_Node (Src));
      end case;
      Put (" at ");
      Put (Files_Map.Image (Get_Location (Src)));
      New_Line;
   end Disp_A_Frame;

   procedure Debug_Bt (Instance : Synth_Instance_Acc)
   is
      Inst : Synth_Instance_Acc;
   begin
      Inst := Instance;
      while Inst /= null loop
         Disp_A_Frame (Inst);
         Inst := Get_Caller_Instance (Inst);
      end loop;
   end Debug_Bt;
   pragma Unreferenced (Debug_Bt);

   procedure Where_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Inst : Synth_Instance_Acc;
   begin
      --  Check_Current_Process;
      Inst := Current_Instance;
      while Inst /= null loop
         if Inst = Current_Frame then
            Put ("* ");
         else
            Put ("  ");
         end if;
         Disp_A_Frame (Inst);
         Inst := Get_Caller_Instance (Inst);
      end loop;
   end Where_Proc;

   procedure Up_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Inst : Synth_Instance_Acc;
   begin
      Inst := Get_Caller_Instance (Current_Frame);
      if Inst = null then
         Put_Line ("top frame reached");
         return;
      end if;
      Current_Frame_Stmt := Get_Statement_Scope (Current_Frame);
      Current_Frame := Inst;
   end Up_Proc;

   procedure List_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Disp_Current_Lines;
   end List_Proc;

   procedure List_Hierarchy (Line : String)
   is
      With_Objs : Boolean;
      Recurse : Boolean;
      List_Pkg : Boolean;
      F, L : Natural;
   begin
      With_Objs := False;
      Recurse := False;
      List_Pkg := False;

      F := Line'First;
      loop
         F := Skip_Blanks (Line, F);
         exit when F > Line'Last;
         L := Get_Word (Line, F);
         if Line (F) = '-' then
            for I in F + 1 .. L loop
               if Line (I) = 'v' then
                  With_Objs := True;
               elsif Line (I) = 'R' then
                  Recurse := True;
               elsif Line (I) = 'p' then
                  List_Pkg := True;
               elsif Line (I) = 'h' then
                  Put_Line ("options:");
                  Put_Line (" -h   this help");
                  Put_Line (" -p   top-level packages only");
                  Put_Line (" -v   with objects");
                  Put_Line (" -R   recurses");
                  return;
               else
                  Put_Line ("unknown option: -" & Line (I));
                  return;
               end if;
            end loop;
         else
            Put_Line ("unknown option: " & Line (F .. L));
            return;
         end if;
         F := L + 1;
      end loop;

      if List_Pkg then
         declare
            It : Iterator_Top_Level_Type := Iterator_Top_Level_Init;
            Pkg_Inst : Synth_Instance_Acc;
         begin
            loop
               Iterate_Top_Level (It, Pkg_Inst);
               exit when Pkg_Inst = null;
               Disp_Top_Package (Pkg_Inst, With_Objs);
            end loop;
         end;
      else
         Disp_Hierarchy (Current_Instance, Recurse, With_Objs);
      end if;
   end List_Hierarchy;

   procedure Change_Hierarchy (Line : String)
   is
      F : Natural;
      Res : Synth_Instance_Acc;
   begin
      F := Skip_Blanks (Line);
      if Line (F .. Line'Last) = ".." then
         Res := Get_Instance_Path_Parent (Current_Instance);
         if Res = null then
            Put_Line ("already at top");
            return;
         end if;
      else
         Res := Get_Sub_Instance_By_Name (Current_Instance,
                                          Line (F .. Line'Last));
         if Res = null then
            Put_Line ("no such sub-instance");
            return;
         end if;
      end if;
      Current_Instance := Res;
      Current_Loc := Null_Node;
   end Change_Hierarchy;

   procedure Print_Hierarchy_Path (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Disp_Instance_Path (Current_Instance);
      New_Line;
   end Print_Hierarchy_Path;

   Menu_Info_Files : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("files"),
      Help => new String'("display files info"),
      Next => null, -- Menu_Info_Tree'Access,
      Proc => Info_Files_Proc'Access);

   Menu_Info_Instance : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("inst*ance"),
      Help => new String'("display instance info"),
      Next => Menu_Info_Files'Access,
      Proc => Info_Instance_Proc'Access);

   Menu_Info_Locals : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("locals"),
      Help => new String'("display local objects"),
      Next => Menu_Info_Instance'Access,
      Proc => Info_Locals_Proc'Access);

   Menu_Info_Params : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("param*eters"),
      Help => new String'("display parameters"),
      Next => Menu_Info_Locals'Access, -- Menu_Info_Tree'Access,
      Proc => Info_Params_Proc'Access);

   Menu_Info : aliased Menu_Entry :=
     (Kind => Menu_Submenu,
      Name => new String'("i*nfo"),
      Help => null,
      Next => null, -- Menu_Ps'Access,
      First => Menu_Info_Params'Access); --  Menu_Info_Proc'Access);

   Menu_Pwh : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("pwh"),
      Help => new String'("display current hierarchy path"),
      Next => Menu_Info'Access,
      Proc => Print_Hierarchy_Path'Access);

   Menu_Ch : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("ch"),
      Help => new String'("change hierarchy path"),
      Next => Menu_Pwh'Access,
      Proc => Change_Hierarchy'Access);

   Menu_Lh : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("lh"),
      Help => new String'("list hierarchy"),
      Next => Menu_Ch'Access,
      Proc => List_Hierarchy'Access);

   Menu_List : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("l*list"),
      Help => new String'("list source around current line"),
      Next => Menu_Lh'Access,
      Proc => List_Proc'Access);

   Menu_Cont : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("c*ont"),
      Help => new String'("continue simulation"),
      Next => Menu_List'Access, --Menu_Print'Access,
      Proc => Cont_Proc'Access);

   Menu_Nstmt : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("ns*tmt"),
      Help => new String'("execute statement (next statement)"),
      Next => Menu_Cont'Access, -- Menu_Up'Access,
      Proc => Next_Stmt_Proc'Access);

   Menu_Fstmt : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("fs*tmt"),
      Help => new String'("execute until end of subprogram"),
      Next => Menu_Nstmt'Access,
      Proc => Finish_Stmt_Proc'Access);

   Menu_Next : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("n*ext"),
      Help => new String'("execute to next statement"),
      Next => Menu_Fstmt'Access,
      Proc => Next_Proc'Access);

   Menu_Step : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("s*tep"),
      Help => new String'("execute one statement"),
      Next => Menu_Next'Access,
      Proc => Step_Proc'Access);

   Menu_Break : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("b*reak"),
      Help => new String'("set a breakpoint (or list then)"),
      Next => Menu_Step'Access,
      Proc => Break_Proc'Access);

   Menu_Up : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("up"),
      Help => new String'("up frame"),
      Next => Menu_Break'Access,
      Proc => Up_Proc'Access);

   Menu_Where : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("w*here"),
      Help => new String'("disp call stack"),
      Next => Menu_Up'Access,
      Proc => Where_Proc'Access);

   Menu_Help2 : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("?"),
      Help => new String'("print help"),
      Next => Menu_Where'Access,
      Proc => Help_Proc'Access);

   Menu_Top : aliased Menu_Entry :=
     (Kind => Menu_Submenu,
      Help => null,
      Name => null,
      Next => null,
      First => Menu_Help2'Access);

   procedure Append_Menu_Command (Name : Cst_String_Acc;
                                  Help : Cst_String_Acc;
                                  Proc : Menu_Procedure) is
   begin
      Append_Menu (Menu_Top, Name, Help, Proc);
   end Append_Menu_Command;

   procedure Append_Info_Command (Name : Cst_String_Acc;
                                  Help : Cst_String_Acc;
                                  Proc : Menu_Procedure) is
   begin
      Append_Menu (Menu_Info, Name, Help, Proc);
   end Append_Info_Command;

   procedure Debug (Reason: Debug_Reason)
   is
      Prev_Hook : constant Error_Hook_Type := Error_Hook;
      Prompt : Ghdl_C_String;
   begin
      --  Do not call the error hook on nested debug.
      Error_Hook := null;

      Prompt := To_Ghdl_C_String (Prompt_Debug'Address);

      case Reason is
         when Reason_Init =>
            Prompt := To_Ghdl_C_String (Prompt_Init'Address);
         when Reason_Error =>
            Prompt := To_Ghdl_C_String (Prompt_Error'Address);
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
         when Reason_Time =>
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

      Debug_Loop (Prompt);

      --  Put ("resuming");

      Error_Hook := Prev_Hook;
   end Debug;

   procedure Debug_Init (Top : Node) is
   begin
      Flag_Debug_Enable := True;

      Current_Instance := null;
      Current_Loc := Top;

      Current_Frame := null;
      Current_Frame_Stmt := Null_Node;

      --  To avoid warnings.
      Exec_Statement := Null_Node;
      Exec_Instance := null;

      Debug (Reason_Init);
   end Debug_Init;

   procedure Debug_Elab (Top : Synth_Instance_Acc) is
   begin
      Current_Instance := Top;
      Current_Loc := Get_Source_Scope (Top);

      Current_Frame := Top;
      Current_Frame_Stmt := Null_Node;

      Flag_Debug_Enable := True;

      --  To avoid warnings.
      Exec_Statement := Null_Node;
      Exec_Instance := null;

      Debug (Reason_Init);
   end Debug_Elab;

   procedure Debug_Break (Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      Current_Instance := Inst;
      Current_Loc := Stmt;

      Current_Frame := Inst;
      Current_Frame_Stmt := Stmt;

      Debug (Reason_Break);
   end Debug_Break;

   procedure Debug_Time (Top : Synth_Instance_Acc) is
   begin
      Current_Instance := Top;
      Current_Loc := Null_Node;

      Current_Frame := Top;
      Current_Frame_Stmt := Null_Node;

      Debug (Reason_Time);
   end Debug_Time;

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
      if Flag_Debug_Enable then
         Current_Instance := Inst;
         Current_Loc := Expr;

         Current_Frame := Inst;
         Current_Frame_Stmt := Null_Node;

         Debug (Reason_Error);
      end if;
      if Error_Hook /= null then
         Error_Hook.all;
      end if;
   end Debug_Error;

begin
   Debuggers.Menu_Top := Menu_Top'Access;
end Elab.Debugger;
