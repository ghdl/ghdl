--  Debugger for interpreter
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

with System;
with Tables;
with Types; use Types;
with Name_Table;
with Str_Table;
with Files_Map;
with Vhdl.Parse;
with Vhdl.Scanner;
with Vhdl.Tokens;
with Vhdl.Sem_Expr;
with Vhdl.Sem_Scopes;
with Vhdl.Canon;
with Std_Names;
with Libraries;
with Vhdl.Std_Package;
with Vhdl.Annotations; use Vhdl.Annotations;
with Simul.Elaboration; use Simul.Elaboration;
with Simul.Execution; use Simul.Execution;
with Vhdl.Utils; use Vhdl.Utils;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Prints;
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;
with Areapools; use Areapools;
with Grt.Types; use Grt.Types;
with Grt.Disp;
with Grt.Readline;
with Grt.Errors;
with Grt.Disp_Signals;
with Grt.Signals; use Grt.Signals;
with Grt.Processes;
with Grt.Options;
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;

package body Simul.Debugger is
   --  This exception can be raised by a debugger command to directly return
   --  to the prompt.
   Command_Error : exception;

   type Menu_Procedure is access procedure (Line : String);

   --  If set (by commands), call this procedure on empty line to repeat
   --  last command.
   Cmd_Repeat : Menu_Procedure;

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

   Dbg_Top_Frame : Block_Instance_Acc;
   Dbg_Cur_Frame : Block_Instance_Acc;

   procedure Set_Cur_Frame (Frame : Block_Instance_Acc) is
   begin
      Dbg_Cur_Frame := Frame;
   end Set_Cur_Frame;

   procedure Set_Top_Frame (Frame : Block_Instance_Acc) is
   begin
      Dbg_Top_Frame := Frame;
      Set_Cur_Frame (Frame);
   end Set_Top_Frame;

   type Breakpoint_Entry is record
      Stmt : Iir;
   end record;

   package Breakpoints is new Tables
     (Table_Index_Type => Natural,
      Table_Component_Type => Breakpoint_Entry,
      Table_Low_Bound => 1,
      Table_Initial => 16);

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
   Exec_Instance : Block_Instance_Acc;

   --  Current statement for next_stmt.
   Exec_Statement : Iir;

   procedure Disp_Iir_Location (N : Iir) is
   begin
      if N = Null_Iir then
         Put (stderr, "??:??:??");
      else
         Put (stderr, Disp_Location (N));
      end if;
      Put (stderr, ": ");
   end Disp_Iir_Location;

   -- Disp a message during execution.
   procedure Error_Msg_Exec (Msg: String; Loc: in Iir) is
   begin
      Disp_Iir_Location (Loc);
      Put_Line (stderr, Msg);
      Grt.Errors.Fatal_Error;
   end Error_Msg_Exec;

   procedure Warning_Msg_Exec (Msg: String; Loc: Iir) is
   begin
      Disp_Iir_Location (Loc);
      Put (stderr, "warning: ");
      Put_Line (stderr, Msg);
   end Warning_Msg_Exec;

   -- Disp a message for a constraint error.
   procedure Error_Msg_Constraint (Expr: in Iir) is
   begin
      if Expr /= Null_Iir then
         Disp_Iir_Location (Expr);
      end if;
      Put (stderr, "constraint violation");
      if Expr /= Null_Iir then
         case Get_Kind (Expr) is
            when Iir_Kind_Addition_Operator =>
               Put_Line (stderr, " in the ""+"" operation");
            when Iir_Kind_Substraction_Operator =>
               Put_Line (stderr, " in the ""-"" operation");
            when Iir_Kind_Integer_Literal =>
               Put_Line (stderr, ", literal out of range");
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Signal_Declaration =>
               Put_Line (stderr, " for " & Disp_Node (Expr));
            when others =>
               New_Line (stderr);
         end case;
      end if;
      Grt.Errors.Fatal_Error;
   end Error_Msg_Constraint;

   function Get_Instance_Local_Name (Instance : Block_Instance_Acc;
                                     Short : Boolean := False)
                                    return String
   is
      Name : constant Iir := Instance.Label;
   begin
      if Name = Null_Iir then
         return "<anon>";
      end if;

      case Get_Kind (Name) is
         when Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Procedure_Declaration
           | Iir_Kinds_Process_Statement
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Configuration_Declaration =>
            return Image_Identifier (Name);
         when Iir_Kind_Generate_Statement_Body =>
            return Image_Identifier (Get_Parent (Name))
              & '(' & Image_Identifier (Name) & ')';
         when Iir_Kind_Iterator_Declaration =>
            return Image_Identifier (Get_Parent (Name)) & '('
              & Execute_Image_Attribute
              (Instance.Objects (Get_Info (Name).Slot), Get_Type (Name))
              & ')';
         when Iir_Kind_Architecture_Body =>
            if Short then
               return Image_Identifier (Get_Entity (Name));
            else
               return Image_Identifier (Get_Entity (Name))
                 & '(' & Image_Identifier (Name) & ')';
            end if;
         when others =>
            Error_Kind ("disp_instance_local_name", Name);
      end case;
   end Get_Instance_Local_Name;

   -- Disp the name of an instance, without newline.
   procedure Disp_Instance_Name (Instance: Block_Instance_Acc;
                                 Short : Boolean := False) is
   begin
      if Instance.Parent /= null then
         Disp_Instance_Name (Instance.Parent);
         Put ('.');
      end if;
      Put (Get_Instance_Local_Name (Instance, Short));
   end Disp_Instance_Name;

   function Get_Instance_Name (Instance: Block_Instance_Acc) return String
   is
      function Parent_Name return String is
      begin
         if Instance.Parent /= null then
            return Get_Instance_Name (Instance.Parent) & '.';
         else
            return "";
         end if;
      end Parent_Name;
   begin
      return Parent_Name & Get_Instance_Local_Name (Instance);
   end Get_Instance_Name;

   procedure Disp_Instances_Tree_Name (Inst : Block_Instance_Acc) is
   begin
      if Inst = null then
         Put ("*null*");
         New_Line;
         return;
      end if;
      Put (Get_Instance_Local_Name (Inst));

      Put (" ");
      case Get_Kind (Inst.Label) is
         when Iir_Kind_Block_Statement =>
            Put ("[block]");
         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body =>
            Put ("[generate]");
         when Iir_Kind_Iterator_Declaration =>
            Put ("[iterator]");
         when Iir_Kind_Component_Instantiation_Statement =>
            Put ("[component]");
         when Iir_Kinds_Process_Statement =>
            Put ("[process]");
         when Iir_Kind_Architecture_Body =>
            Put ("[entity]");
         when Iir_Kind_Package_Declaration =>
            Put ("[package]");
         when Iir_Kind_Configuration_Declaration =>
            Put ("[configuration]");
         when others =>
            Error_Kind ("disp_instances_tree_name", Inst.Label);
      end case;
      New_Line;
   end Disp_Instances_Tree_Name;

   procedure Disp_Instances_Tree1 (Inst : Block_Instance_Acc; Pfx : String)
   is
      Child : Block_Instance_Acc;
   begin
      Child := Inst.Children;
      if Child = null then
         return;
      end if;

      loop
         if Child.Brother /= null then
            Put (Pfx & "+-");
            Disp_Instances_Tree_Name (Child);

            Disp_Instances_Tree1 (Child, Pfx & "| ");
            Child := Child.Brother;
         else
            Put (Pfx & "`-");
            Disp_Instances_Tree_Name (Child);

            Disp_Instances_Tree1 (Child, Pfx & "  ");
            exit;
         end if;
      end loop;
   end Disp_Instances_Tree1;

   procedure Disp_Instances_Tree is
   begin
      for I in Global_Instances.Objects'Range loop
         if Global_Instances.Objects (I) /= null then
            Disp_Instances_Tree_Name (Global_Instances.Objects (I).Instance);
         end if;
      end loop;
      Disp_Instances_Tree_Name (Top_Instance);
      Disp_Instances_Tree1 (Top_Instance, "");
   end Disp_Instances_Tree;

   --  Disp a block instance, in a human readable way.
   --  Used to debug.
   procedure Disp_Block_Instance (Instance: Block_Instance_Acc) is
   begin
      Put_Line ("Objects:");
      for I in Instance.Objects'Range loop
         Put (Object_Slot_Type'Image (I) & ": ");
         Disp_Value_Tab (Instance.Objects (I), 3);
         New_Line;
      end loop;
   end Disp_Block_Instance;

   procedure Disp_Signal (Value : Iir_Value_Literal_Acc; A_Type : Iir);

   procedure Disp_Signal_Array (Value : Iir_Value_Literal_Acc;
                                A_Type : Iir;
                                Dim : Natural)
   is
   begin
      if Dim = Get_Nbr_Elements (Get_Index_Subtype_List (A_Type)) then
         Put ("(");
         for I in Value.Val_Array.V'Range loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Signal (Value.Val_Array.V (I), Get_Element_Subtype (A_Type));
         end loop;
         Put (")");
      else
         Put ("(");
         Disp_Signal_Array (Value, A_Type, Dim + 1);
         Put (")");
      end if;
   end Disp_Signal_Array;

   procedure Disp_Signal_Record (Value : Iir_Value_Literal_Acc; A_Type : Iir)
   is
      List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (A_Type));
      El : Iir_Element_Declaration;
   begin
      Put ("(");
      for I in Value.Val_Record.V'Range loop
         El := Get_Nth_Element (List, Natural (I - 1));
         if I /= 1 then
            Put (", ");
         end if;
         Put (Name_Table.Image (Get_Identifier (El)));
         Put (" => ");
         Disp_Signal (Value.Val_Record.V (I), Get_Type (El));
      end loop;
      Put (")");
   end Disp_Signal_Record;

   procedure Disp_Signal_Value
     (Val : Value_Union; Mode : Mode_Type; Sig_Type : Iir) is
   begin
      case Mode is
         when Mode_I64 =>
            Put (Ghdl_I64'Image (Val.I64));
         when Mode_I32 =>
            Put (Ghdl_I32'Image (Val.I32));
         when Mode_F64 =>
            Put (Ghdl_F64'Image (Val.F64));
         when Mode_E32 =>
            Disp_Iir_Value_Enum (Ghdl_E32'Pos (Val.E32), Sig_Type);
         when Mode_E8 =>
            Disp_Iir_Value_Enum (Ghdl_E8'Pos (Val.E8), Sig_Type);
         when Mode_B1 =>
            Disp_Iir_Value_Enum (Ghdl_B1'Pos (Val.B1), Sig_Type);
      end case;
   end Disp_Signal_Value;

   procedure Disp_Transaction
     (Head : Transaction_Acc; Mode : Mode_Type; Sig_Type : Iir)
   is
      Trans : Transaction_Acc;
   begin
      Trans := Head;
      loop
         case Trans.Kind is
            when Trans_Value =>
               Disp_Signal_Value (Trans.Val, Mode, Sig_Type);
            when Trans_Direct =>
               Disp_Signal_Value (Trans.Val_Ptr.all, Mode, Sig_Type);
            when Trans_Null =>
               Put ("NULL");
            when Trans_Error =>
               Put ("ERROR");
         end case;
         if Trans.Kind = Trans_Direct then
            Put ("[DIRECT]");
         else
            Put ("@");
            Put_Time (stdout, Trans.Time);
         end if;
         Trans := Trans.Next;
         exit when Trans = null;
         Put (", ");
      end loop;
   end Disp_Transaction;

   procedure Disp_Signal (Value : Iir_Value_Literal_Acc; A_Type : Iir) is
   begin
      if Value = null then
         Put ("!NULL!");
         return;
      end if;
      case Value.Kind is
         when Iir_Value_Scalars
           | Iir_Value_Access =>
            Disp_Iir_Value (Value, A_Type);
         when Iir_Value_Array =>
            Disp_Signal_Array (Value, A_Type, 1);
         when Iir_Value_Record =>
            Disp_Signal_Record (Value, A_Type);
         when Iir_Value_Range =>
            -- FIXME.
            raise Internal_Error;
         when Iir_Value_Signal =>
            declare
               Sig : constant Ghdl_Signal_Ptr := Value.Sig;
            begin
               Disp_Signal_Value (Sig.Value_Ptr.all, Sig.Mode, A_Type);
               Grt.Disp_Signals.Disp_Single_Signal_Attributes (Value.Sig);
               New_Line;
               if Sig.S.Mode_Sig in Mode_Signal_User then
                  for I in 1 .. Sig.S.Nbr_Drivers loop
                     Put ("    ");
                     Disp_Transaction (Sig.S.Drivers (I - 1).First_Trans,
                                       Sig.Mode, A_Type);
                     New_Line;
                  end loop;
               end if;
            end;
         when Iir_Value_File
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Disp_Signal;

   procedure Disp_Instance_Signal (Instance: Block_Instance_Acc; Decl : Iir)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
   begin
      Put ("  ");
      Put (Name_Table.Image (Get_Identifier (Decl)));
      Put (" = ");
      Disp_Signal (Instance.Objects (Info.Slot), Get_Type (Decl));
   end Disp_Instance_Signal;

   procedure Disp_Instance_Signals_Of_Chain (Instance: Block_Instance_Acc;
                                             Chain : Iir)
   is
      El : Iir;
   begin
      El := Chain;
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Interface_Signal_Declaration =>
               Disp_Instance_Signal (Instance, El);
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
   end Disp_Instance_Signals_Of_Chain;

   procedure Disp_Instance_Signals (Instance: Block_Instance_Acc)
   is
      Blk : constant Iir := Instance.Label;
      Child: Block_Instance_Acc;
   begin
      case Get_Kind (Blk) is
         when Iir_Kind_Architecture_Body =>
            declare
               Ent : constant Iir := Get_Entity (Blk);
            begin
               Disp_Instance_Name (Instance);
               Put_Line (" [architecture]:");

               Disp_Instance_Signals_Of_Chain
                 (Instance, Get_Port_Chain (Ent));
               Disp_Instance_Signals_Of_Chain
                 (Instance, Get_Declaration_Chain (Ent));
               Disp_Instance_Signals_Of_Chain
                 (Instance, Get_Declaration_Chain (Blk));
            end;
         when Iir_Kind_Block_Statement =>
            Disp_Instance_Name (Instance);
            Put_Line (" [block]:");

            declare
               Header : constant Iir := Get_Block_Header (Blk);
            begin
               if Header /= Null_Iir then
                  Disp_Instance_Signals_Of_Chain
                    (Instance, Get_Port_Chain (Header));
               end if;
            end;
            Disp_Instance_Signals_Of_Chain
              (Instance, Get_Declaration_Chain (Blk));

         when Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement =>
            Disp_Instance_Name (Instance);
            Put_Line (" [generate]:");

         when Iir_Kind_Generate_Statement_Body =>
            Disp_Instance_Signals_Of_Chain
              (Instance, Get_Declaration_Chain (Blk));
         when Iir_Kind_Component_Instantiation_Statement =>
            Disp_Instance_Name (Instance);
            Put_Line (" [component]:");
            Disp_Instance_Signals_Of_Chain
              (Instance, Get_Port_Chain (Instance.Stmt));
         when Iir_Kinds_Process_Statement =>
            null;
         when Iir_Kind_Iterator_Declaration =>
            null;
         when others =>
            Error_Kind ("disp_instance_signals", Instance.Label);
      end case;

      Child := Instance.Children;
      while Child /= null loop
         Disp_Instance_Signals (Child);
         Child := Child.Brother;
      end loop;
   end Disp_Instance_Signals;

   --  Disp all signals name and values.
   procedure Disp_Signals_Value is
   begin
      if Disp_Time_Before_Values then
         Grt.Disp.Disp_Now;
      end if;
      Disp_Instance_Signals (Top_Instance);
   end Disp_Signals_Value;

   procedure Disp_Label (Process : Iir)
   is
      Label : Name_Id;
   begin
      Label := Get_Label (Process);
      if Label = Null_Identifier then
         Put ("<unlabeled>");
      else
         Put (Name_Table.Image (Label));
      end if;
   end Disp_Label;

   procedure Disp_Declaration_Object
     (Instance : Block_Instance_Acc; Decl : Iir) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration
           | Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            Put (Disp_Node (Decl));
            Put (" = ");
            Disp_Value_Tab (Instance.Objects (Get_Info (Decl).Slot), 3);
         when Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Signal_Declaration =>
            declare
               Sig : Iir_Value_Literal_Acc;
            begin
               Sig := Instance.Objects (Get_Info (Decl).Slot);
               Put (Disp_Node (Decl));
               Put (" = ");
               Disp_Signal (Sig, Get_Type (Decl));
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
         when others =>
            Error_Kind ("disp_declaration_object", Decl);
      end case;
   end Disp_Declaration_Object;

   procedure Disp_Declaration_Objects
     (Instance : Block_Instance_Acc; Decl_Chain : Iir)
   is
      El : Iir;
   begin
      El := Decl_Chain;
      while El /= Null_Iir loop
         Disp_Declaration_Object (Instance, El);
         El := Get_Chain (El);
      end loop;
   end Disp_Declaration_Objects;

   procedure Disp_Objects (Instance : Block_Instance_Acc)
   is
      Decl : constant Iir := Instance.Label;
   begin
      Disp_Instance_Name (Instance);
      New_Line;
      case Get_Kind (Decl) is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            Disp_Declaration_Objects
              (Instance, Get_Interface_Declaration_Chain (Decl));
            Disp_Declaration_Objects
              (Instance,
               Get_Declaration_Chain (Get_Subprogram_Body (Decl)));
         when Iir_Kind_Architecture_Body =>
            declare
               Entity : constant Iir_Entity_Declaration := Get_Entity (Decl);
            begin
               Disp_Declaration_Objects
                 (Instance, Get_Generic_Chain (Entity));
               Disp_Declaration_Objects
                 (Instance, Get_Port_Chain (Entity));
               Disp_Declaration_Objects
                 (Instance, Get_Declaration_Chain (Entity));
               Disp_Declaration_Objects
                 (Instance, Get_Declaration_Chain (Decl));
               --  FIXME: processes.
            end;
         when Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Error_Kind ("disp_objects", Decl);
      end case;
   end Disp_Objects;
   pragma Unreferenced (Disp_Objects);

   procedure Disp_Process_Stats
   is
      Proc : Iir;
      Stmt : Iir;
      Nbr_User_Sensitized_Processes : Natural := 0;
      Nbr_User_If_Sensitized_Processes : Natural := 0;
      Nbr_Conc_Sensitized_Processes : Natural := 0;
      Nbr_User_Non_Sensitized_Processes : Natural := 0;
      Nbr_Conc_Non_Sensitized_Processes : Natural := 0;
   begin
      for I in Processes_Table.First .. Processes_Table.Last loop
         Proc := Processes_Table.Table (I).Label;
         case Get_Kind (Proc) is
            when Iir_Kind_Sensitized_Process_Statement =>
               if Get_Process_Origin (Proc) = Null_Iir then
                  Stmt := Get_Sequential_Statement_Chain (Proc);
                  if Stmt /= Null_Iir
                    and then Get_Kind (Stmt) = Iir_Kind_If_Statement
                    and then Get_Chain (Stmt) = Null_Iir
                  then
                     Nbr_User_If_Sensitized_Processes :=
                       Nbr_User_If_Sensitized_Processes + 1;
                  else
                     Nbr_User_Sensitized_Processes :=
                       Nbr_User_Sensitized_Processes + 1;
                  end if;
               else
                  Nbr_Conc_Sensitized_Processes :=
                    Nbr_Conc_Sensitized_Processes + 1;
               end if;
            when Iir_Kind_Process_Statement =>
               if Get_Process_Origin (Proc) = Null_Iir then
                  Nbr_User_Non_Sensitized_Processes :=
                    Nbr_User_Non_Sensitized_Processes + 1;
               else
                  Nbr_Conc_Non_Sensitized_Processes :=
                    Nbr_Conc_Non_Sensitized_Processes + 1;
               end if;
            when others =>
               raise Internal_Error;
         end case;
      end loop;

      Put (Natural'Image (Nbr_User_If_Sensitized_Processes));
      Put_Line (" user sensitized processes with only a if stmt");
      Put (Natural'Image (Nbr_User_Sensitized_Processes));
      Put_Line (" user sensitized processes (others)");
      Put (Natural'Image (Nbr_User_Non_Sensitized_Processes));
      Put_Line (" user non sensitized processes");
      Put (Natural'Image (Nbr_Conc_Sensitized_Processes));
      Put_Line (" sensitized concurrent statements");
      Put (Natural'Image (Nbr_Conc_Non_Sensitized_Processes));
      Put_Line (" non sensitized concurrent statements");
      Put (Process_Index_Type'Image (Processes_Table.Last));
      Put_Line (" processes (total)");
   end Disp_Process_Stats;

   procedure Disp_Signals_Stats
   is
      type Counters_Type is array (Mode_Signal_Type) of Natural;
      Counters : Counters_Type := (others => 0);
      Nbr_User_Signals : Natural := 0;
      Nbr_Signal_Elements : Natural := 0;
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            Ent : Signal_Entry renames Signals_Table.Table (I);
         begin
            if Ent.Kind in Mode_Signal_User then
               Nbr_User_Signals := Nbr_User_Signals + 1;
               Nbr_Signal_Elements := Nbr_Signal_Elements +
                 Get_Nbr_Of_Scalars (Signals_Table.Table (I).Sig);
            end if;
            Counters (Ent.Kind) := Counters (Ent.Kind) + 1;
         end;
      end loop;
      Put (Integer'Image (Nbr_User_Signals));
      Put_Line (" declared user signals or ports");
      Put (Integer'Image (Nbr_Signal_Elements));
      Put_Line (" user signals sub-elements");
      Put (Integer'Image (Counters (Mode_Quiet)));
      Put_Line (" 'quiet implicit signals");
      Put (Integer'Image (Counters (Mode_Stable)));
      Put_Line (" 'stable implicit signals");
      Put (Integer'Image (Counters (Mode_Delayed)));
      Put_Line (" 'delayed implicit signals");
      Put (Integer'Image (Counters (Mode_Transaction)));
      Put_Line (" 'transaction implicit signals");
      Put (Integer'Image (Counters (Mode_Guard)));
      Put_Line (" guard signals");
   end Disp_Signals_Stats;

   procedure Disp_Design_Stats is
   begin
      Disp_Process_Stats;

      New_Line;

      Disp_Signals_Stats;

      New_Line;

      Put (Integer'Image (Connect_Table.Last));
      Put_Line (" connections");
   end Disp_Design_Stats;

   procedure Disp_Design_Non_Sensitized
   is
      Instance : Block_Instance_Acc;
      Proc : Iir;
   begin
      for I in Processes_Table.First .. Processes_Table.Last loop
         Instance := Processes_Table.Table (I);
         Proc := Processes_Table.Table (I).Label;
         if Get_Kind (Proc) = Iir_Kind_Process_Statement then
            Disp_Instance_Name (Instance);
            New_Line;
            Put_Line ("   at " & Disp_Location (Proc));
         end if;
      end loop;
   end Disp_Design_Non_Sensitized;

   procedure Disp_Design_Connections is
   begin
      for I in Connect_Table.First .. Connect_Table.Last loop
         declare
            Conn : Connect_Entry renames Connect_Table.Table (I);
         begin
            Disp_Iir_Location (Conn.Assoc);
            New_Line;
         end;
      end loop;
   end Disp_Design_Connections;

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
               when Iir_Kind_Component_Instantiation_Statement =>
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
                  Error_Kind ("walk_conc_chain", Stmt);
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
            Error_Kind ("Cb_Walk_Declarations", Unit);
      end case;
      return Walk_Continue;
   end Cb_Walk_Declarations;

   function Walk_Declarations (Cb : Walk_Cb) return Walk_Status is
   begin
      Walk_Declarations_Cb := Cb;
      return Walk_Units (Cb_Walk_Declarations'Access);
   end Walk_Declarations;

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

   procedure Disp_A_Frame (Instance: Block_Instance_Acc) is
   begin
      if Instance = Global_Instances then
         pragma Assert (Instance.Label = Null_Iir);
         Put_Line ("global instances");
         return;
      end if;

      Put (Disp_Node (Instance.Label));
      if Instance.Stmt /= Null_Iir then
         Put (" at ");
         Put (Files_Map.Image (Get_Location (Instance.Stmt)));
      end if;
      New_Line;
   end Disp_A_Frame;

   procedure Debug_Bt (Instance : Block_Instance_Acc)
   is
      Inst : Block_Instance_Acc;
   begin
      Inst := Instance;
      while Inst /= null loop
         Disp_A_Frame (Inst);
         Inst := Inst.Parent;
      end loop;
   end Debug_Bt;
   pragma Unreferenced (Debug_Bt);

   procedure Debug_Upblock (Instance : Block_Instance_Acc)
   is
      Inst : Block_Instance_Acc;
   begin
      Inst := Instance;
      while Inst /= null loop
         Disp_A_Frame (Inst);
         Inst := Inst.Up_Block;
      end loop;
   end Debug_Upblock;
   pragma Unreferenced (Debug_Upblock);

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

   --  Check there is a current process.
   procedure Check_Current_Process is
   begin
      if Current_Process = null then
         Put_Line ("no current process");
         raise Command_Error;
      end if;
   end Check_Current_Process;

   --  The status of the debugger.  This status can be modified by a command
   --  as a side effect to resume or quit the debugger.
   type Command_Status_Type is (Status_Default, Status_Quit);
   Command_Status : Command_Status_Type;

   procedure Help_Proc (Line : String);

   procedure Disp_Process_Loc (Proc : Process_State_Type) is
   begin
      Disp_Instance_Name (Proc.Top_Instance);
      Put (" (" & Files_Map.Image (Get_Location (Proc.Proc)) & ")");
      New_Line;
   end Disp_Process_Loc;

   --  Disp the list of processes (and its state)
   procedure Ps_Proc (Line : String) is
      pragma Unreferenced (Line);
      Process : Iir;
   begin
      if Processes_State = null then
         Put_Line ("no processes");
         return;
      end if;

      for I in Processes_State'Range loop
         Put (Process_Index_Type'Image (I) & ": ");
         Process := Processes_State (I).Proc;
         if Process /= Null_Iir then
            Disp_Process_Loc (Processes_State (I));
            Disp_A_Frame (Processes_State (I).Instance);
         else
            Put_Line ("not yet elaborated");
         end if;
      end loop;
   end Ps_Proc;

   procedure List_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Disp_Current_Lines;
   end List_Proc;

   procedure Up_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Check_Current_Process;
      if Dbg_Cur_Frame.Parent = null then
         Put_Line ("top of frames reached");
      else
         Set_Cur_Frame (Dbg_Cur_Frame.Parent);
      end if;
   end Up_Proc;

   procedure Down_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Inst : Block_Instance_Acc;
   begin
      Check_Current_Process;
      if Dbg_Cur_Frame = Dbg_Top_Frame then
         Put_Line ("bottom of frames reached");
      else
         Inst := Dbg_Top_Frame;
         while Inst.Parent /= Dbg_Cur_Frame loop
            Inst := Inst.Parent;
         end loop;
         Set_Cur_Frame (Inst);
      end if;
   end Down_Proc;

   procedure Set_Breakpoint (Stmt : Iir) is
   begin
      Put_Line ("set breakpoint at: " & Files_Map.Image (Get_Location (Stmt)));
      Breakpoints.Append (Breakpoint_Entry'(Stmt => Stmt));
      Flag_Need_Debug := True;
   end Set_Breakpoint;

   function Is_Within_Statement (Stmt : Iir; Cur : Iir) return Boolean
   is
      Parent : Iir;
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

   --  Next statement in the same frame, but handle compound statements as
   --  one statement.
   procedure Next_Stmt_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next_Stmt;
      Exec_Instance := Dbg_Top_Frame;
      Exec_Statement := Dbg_Top_Frame.Stmt;
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
   end Next_Stmt_Proc;

   --  Finish parent statement.
   procedure Finish_Stmt_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next_Stmt;
      Exec_Instance := Dbg_Top_Frame;
      Exec_Statement := Get_Parent (Dbg_Top_Frame.Stmt);
      Flag_Need_Debug := True;
      Command_Status := Status_Quit;
   end Finish_Stmt_Proc;

   procedure Next_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Exec_State := Exec_Next;
      Exec_Instance := Dbg_Top_Frame;
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

   procedure Where_Proc (Line : String) is
      pragma Unreferenced (Line);
      Frame : Block_Instance_Acc;
   begin
      Check_Current_Process;
      Frame := Dbg_Top_Frame;
      while Frame /= null loop
         if Frame = Dbg_Cur_Frame then
            Put ("* ");
         else
            Put ("  ");
         end if;
         Disp_A_Frame (Frame);
         Frame := Frame.Parent;
      end loop;
   end Where_Proc;

   procedure Info_Tree_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      if Top_Instance = null then
         Put_Line ("design not yet fully elaborated");
      else
         Disp_Instances_Tree;
      end if;
   end Info_Tree_Proc;

   procedure Info_Instances_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      procedure Disp_Instances (Inst : Block_Instance_Acc)
      is
         Child : Block_Instance_Acc;
      begin
         case Get_Kind (Inst.Label) is
            when Iir_Kind_Architecture_Body =>
               Disp_Instances_Tree_Name (Inst);
            when others =>
               null;
         end case;

         Child := Inst.Children;
         while Child /= null loop
            if Get_Kind (Child.Label) not in Iir_Kinds_Process_Statement then
               Disp_Instances (Child);
            end if;
            Child := Child.Brother;
         end loop;

      end Disp_Instances;
   begin
      if Top_Instance = null then
         Put_Line ("design not yet fully elaborated");
         return;
      end if;
      for I in Global_Instances.Objects'Range loop
         if Global_Instances.Objects (I) /= null then
            Put (Get_Instance_Local_Name
                   (Global_Instances.Objects (I).Instance));
            Put_Line (" [package]");
         end if;
      end loop;
      Disp_Instances (Top_Instance);
   end Info_Instances_Proc;

   procedure Info_Params_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Decl : Iir;
      Params : Iir;
   begin
      Check_Current_Process;
      if Dbg_Cur_Frame = null then
         Put_Line ("not in a subprogram");
         return;
      end if;
      Decl := Dbg_Cur_Frame.Label;
      if Decl = Null_Iir
        or else Get_Kind (Decl) not in Iir_Kinds_Subprogram_Declaration
      then
         Put_Line ("current frame is not a subprogram");
         return;
      end if;
      Params := Get_Interface_Declaration_Chain (Decl);
      Disp_Declaration_Objects (Dbg_Cur_Frame, Params);
   end Info_Params_Proc;

   procedure Info_Proc_Proc (Line : String) is
      pragma Unreferenced (Line);
   begin
      Check_Current_Process;
      Disp_Process_Loc (Current_Process.all);
   end Info_Proc_Proc;

   function Cb_Disp_Subprograms (El : Iir) return Walk_Status is
   begin
      case Get_Kind (El) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            Put_Line (Name_Table.Image (Get_Identifier (El)));
         when others =>
            null;
      end case;
      return Walk_Continue;
   end Cb_Disp_Subprograms;

   procedure Info_Subprograms_Proc (Line : String) is
      pragma Unreferenced (Line);
      Status : Walk_Status;
   begin
      Status := Walk_Declarations (Cb_Disp_Subprograms'Access);
      pragma Assert (Status = Walk_Continue);
   end Info_Subprograms_Proc;

   function Cb_Disp_Units (El : Iir) return Walk_Status is
   begin
      case Get_Kind (El) is
         when Iir_Kind_Package_Declaration =>
            Put ("package ");
            Put_Line (Name_Table.Image (Get_Identifier (El)));
         when Iir_Kind_Entity_Declaration =>
            Put ("entity ");
            Put_Line (Name_Table.Image (Get_Identifier (El)));
         when Iir_Kind_Architecture_Body =>
            Put ("architecture ");
            Put (Name_Table.Image (Get_Identifier (El)));
            Put (" of ");
            Put_Line (Name_Table.Image (Get_Identifier
                                          (Get_Entity_Name (El))));
         when Iir_Kind_Configuration_Declaration =>
            Put ("configuration ");
            Put_Line (Name_Table.Image (Get_Identifier (El)));
         when Iir_Kind_Package_Body =>
            null;
         when others =>
            Error_Kind ("cb_disp_units", El);
      end case;
      return Walk_Continue;
   end Cb_Disp_Units;

   procedure Info_Units_Proc (Line : String) is
      pragma Unreferenced (Line);
      Status : Walk_Status;
   begin
      Status := Walk_Units (Cb_Disp_Units'Access);
      pragma Assert (Status = Walk_Continue);
   end Info_Units_Proc;

   function Cb_Disp_File (El : Iir) return Walk_Status is
   begin
      Put_Line (Name_Table.Image (Get_Design_File_Filename (El)));
      return Walk_Continue;
   end Cb_Disp_File;

   procedure Info_PSL_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      if PSL_Table.Last < PSL_Table.First then
         Put_Line ("no PSL directive");
         return;
      end if;

      for I in PSL_Table.First .. PSL_Table.Last loop
         declare
            E : PSL_Entry renames PSL_Table.Table (I);
         begin
            Disp_Instance_Name (E.Instance);
            Put ('.');
            Put (Name_Table.Image (Get_Identifier (E.Stmt)));
            New_Line;
            Vhdl.Prints.Disp_PSL_NFA (Get_PSL_NFA (E.Stmt));
            Put ("    01234567890123456789012345678901234567890123456789");
            for I in E.States'Range loop
               if I mod 50 = 0 then
                  New_Line;
                  Put (Int32'Image (I / 10));
                  Put (": ");
               end if;
               if E.States (I) then
                  Put ('*');
               else
                  Put ('.');
               end if;
            end loop;
            New_Line;
         end;
      end loop;
   end Info_PSL_Proc;

   procedure Info_Stats_Proc (Line : String) is
      P : Natural := Line'First;
      E : Natural;
   begin
      P := Skip_Blanks (Line (P .. Line'Last));
      if P > Line'Last then
         --  No parameters.
         Disp_Design_Stats;
         return;
      end if;

      E := Get_Word (Line (P .. Line'Last));
      if Line (P .. E) = "global" then
         Disp_Design_Stats;
      elsif Line (P .. E) = "non-sensitized" then
         Disp_Design_Non_Sensitized;
         null;
      elsif Line (P .. E) = "connections" then
         Disp_Design_Connections;
         --  TODO: nbr of conversions
      else
         Put_Line ("options are: global, non-sensitized, connections");
         --  TODO: signals: nbr of scalars, nbr of non-user...
      end if;
   end Info_Stats_Proc;

   procedure Info_Files_Proc (Line : String)
   is
      pragma Unreferenced (Line);
      Status : Walk_Status;
   begin
      Status := Walk_Files (Cb_Disp_File'Access);
      pragma Assert (Status = Walk_Continue);
   end Info_Files_Proc;

   procedure Info_Libraries_Proc (Line : String) is
      pragma Unreferenced (Line);
      Lib : Iir_Library_Declaration := Libraries.Get_Libraries_Chain;
   begin
      while Lib /= Null_Iir loop
         Put_Line (Name_Table.Image (Get_Identifier (Lib)));
         Lib := Get_Chain (Lib);
      end loop;
   end Info_Libraries_Proc;

   procedure Disp_Declared_Signals_Chain
     (Chain : Iir; Instance : Block_Instance_Acc)
   is
      pragma Unreferenced (Instance);
      Decl : Iir;
   begin
      Decl := Chain;
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Signal_Declaration =>
               Put_Line (" " & Name_Table.Image (Get_Identifier (Decl)));
            when others =>
               null;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Disp_Declared_Signals_Chain;

   procedure Disp_Declared_Signals (Decl : Iir; Instance : Block_Instance_Acc)
   is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            Disp_Declared_Signals (Get_Parent (Decl), Instance);
         when Iir_Kind_Architecture_Body =>
            Disp_Declared_Signals (Get_Entity (Decl), Instance);
         when Iir_Kind_Entity_Declaration =>
            null;
         when others =>
            Error_Kind ("disp_declared_signals", Decl);
      end case;

      case Get_Kind (Decl) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            --  No signal declaration in a process (FIXME: implicit signals)
            null;
         when Iir_Kind_Architecture_Body =>
            Put_Line ("Signals of architecture "
                        & Name_Table.Image (Get_Identifier (Decl)) & ':');
            Disp_Declared_Signals_Chain
              (Get_Declaration_Chain (Decl), Instance);
         when Iir_Kind_Entity_Declaration =>
            Put_Line ("Ports of entity "
                        & Name_Table.Image (Get_Identifier (Decl)) & ':');
            Disp_Declared_Signals_Chain
              (Get_Port_Chain (Decl), Instance);
         when others =>
            Error_Kind ("disp_declared_signals (2)", Decl);
      end case;
   end Disp_Declared_Signals;

   procedure Info_Signals_Proc (Line : String)
   is
      Verbose : Boolean;
      P : Natural;
      E : Natural;
   begin
      Verbose := False;

      P := Skip_Blanks (Line);
      loop
         E := Get_Word (Line (P .. Line'Last));
         exit when P > Line'Last;
         if Line (P .. E) = "-v" then
            Verbose := True;
         elsif Line (P .. E) = "-l" then
            --  Local signals
            Check_Current_Process;
            Disp_Declared_Signals
              (Current_Process.Proc, Current_Process.Top_Instance);
            return;
         elsif Line (P .. E) = "-t" then
            Disp_Signals_Value;
            return;
         elsif Line (P .. E) = "-T" then
            Grt.Disp_Signals.Disp_Signals_Table;
            return;
         else
            Put_Line ("options: -v(erbose) -l(ocal) -t(ree) -T(able)");
            return;
         end if;
         P := E + 1;
      end loop;

      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            S : Signal_Entry renames Signals_Table.Table (I);
         begin
            Disp_Instance_Name (S.Instance, False);
            Put ('.');
            if S.Kind in Grt.Types.Mode_Signal_User then
               Put (Name_Table.Image (Get_Identifier (S.Decl)));
               New_Line;
               Put (" sig: ");
               Disp_Value (S.Sig);
               Put (" val: ");
               Disp_Value (S.Val);
               if Verbose then
                  --  Dummy to keep compiler happy.
                  Verbose := False;
               end if;
            else
               Disp_Declaration_Object (S.Instance, S.Decl);
            end if;
         end;
      end loop;
   end Info_Signals_Proc;

   type Handle_Scope_Type is access procedure (N : Iir);

   procedure Foreach_Scopes (N : Iir; Handler : Handle_Scope_Type) is
   begin
      case Get_Kind (N) is
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            Foreach_Scopes (Get_Parent (N), Handler);
            Handler.all (N);
         when Iir_Kind_Architecture_Body =>
            Foreach_Scopes (Get_Entity (N), Handler);
            Handler.all (N);

         when Iir_Kind_Entity_Declaration =>
            --  Top of scopes.
            Handler.all (N);

         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            Foreach_Scopes (Get_Parent (N), Handler);
            Handler.all (N);
         when Iir_Kind_Package_Body =>
            Handler.all (N);

         when Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Case_Statement =>
            Foreach_Scopes (Get_Parent (N), Handler);

         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body =>
            Foreach_Scopes (Get_Parent (N), Handler);
            Handler.all (N);

         when others =>
            Error_Kind ("foreach_scopes", N);
      end case;
   end Foreach_Scopes;

   procedure Add_Decls_For (N : Iir)
   is
      use Vhdl.Sem_Scopes;
   begin
      case Get_Kind (N) is
         when Iir_Kind_Entity_Declaration =>
            declare
               Unit : constant Iir := Get_Design_Unit (N);
            begin
               Add_Context_Clauses (Unit);
               --  Add_Name (Unit, Get_Identifier (N), False);
               Add_Entity_Declarations (N);
            end;
         when Iir_Kind_Architecture_Body =>
            Open_Declarative_Region;
            Add_Context_Clauses (Get_Design_Unit (N));
            Add_Declarations (Get_Declaration_Chain (N), False);
            Add_Declarations_Of_Concurrent_Statement (N);
         when Iir_Kind_Package_Body =>
            declare
               Package_Decl : constant Iir := Get_Package (N);
               Package_Unit : constant Iir := Get_Design_Unit (Package_Decl);
            begin
               Add_Name (Package_Unit);
               Add_Context_Clauses (Package_Unit);
               Open_Declarative_Region;
               Add_Declarations (Get_Declaration_Chain (Package_Decl), False);
               Add_Declarations (Get_Declaration_Chain (N), False);
            end;
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            declare
               Spec : constant Iir := Get_Subprogram_Specification (N);
            begin
               Open_Declarative_Region;
               Add_Declarations
                 (Get_Interface_Declaration_Chain (Spec), False);
               Add_Declarations
                 (Get_Declaration_Chain (N), False);
            end;
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement =>
            Open_Declarative_Region;
            Add_Declarations (Get_Declaration_Chain (N), False);
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_For_Generate_Statement =>
            Open_Declarative_Region;
            Add_Name (Get_Parameter_Specification (N));
         when Iir_Kind_Block_Statement =>
            declare
               Header : constant Iir := Get_Block_Header (N);
            begin
               Open_Declarative_Region;
               if Header /= Null_Iir then
                  Add_Declarations (Get_Generic_Chain (Header), False);
                  Add_Declarations (Get_Port_Chain (Header), False);
               end if;
               Add_Declarations (Get_Declaration_Chain (N), False);
               Add_Declarations_Of_Concurrent_Statement (N);
            end;
         when Iir_Kind_Generate_Statement_Body =>
            Open_Declarative_Region;
            Add_Declarations (Get_Declaration_Chain (N), False);
            Add_Declarations_Of_Concurrent_Statement (N);
         when others =>
            Error_Kind ("enter_scope(2)", N);
      end case;
   end Add_Decls_For;

   procedure Enter_Scope (Node : Iir)
   is
      use Vhdl.Sem_Scopes;
   begin
      Push_Interpretations;
      Open_Declarative_Region;

      --  Add STD
      Add_Name (Libraries.Std_Library, Std_Names.Name_Std, False);
      Use_All_Names (Vhdl.Std_Package.Standard_Package);

      Foreach_Scopes (Node, Add_Decls_For'Access);
   end Enter_Scope;

   procedure Del_Decls_For (N : Iir)
   is
      use Vhdl.Sem_Scopes;
   begin
      case Get_Kind (N) is
         when Iir_Kind_Entity_Declaration =>
            null;
         when Iir_Kind_Architecture_Body =>
            Close_Declarative_Region;
         when Iir_Kind_Process_Statement
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Package_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_If_Generate_Statement
           | Iir_Kind_For_Generate_Statement
           | Iir_Kind_Generate_Statement_Body =>
            Close_Declarative_Region;
         when others =>
            Error_Kind ("Decl_Decls_For", N);
      end case;
   end Del_Decls_For;

   procedure Leave_Scope (Node : Iir)
   is
      use Vhdl.Sem_Scopes;
   begin
      Foreach_Scopes (Node, Del_Decls_For'Access);

      Close_Declarative_Region;
      Pop_Interpretations;
   end Leave_Scope;

   Buffer_Index : Natural := 1;

   procedure Print_Proc (Line : String)
   is
      use Vhdl.Tokens;
      Index_Str : String := Natural'Image (Buffer_Index);
      File : Source_File_Entry;
      Expr : Iir;
      Res : Iir_Value_Literal_Acc;
      P : Natural;
      Opt_Value : Boolean := False;
      Opt_Name : Boolean := False;
      Marker : Mark_Type;
   begin
      --  Decode options: /v
      P := Line'First;
      loop
         P := Skip_Blanks (Line (P .. Line'Last));
         if P + 2 < Line'Last and then Line (P .. P + 1) = "/v" then
            Opt_Value := True;
            P := P + 2;
         elsif P + 2 < Line'Last and then Line (P .. P + 1) = "/n" then
            Opt_Name := True;
            P := P + 2;
         else
            exit;
         end if;
      end loop;

      Buffer_Index := Buffer_Index + 1;
      Index_Str (Index_Str'First) := '*';
      File := Files_Map.Create_Source_File_From_String
        (Name_Table.Get_Identifier ("*debug" & Index_Str & '*'),
         Line (P .. Line'Last));
      Vhdl.Scanner.Set_File (File);
      Vhdl.Scanner.Scan;
      Expr := Vhdl.Parse.Parse_Expression;
      if Vhdl.Scanner.Current_Token /= Tok_Eof then
         Put_Line ("garbage at end of expression ignored");
      end if;
      Vhdl.Scanner.Close_File;
      if Nbr_Errors /= 0 then
         Put_Line ("error while parsing expression, evaluation aborted");
         Nbr_Errors := 0;
         return;
      end if;

      Enter_Scope (Dbg_Cur_Frame.Stmt);
      Expr := Vhdl.Sem_Expr.Sem_Expression_Universal (Expr);
      Leave_Scope (Dbg_Cur_Frame.Stmt);

      if Expr = Null_Iir
        or else Nbr_Errors /= 0
      then
         Put_Line ("error while analyzing expression, evaluation aborted");
         Nbr_Errors := 0;
         return;
      end if;

      Vhdl.Prints.Disp_Expression (Expr);
      New_Line;

      Annotate_Expand_Table;
      Vhdl.Canon.Canon_Expression (Expr);

      Mark (Marker, Expr_Pool);

      if Opt_Name then
         case Get_Kind (Expr) is
            when Iir_Kind_Simple_Name =>
               null;
            when others =>
               Put_Line ("expression is not a name");
               Opt_Name := False;
         end case;
      end if;
      if Opt_Name then
         Res := Execute_Name (Dbg_Cur_Frame, Expr, True);
      else
         Res := Execute_Expression (Dbg_Cur_Frame, Expr);
      end if;
      if Opt_Value then
         Disp_Value (Res);
      else
         Disp_Iir_Value (Res, Get_Type (Expr));
      end if;
      New_Line;

      --  Free value
      Release (Marker, Expr_Pool);
   end Print_Proc;

   procedure Quit_Proc (Line : String) is
      pragma Unreferenced (Line);
   begin
      Command_Status := Status_Quit;
      raise Debugger_Quit;
   end Quit_Proc;

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

   procedure Run_Proc (Line : String)
   is
      Delta_Time : Std_Time;
      P : Positive;
   begin
      P := Skip_Blanks (Line);
      if P <= Line'Last then
         Delta_Time := Grt.Options.Parse_Time (Line (P .. Line'Last));
         if Delta_Time = -1 then
            return;
         end if;
         Break_Time := Grt.Processes.Next_Time + Delta_Time;
      end if;

      Prepare_Continue;
   end Run_Proc;

   procedure Cont_Proc (Line : String) is
      pragma Unreferenced (Line);
   begin
      Prepare_Continue;
   end Cont_Proc;

   Menu_Info_Instances : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("instances"),
      Next => null,
      Proc => Info_Instances_Proc'Access);

   Menu_Info_Psl : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("psl"),
      Next => Menu_Info_Instances'Access,
      Proc => Info_PSL_Proc'Access);

   Menu_Info_Stats : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("stats"),
      Next => Menu_Info_Psl'Access,
      Proc => Info_Stats_Proc'Access);

   Menu_Info_Tree : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("tree"),
      Next => Menu_Info_Stats'Access,
      Proc => Info_Tree_Proc'Access);

   Menu_Info_Params : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("param*eters"),
      Next => Menu_Info_Tree'Access,
      Proc => Info_Params_Proc'Access);

   Menu_Info_Subprograms : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("subp*rograms"),
      Next => Menu_Info_Params'Access,
      Proc => Info_Subprograms_Proc'Access);

   Menu_Info_Units : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("units"),
      Next => Menu_Info_Subprograms'Access,
      Proc => Info_Units_Proc'Access);

   Menu_Info_Files : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("files"),
      Next => Menu_Info_Units'Access,
      Proc => Info_Files_Proc'Access);

   Menu_Info_Libraries : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("lib*raries"),
      Next => Menu_Info_Files'Access,
      Proc => Info_Libraries_Proc'Access);

   Menu_Info_Signals : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("sig*nals"),
      Next => Menu_Info_Libraries'Access,
      Proc => Info_Signals_Proc'Access);

   Menu_Info_Proc : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("proc*esses"),
      Next => Menu_Info_Signals'Access,
      Proc => Info_Proc_Proc'Access);

   Menu_List : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("l*list"),
      Next => null,
      Proc => List_Proc'Access);

   Menu_Down : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("down"),
      Next => Menu_List'Access,
      Proc => Down_Proc'Access);

   Menu_Up : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("up"),
      Next => Menu_Down'Access,
      Proc => Up_Proc'Access);

   Menu_Nstmt : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("ns*tmt"),
      Next => Menu_Up'Access,
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

   Menu_Where : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("where"),
      Next => Menu_Break'Access,
      Proc => Where_Proc'Access);

   Menu_Ps : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("ps"),
      Next => Menu_Where'Access,
      Proc => Ps_Proc'Access);

   Menu_Info : aliased Menu_Entry :=
     (Kind => Menu_Submenu,
      Name => new String'("i*nfo"),
      Next => Menu_Ps'Access,
      First | Last => Menu_Info_Proc'Access);

   Menu_Print : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("pr*int"),
      Next => Menu_Info'Access,
      Proc => Print_Proc'Access);

   Menu_Cont : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("c*ont"),
      Next => Menu_Print'Access,
      Proc => Cont_Proc'Access);

   Menu_Run : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("r*un"),
      Next => Menu_Cont'Access,
      Proc => Run_Proc'Access);

   Menu_Quit : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("q*uit"),
      Next => Menu_Run'Access,
      Proc => Quit_Proc'Access);

   Menu_Help1 : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("help"),
      Next => Menu_Quit'Access,
      Proc => Help_Proc'Access);

   Menu_Help2 : aliased Menu_Entry :=
     (Kind => Menu_Command,
      Name => new String'("?"),
      Next => Menu_Help1'Access,
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

   function Breakpoint_Hit return Natural
   is
      Stmt : constant Iir := Current_Process.Instance.Stmt;
   begin
      for I in Breakpoints.First .. Breakpoints.Last loop
         if Stmt = Breakpoints.Table (I).Stmt then
            return I;
         end if;
      end loop;
      return 0;
   end Breakpoint_Hit;

   Prompt_Debug : constant String := "debug> " & ASCII.NUL;
   Prompt_Error : constant String := "error> " & ASCII.NUL;
   Prompt_Init  : constant String := "init> " & ASCII.NUL;
   Prompt_Elab  : constant String := "elab> " & ASCII.NUL;

   procedure Debug (Reason: Debug_Reason)
   is
      use Grt.Readline;
      Raw_Line : Char_Ptr;
      Prompt : System.Address;
   begin
      --  Unless interractive, do not use the debugger.
      case Reason is
         when Reason_Internal_Debug =>
            null;
         when Reason_Assert
           | Reason_Error =>
            if not Flag_Debugger then
               return;
            end if;
         when Reason_Start
           | Reason_Elab =>
            if not Flag_Interractive then
               return;
            end if;
         when Reason_Break
           | Reason_Time =>
            null;
      end case;

      Prompt := Prompt_Debug'Address;

      case Reason is
         when Reason_Start =>
            Set_Top_Frame (null);
            Prompt := Prompt_Init'Address;
         when Reason_Elab =>
            Set_Top_Frame (null);
            Prompt := Prompt_Elab'Address;
         when Reason_Internal_Debug =>
            if Current_Process = null then
               Set_Top_Frame (null);
            else
               Set_Top_Frame (Current_Process.Instance);
            end if;
         when Reason_Time =>
            Break_Time := Grt.Types.Std_Time'Last;
            Exec_State := Exec_Run;
         when Reason_Break =>
            case Exec_State is
               when Exec_Run =>
                  if Breakpoint_Hit /= 0 then
                     Put_Line ("breakpoint hit");
                  else
                     return;
                  end if;
               when Exec_Single_Step =>
                  null;
               when Exec_Next =>
                  if Current_Process.Instance /= Exec_Instance then
                     return;
                  end if;
               when Exec_Next_Stmt =>
                  if Current_Process.Instance /= Exec_Instance
                    or else Is_Within_Statement (Exec_Statement,
                                                 Current_Process.Instance.Stmt)
                  then
                     return;
                  end if;
            end case;
            --  Default state.
            Exec_State := Exec_Run;
            Set_Top_Frame (Current_Process.Instance);
            declare
               Stmt : constant Iir := Dbg_Cur_Frame.Stmt;
            begin
               Put ("stopped at: ");
               Disp_Iir_Location (Stmt);
               New_Line;
               Disp_Source_Line (Get_Location (Stmt));
            end;
         when Reason_Assert =>
            Set_Top_Frame (Current_Process.Instance);
            Prompt := Prompt_Error'Address;
            Put_Line ("assertion failure, enterring in debugger");
         when Reason_Error =>
            Set_Top_Frame (Current_Process.Instance);
            Prompt := Prompt_Error'Address;
            Put_Line ("error occurred, enterring in debugger");
      end case;

      if Dbg_Cur_Frame /= null then
         Set_List_Current (Get_Location (Dbg_Cur_Frame.Stmt));
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

   procedure Debug_Error is
   begin
      Debug (Reason_Error);
   end Debug_Error;
end Simul.Debugger;
