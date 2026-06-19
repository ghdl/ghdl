--  Native dataflow (.flow) JSON exporter for simulation.
--  Copyright (C) 2026 Tristan Gingold
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

with Ada.Text_IO; use Ada.Text_IO;

with Types; use Types;
with Name_Table;
with Files_Map;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Utils;
with Vhdl.Canon;

with Libraries;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;

with Flags;
with Version;

package body Simul.Flow is

   --  ------------------------------------------------------------------
   --  Small helpers.
   --  ------------------------------------------------------------------

   --  Integer image without the leading space of 'Image.
   function Img (N : Integer) return String is
      S : constant String := Integer'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

   --  Int64 image without the leading space of 'Image.
   function Img64 (N : Int64) return String is
      S : constant String := Int64'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img64;

   function Hex1 (V : Natural) return Character is
      H : constant String := "0123456789abcdef";
   begin
      return H (H'First + V);
   end Hex1;

   --  Emit S as a quoted, escaped JSON string.
   procedure Put_Str (F : File_Type; S : String) is
   begin
      Put (F, '"');
      for C of S loop
         case C is
            when '"' =>
               Put (F, "\""");
            when '\' =>
               Put (F, "\\");
            when ASCII.LF =>
               Put (F, "\n");
            when ASCII.CR =>
               Put (F, "\r");
            when ASCII.HT =>
               Put (F, "\t");
            when others =>
               if Character'Pos (C) < 16#20# then
                  Put (F, "\u00");
                  Put (F, Hex1 (Character'Pos (C) / 16));
                  Put (F, Hex1 (Character'Pos (C) mod 16));
               else
                  Put (F, C);
               end if;
         end case;
      end loop;
      Put (F, '"');
   end Put_Str;

   --  ------------------------------------------------------------------
   --  File registry (for the top-level "files" array).
   --  ------------------------------------------------------------------

   Max_Files : constant := 8192;
   Files_Seen : array (1 .. Max_Files) of Name_Id;
   N_Files : Natural := 0;

   procedure Note_File (Id : Name_Id) is
   begin
      if Id = Null_Identifier then
         return;
      end if;
      for K in 1 .. N_Files loop
         if Files_Seen (K) = Id then
            return;
         end if;
      end loop;
      if N_Files < Max_Files then
         N_Files := N_Files + 1;
         Files_Seen (N_Files) := Id;
      end if;
   end Note_File;

   --  Return the file name of node N (and register it), or "".
   function Node_File (N : Iir) return String is
      Loc : Location_Type;
      Fid : Name_Id;
      Line : Positive;
      Col : Natural;
   begin
      if N = Null_Iir then
         return "";
      end if;
      Loc := Get_Location (N);
      if Loc = No_Location then
         return "";
      end if;
      Files_Map.Location_To_Position (Loc, Fid, Line, Col);
      Note_File (Fid);
      return Name_Table.Image (Fid);
   end Node_File;

   --  Emit a {"off","line","col"} position object for node N.
   procedure Put_Pos (F : File_Type; N : Iir) is
      Loc : Location_Type;
      Fid : Name_Id;
      Line : Positive;
      Col : Natural;
   begin
      if N = Null_Iir then
         Put (F, "{""off"": 0, ""line"": 0, ""col"": 0}");
         return;
      end if;
      Loc := Get_Location (N);
      if Loc = No_Location then
         Put (F, "{""off"": 0, ""line"": 0, ""col"": 0}");
         return;
      end if;
      Files_Map.Location_To_Position (Loc, Fid, Line, Col);
      Note_File (Fid);
      Put (F, "{""off"": 0, ""line"": " & Img (Line)
           & ", ""col"": " & Img (Col) & "}");
   end Put_Pos;

   --  ------------------------------------------------------------------
   --  Name / type resolution.
   --  ------------------------------------------------------------------

   --  Reduce a name (possibly indexed/sliced/selected) to its base
   --  declaration node.
   function Resolve_Obj (N : Iir) return Iir is
      B : Iir := N;
   begin
      if B = Null_Iir then
         return Null_Iir;
      end if;
      loop
         case Get_Kind (B) is
            when Iir_Kind_Indexed_Name
               | Iir_Kind_Slice_Name
               | Iir_Kind_Selected_Element
               | Iir_Kind_Parenthesis_Name =>
               B := Get_Prefix (B);
            when Iir_Kinds_Denoting_Name =>
               declare
                  Ne : constant Iir := Get_Named_Entity (B);
               begin
                  exit when Ne = Null_Iir;
                  B := Ne;
               end;
            when others =>
               exit;
         end case;
      end loop;
      return B;
   end Resolve_Obj;

   --  Identifier image of a declaration node, or "" if it has none.
   function Decl_Ident (D : Iir) return String is
   begin
      if D = Null_Iir then
         return "";
      end if;
      case Get_Kind (D) is
         when Iir_Kind_Signal_Declaration
            | Iir_Kind_Interface_Signal_Declaration
            | Iir_Kind_Guard_Signal_Declaration
            | Iir_Kind_Constant_Declaration
            | Iir_Kind_Interface_Constant_Declaration
            | Iir_Kind_Variable_Declaration
            | Iir_Kind_Interface_Variable_Declaration
            | Iir_Kind_File_Declaration
            | Iir_Kind_Iterator_Declaration
            | Iir_Kind_Object_Alias_Declaration
            | Iir_Kind_Type_Declaration
            | Iir_Kind_Subtype_Declaration
            | Iir_Kind_Enumeration_Literal
            | Iir_Kind_Unit_Declaration
            | Iir_Kind_Entity_Declaration
            | Iir_Kind_Architecture_Body
            | Iir_Kind_Component_Declaration
            | Iir_Kind_Configuration_Declaration
            | Iir_Kind_Package_Declaration =>
            return Name_Table.Image (Get_Identifier (D));
         when others =>
            return "";
      end case;
   end Decl_Ident;

   --  Underlying object name of an expression/name.
   function Obj_Name (N : Iir) return String is
   begin
      return Decl_Ident (Resolve_Obj (N));
   end Obj_Name;

   --  Best-effort printable type name of a declaration's subtype.
   --  Image of a static integer-literal expression, or "".
   function Int_Expr_Image (E : Node) return String is
   begin
      if E /= Null_Node
        and then Get_Kind (E) = Iir_Kind_Integer_Literal
      then
         return Img64 (Get_Value (E));
      end if;
      return "";
   end Int_Expr_Image;

   --  "L downto R" / "L to R" from a range expression, or "".
   function Range_Image (Rng : Node) return String is
   begin
      if Rng = Null_Node
        or else Get_Kind (Rng) /= Iir_Kind_Range_Expression
      then
         return "";
      end if;
      declare
         L : constant String := Int_Expr_Image (Get_Left_Limit (Rng));
         R : constant String := Int_Expr_Image (Get_Right_Limit (Rng));
      begin
         if L = "" or else R = "" then
            return "";
         end if;
         if Get_Direction (Rng) = Dir_Downto then
            return L & " downto " & R;
         else
            return L & " to " & R;
         end if;
      end;
   end Range_Image;

   --  The range node of an index-constraint element (a range expression
   --  or an index subtype carrying a range constraint).
   function Element_Range (E : Node) return Node is
   begin
      if E = Null_Node then
         return Null_Node;
      end if;
      if Get_Kind (E) = Iir_Kind_Range_Expression then
         return E;
      end if;
      begin
         return Get_Range_Constraint (E);
      exception
         when others =>
            return Null_Node;
      end;
   end Element_Range;

   --  "(3 downto 0)" for a single-index constrained array subtype, else
   --  "" (multi-dimensional or non-static constraints are skipped).
   function Index_Constraint_Image (Ind : Node) return String is
      Lst : Iir_Flist;
   begin
      begin
         Lst := Get_Index_Constraint_List (Ind);
      exception
         when others =>
            return "";
      end;
      if Lst = Null_Iir_Flist or else Flist_Last (Lst) /= 0 then
         return "";
      end if;
      declare
         R : constant String :=
           Range_Image (Element_Range (Get_Nth_Element (Lst, 0)));
      begin
         if R = "" then
            return "";
         end if;
         return "(" & R & ")";
      end;
   end Index_Constraint_Image;

   function Type_Image (Decl : Iir) return String is
      Ind : constant Iir := Get_Subtype_Indication (Decl);
      T, D, B : Iir;
   begin
      --  Prefer the written subtype mark (e.g. "std_logic_vector")
      --  over the resolved base type (e.g. "std_ulogic_vector").
      if Ind /= Null_Iir then
         if Get_Kind (Ind) in Iir_Kinds_Denoting_Name then
            declare
               Nm : constant String := Decl_Ident (Resolve_Obj (Ind));
            begin
               if Nm /= "" then
                  return Nm;
               end if;
            end;
         else
            begin
               declare
                  TM : constant Iir := Get_Subtype_Type_Mark (Ind);
               begin
                  if TM /= Null_Iir then
                     declare
                        Nm : constant String :=
                          Decl_Ident (Resolve_Obj (TM));
                     begin
                        if Nm /= "" then
                           return Nm & Index_Constraint_Image (Ind);
                        end if;
                     end;
                  end if;
               end;
            exception
               when others =>
                  null;
            end;
         end if;
      end if;
      T := Get_Type (Decl);
      if T = Null_Iir then
         return "";
      end if;
      D := Get_Type_Declarator (T);
      if D /= Null_Iir then
         return Name_Table.Image (Get_Identifier (D));
      end if;
      B := Vhdl.Utils.Get_Base_Type (T);
      if B /= Null_Iir then
         D := Get_Type_Declarator (B);
         if D /= Null_Iir then
            return Name_Table.Image (Get_Identifier (D));
         end if;
      end if;
      return "";
   end Type_Image;

   --  ------------------------------------------------------------------
   --  Name-list emission (reads / drives / sensitivity).
   --  ------------------------------------------------------------------

   --  Emit a JSON array of object names from an Iir_List, deduplicated
   --  by resolved declaration.
   procedure Put_Name_List (F : File_Type; List : Iir_List) is
      Seen : array (1 .. 8192) of Iir := (others => Null_Iir);
      N_Seen : Natural := 0;
      First : Boolean := True;
      It : List_Iterator;
   begin
      Put (F, "[");
      if List /= Null_Iir_List and then List /= Iir_List_All then
         It := List_Iterate (List);
         while Is_Valid (It) loop
            declare
               El : constant Iir := Get_Element (It);
               D : constant Iir := Resolve_Obj (El);
               Dup : Boolean := False;
            begin
               if D /= Null_Iir then
                  for K in 1 .. N_Seen loop
                     if Seen (K) = D then
                        Dup := True;
                        exit;
                     end if;
                  end loop;
                  if not Dup then
                     if N_Seen < Seen'Last then
                        N_Seen := N_Seen + 1;
                        Seen (N_Seen) := D;
                     end if;
                     declare
                        Nm : constant String := Decl_Ident (D);
                     begin
                        if Nm /= "" then
                           if not First then
                              Put (F, ", ");
                           end if;
                           First := False;
                           Put_Str (F, Nm);
                        end if;
                     end;
                  end if;
               end if;
            end;
            Next (It);
         end loop;
      end if;
      Put (F, "]");
   end Put_Name_List;

   --  ------------------------------------------------------------------
   --  Driver (write) set: walk a sequential statement chain collecting
   --  signal-assignment targets into LIST.
   --  ------------------------------------------------------------------

   --  Read (sensitivity) set: walk a sequential statement chain
   --  collecting signals read into LIST.  Unlike
   --  Canon_Extract_Sensitivity_Statement (which rejects wait
   --  statements, as it targets process(all)), this tolerates the full
   --  sequential grammar by handling control-flow and wait itself and
   --  delegating only leaf statements to canon.
   procedure Collect_Reads (Chain : Iir; List : Iir_List) is
      Stmt : Iir := Chain;
   begin
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Wait_Statement =>
               if Get_Condition_Clause (Stmt) /= Null_Iir then
                  Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                    (Get_Condition_Clause (Stmt), List);
               end if;
               if Get_Timeout_Clause (Stmt) /= Null_Iir then
                  Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                    (Get_Timeout_Clause (Stmt), List);
               end if;
               declare
                  SL : constant Iir_List := Get_Sensitivity_List (Stmt);
                  It : List_Iterator;
               begin
                  if SL /= Null_Iir_List and then SL /= Iir_List_All then
                     It := List_Iterate (SL);
                     while Is_Valid (It) loop
                        Append_Element (List, Get_Element (It));
                        Next (It);
                     end loop;
                  end if;
               end;
            when Iir_Kind_If_Statement =>
               declare
                  Cl : Iir := Stmt;
               begin
                  while Cl /= Null_Iir loop
                     if Get_Condition (Cl) /= Null_Iir then
                        Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                          (Get_Condition (Cl), List);
                     end if;
                     Collect_Reads
                       (Get_Sequential_Statement_Chain (Cl), List);
                     Cl := Get_Else_Clause (Cl);
                  end loop;
               end;
            when Iir_Kind_Case_Statement =>
               if Get_Expression (Stmt) /= Null_Iir then
                  Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                    (Get_Expression (Stmt), List);
               end if;
               declare
                  Alt : Iir := Get_Case_Statement_Alternative_Chain (Stmt);
               begin
                  while Alt /= Null_Iir loop
                     Collect_Reads (Get_Associated_Chain (Alt), List);
                     Alt := Get_Chain (Alt);
                  end loop;
               end;
            when Iir_Kind_While_Loop_Statement =>
               if Get_Condition (Stmt) /= Null_Iir then
                  Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                    (Get_Condition (Stmt), List);
               end if;
               Collect_Reads
                 (Get_Sequential_Statement_Chain (Stmt), List);
            when Iir_Kind_For_Loop_Statement =>
               Collect_Reads
                 (Get_Sequential_Statement_Chain (Stmt), List);
            when others =>
               --  Leaf statement (assignment, procedure call,
               --  assertion, ...).  Delegate to canon, but degrade
               --  gracefully on any node kind it cannot handle.
               begin
                  Vhdl.Canon.Canon_Extract_Sensitivity_Statement
                    (Stmt, List);
               exception
                  when others =>
                     null;
               end;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Collect_Reads;

   procedure Collect_Drivers (Chain : Iir; List : Iir_List) is
      Stmt : Iir := Chain;
   begin
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Simple_Signal_Assignment_Statement
               | Iir_Kind_Conditional_Signal_Assignment_Statement
               | Iir_Kind_Selected_Waveform_Assignment_Statement
               | Iir_Kind_Signal_Force_Assignment_Statement
               | Iir_Kind_Signal_Release_Assignment_Statement =>
               declare
                  D : constant Iir := Resolve_Obj (Get_Target (Stmt));
               begin
                  if D /= Null_Iir then
                     Append_Element (List, D);
                  end if;
               end;
            when Iir_Kind_If_Statement =>
               declare
                  Cl : Iir := Stmt;
               begin
                  while Cl /= Null_Iir loop
                     Collect_Drivers
                       (Get_Sequential_Statement_Chain (Cl), List);
                     Cl := Get_Else_Clause (Cl);
                  end loop;
               end;
            when Iir_Kind_Case_Statement =>
               declare
                  Alt : Iir := Get_Case_Statement_Alternative_Chain (Stmt);
               begin
                  while Alt /= Null_Iir loop
                     Collect_Drivers (Get_Associated_Chain (Alt), List);
                     Alt := Get_Chain (Alt);
                  end loop;
               end;
            when Iir_Kind_For_Loop_Statement
               | Iir_Kind_While_Loop_Statement =>
               Collect_Drivers
                 (Get_Sequential_Statement_Chain (Stmt), List);
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Collect_Drivers;

   --  ------------------------------------------------------------------
   --  Concurrent-statement walkers (each emits one module sub-array).
   --  Generate bodies are flattened into the enclosing module.
   --  ------------------------------------------------------------------

   procedure Walk_Processes
     (F : File_Type; Chain : Iir; First : in out Boolean);
   procedure Walk_Assignments
     (F : File_Type; Chain : Iir; First : in out Boolean);
   procedure Walk_Instances
     (F : File_Type; Chain : Iir; First : in out Boolean);

   --  Emit a single process object.
   procedure Emit_Process (F : File_Type; Proc : Iir) is
      Reads : constant Iir_List := Create_Iir_List;
      Drives : constant Iir_List := Create_Iir_List;
      Lbl : constant Name_Id := Get_Label (Proc);
   begin
      Put (F, "{""label"": ");
      if Lbl = Null_Identifier then
         Put (F, "null");
      else
         Put_Str (F, Name_Table.Image (Lbl));
      end if;
      Put (F, ", ""pos"": ");
      Put_Pos (F, Proc);

      Put (F, ", ""sensitivity"": ");
      if Get_Kind (Proc) = Iir_Kind_Sensitized_Process_Statement then
         Put_Name_List (F, Get_Sensitivity_List (Proc));
      else
         Put (F, "[]");
      end if;

      Collect_Reads (Get_Sequential_Statement_Chain (Proc), Reads);
      Collect_Drivers (Get_Sequential_Statement_Chain (Proc), Drives);

      Put (F, ", ""drives"": ");
      Put_Name_List (F, Drives);
      Put (F, ", ""reads"": ");
      Put_Name_List (F, Reads);
      Put (F, "}");
   end Emit_Process;

   procedure Walk_Processes
     (F : File_Type; Chain : Iir; First : in out Boolean)
   is
      Stmt : Iir := Chain;
   begin
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Sensitized_Process_Statement
               | Iir_Kind_Process_Statement =>
               if not First then
                  Put (F, ", ");
               end if;
               First := False;
               Emit_Process (F, Stmt);
            when Iir_Kind_For_Generate_Statement
               | Iir_Kind_If_Generate_Statement =>
               declare
                  Bod : constant Iir :=
                    Get_Generate_Statement_Body (Stmt);
               begin
                  if Bod /= Null_Iir then
                     Walk_Processes
                       (F, Get_Concurrent_Statement_Chain (Bod), First);
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Walk_Processes;

   --  Emit a single concurrent-assignment object.
   procedure Emit_Assignment (F : File_Type; Stmt : Iir) is
      Reads : constant Iir_List := Create_Iir_List;
   begin
      Put (F, "{""target"": ");
      Put_Str (F, Obj_Name (Get_Target (Stmt)));
      Put (F, ", ""pos"": ");
      Put_Pos (F, Stmt);

      begin
         case Get_Kind (Stmt) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
               Vhdl.Canon.
                 Canon_Extract_Sensitivity_Simple_Signal_Assignment
                   (Stmt, Reads);
            when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               Vhdl.Canon.
                 Canon_Extract_Sensitivity_Conditional_Signal_Assignment
                   (Stmt, Reads);
            when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               Vhdl.Canon.
                 Canon_Extract_Sensitivity_Selected_Signal_Assignment
                   (Stmt, Reads);
            when others =>
               null;
         end case;
      exception
         when others =>
            null;
      end;

      Put (F, ", ""drives"": [");
      declare
         Tgt : constant String := Obj_Name (Get_Target (Stmt));
      begin
         if Tgt /= "" then
            Put_Str (F, Tgt);
         end if;
      end;
      Put (F, "], ""reads"": ");
      Put_Name_List (F, Reads);
      Put (F, "}");
   end Emit_Assignment;

   procedure Walk_Assignments
     (F : File_Type; Chain : Iir; First : in out Boolean)
   is
      Stmt : Iir := Chain;
   begin
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Concurrent_Simple_Signal_Assignment
               | Iir_Kind_Concurrent_Conditional_Signal_Assignment
               | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               if not First then
                  Put (F, ", ");
               end if;
               First := False;
               Emit_Assignment (F, Stmt);
            when Iir_Kind_For_Generate_Statement
               | Iir_Kind_If_Generate_Statement =>
               declare
                  Bod : constant Iir :=
                    Get_Generate_Statement_Body (Stmt);
               begin
                  if Bod /= Null_Iir then
                     Walk_Assignments
                       (F, Get_Concurrent_Statement_Chain (Bod), First);
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Walk_Assignments;

   --  Resolve the module name / arch / entity-flag of an instantiation.
   procedure Inst_Unit
     (Stmt : Iir;
      Mod_Name : out Name_Id;
      Arch_Name : out Name_Id;
      Is_Entity : out Boolean)
   is
      IU : constant Iir := Get_Instantiated_Unit (Stmt);
      U : Iir := IU;
   begin
      Mod_Name := Null_Identifier;
      Arch_Name := Null_Identifier;
      Is_Entity := False;
      if U = Null_Iir then
         return;
      end if;
      if Get_Kind (U) = Iir_Kind_Entity_Aspect_Entity then
         Is_Entity := True;
         declare
            En : constant Iir := Get_Entity_Name (U);
            Ar : constant Iir := Get_Architecture (U);
            Re : Iir;
         begin
            if En /= Null_Iir then
               Re := Get_Named_Entity (En);
               if Re /= Null_Iir then
                  Mod_Name := Get_Identifier (Re);
               else
                  Mod_Name := Get_Identifier (En);
               end if;
            end if;
            if Ar /= Null_Iir then
               Arch_Name := Get_Identifier (Ar);
            end if;
         end;
         return;
      end if;
      if Get_Kind (U) in Iir_Kinds_Denoting_Name then
         declare
            Ne : constant Iir := Get_Named_Entity (U);
         begin
            if Ne /= Null_Iir then
               U := Ne;
            end if;
         end;
      end if;
      case Get_Kind (U) is
         when Iir_Kind_Component_Declaration =>
            Mod_Name := Get_Identifier (U);
         when Iir_Kind_Entity_Declaration =>
            Is_Entity := True;
            Mod_Name := Get_Identifier (U);
         when others =>
            if Get_Kind (U) in Iir_Kinds_Denoting_Name then
               Mod_Name := Get_Identifier (U);
            end if;
      end case;
   end Inst_Unit;

   --  Emit a port_map or generic_map association chain.
   procedure Emit_Map (F : File_Type; Chain : Iir) is
      Assoc : Iir := Chain;
      First : Boolean := True;
   begin
      while Assoc /= Null_Iir loop
         declare
            Formal : constant Iir := Get_Formal (Assoc);
            Positional : constant Boolean := Formal = Null_Iir;
            Actual : Iir := Null_Iir;
         begin
            case Get_Kind (Assoc) is
               when Iir_Kind_Association_Element_By_Expression
                  | Iir_Kind_Association_Element_By_Name =>
                  Actual := Get_Actual (Assoc);
               when others =>
                  Actual := Null_Iir;
            end case;

            if not First then
               Put (F, ", ");
            end if;
            First := False;
            Put (F, "{""formal"": ");
            if Positional then
               Put (F, "null");
            else
               Put_Str (F, Obj_Name (Formal));
            end if;
            if not Positional then
               Put (F, ", ""fbase"": ");
               Put_Str (F, Obj_Name (Formal));
            end if;
            Put (F, ", ""actual"": ");
            Put_Str (F, Obj_Name (Actual));
            Put (F, ", ""actuals"": [");
            declare
               A : constant String := Obj_Name (Actual);
            begin
               if A /= "" then
                  Put_Str (F, A);
               end if;
            end;
            Put (F, "]");
            if Positional then
               Put (F, ", ""positional"": true");
            end if;
            Put (F, "}");
         end;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Emit_Map;

   --  Emit a single instantiation object.
   procedure Emit_Instance (F : File_Type; Stmt : Iir) is
      Mod_Name : Name_Id;
      Arch_Name : Name_Id;
      Is_Entity : Boolean;
      Lbl : constant Name_Id := Get_Label (Stmt);
   begin
      Inst_Unit (Stmt, Mod_Name, Arch_Name, Is_Entity);
      Put (F, "{""label"": ");
      if Lbl = Null_Identifier then
         Put (F, "null");
      else
         Put_Str (F, Name_Table.Image (Lbl));
      end if;
      Put (F, ", ""module"": ");
      if Mod_Name = Null_Identifier then
         Put_Str (F, "");
      else
         Put_Str (F, Name_Table.Image (Mod_Name));
      end if;
      if Arch_Name /= Null_Identifier then
         Put (F, ", ""arch"": ");
         Put_Str (F, Name_Table.Image (Arch_Name));
      end if;
      Put (F, ", ""is_entity_inst"": ");
      Put (F, (if Is_Entity then "true" else "false"));
      Put (F, ", ""pos"": ");
      Put_Pos (F, Stmt);
      Put (F, ", ""generic_map"": [");
      Emit_Map (F, Get_Generic_Map_Aspect_Chain (Stmt));
      Put (F, "], ""port_map"": [");
      Emit_Map (F, Get_Port_Map_Aspect_Chain (Stmt));
      Put (F, "]}");
   end Emit_Instance;

   procedure Walk_Instances
     (F : File_Type; Chain : Iir; First : in out Boolean)
   is
      Stmt : Iir := Chain;
   begin
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement =>
               if not First then
                  Put (F, ", ");
               end if;
               First := False;
               Emit_Instance (F, Stmt);
            when Iir_Kind_For_Generate_Statement
               | Iir_Kind_If_Generate_Statement =>
               declare
                  Bod : constant Iir :=
                    Get_Generate_Statement_Body (Stmt);
               begin
                  if Bod /= Null_Iir then
                     Walk_Instances
                       (F, Get_Concurrent_Statement_Chain (Bod), First);
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Walk_Instances;

   --  ------------------------------------------------------------------
   --  Entity interface (ports / generics).
   --  ------------------------------------------------------------------

   function Mode_Image (M : Iir_Mode) return String is
   begin
      case M is
         when Iir_In_Mode =>
            return "in";
         when Iir_Out_Mode =>
            return "out";
         when Iir_Inout_Mode =>
            return "inout";
         when Iir_Buffer_Mode =>
            return "buffer";
         when Iir_Linkage_Mode =>
            return "linkage";
         when others =>
            return "in";
      end case;
   end Mode_Image;

   procedure Emit_Ports (F : File_Type; Ent : Iir) is
      Port : Iir;
      First : Boolean := True;
   begin
      if Ent = Null_Iir then
         return;
      end if;
      Port := Get_Port_Chain (Ent);
      while Port /= Null_Iir loop
         if not First then
            Put (F, ", ");
         end if;
         First := False;
         Put (F, "{""name"": ");
         Put_Str (F, Name_Table.Image (Get_Identifier (Port)));
         Put (F, ", ""dir"": ");
         Put_Str (F, Mode_Image (Get_Mode (Port)));
         Put (F, ", ""type"": ");
         Put_Str (F, Type_Image (Port));
         Put (F, ", ""pos"": ");
         Put_Pos (F, Port);
         Put (F, "}");
         Port := Get_Chain (Port);
      end loop;
   end Emit_Ports;

   procedure Emit_Generics (F : File_Type; Ent : Iir) is
      Gen : Iir;
      First : Boolean := True;
   begin
      if Ent = Null_Iir then
         return;
      end if;
      Gen := Get_Generic_Chain (Ent);
      while Gen /= Null_Iir loop
         if not First then
            Put (F, ", ");
         end if;
         First := False;
         Put (F, "{""name"": ");
         Put_Str (F, Name_Table.Image (Get_Identifier (Gen)));
         Put (F, ", ""type"": ");
         Put_Str (F, Type_Image (Gen));
         Put (F, ", ""pos"": ");
         Put_Pos (F, Gen);
         Put (F, "}");
         Gen := Get_Chain (Gen);
      end loop;
   end Emit_Generics;

   --  Emit constant declarations from a declaration chain, with the
   --  static integer value when the default expression is a literal.
   procedure Emit_Constant_Decls (F : File_Type; Chain : Node) is
      Decl : Node := Chain;
      First : Boolean := True;
   begin
      while Decl /= Null_Node loop
         if Get_Kind (Decl) = Iir_Kind_Constant_Declaration then
            if not First then
               Put (F, ", ");
            end if;
            First := False;
            Put (F, "{""name"": ");
            Put_Str (F, Name_Table.Image (Get_Identifier (Decl)));
            Put (F, ", ""type"": ");
            Put_Str (F, Type_Image (Decl));
            declare
               IV : constant String :=
                 Int_Expr_Image (Get_Default_Value (Decl));
            begin
               if IV /= "" then
                  Put (F, ", ""value"": " & IV);
               end if;
            end;
            Put (F, ", ""pos"": ");
            Put_Pos (F, Decl);
            Put (F, "}");
         end if;
         Decl := Get_Chain (Decl);
      end loop;
   end Emit_Constant_Decls;

   procedure Emit_Signals (F : File_Type; Arch : Iir) is
      Decl : Iir;
      First : Boolean := True;
   begin
      Decl := Get_Declaration_Chain (Arch);
      while Decl /= Null_Iir loop
         if Get_Kind (Decl) = Iir_Kind_Signal_Declaration then
            if not First then
               Put (F, ", ");
            end if;
            First := False;
            Put (F, "{""name"": ");
            Put_Str (F, Name_Table.Image (Get_Identifier (Decl)));
            Put (F, ", ""type"": ");
            Put_Str (F, Type_Image (Decl));
            Put (F, ", ""skind"": ");
            Put_Str (F, "signal");
            Put (F, ", ""pos"": ");
            Put_Pos (F, Decl);
            Put (F, "}");
         end if;
         Decl := Get_Chain (Decl);
      end loop;
   end Emit_Signals;

   --  ------------------------------------------------------------------
   --  Module emission (one per architecture body).
   --  ------------------------------------------------------------------

   procedure Emit_Module (F : File_Type; Arch : Iir) is
      Ent : constant Iir :=
        Get_Named_Entity (Get_Entity_Name (Arch));
      Chain : constant Iir := Get_Concurrent_Statement_Chain (Arch);
      First : Boolean;
   begin
      Put (F, "    {""name"": ");
      if Ent /= Null_Iir then
         Put_Str (F, Name_Table.Image (Get_Identifier (Ent)));
      else
         Put_Str (F, "");
      end if;
      Put (F, ", ""kind"": ");
      Put_Str (F, "entity");
      Put (F, ", ""lang"": ");
      Put_Str (F, "vhdl");
      Put (F, ", ""file"": ");
      Put_Str (F, Node_File (Arch));
      Put (F, ", ""pos"": ");
      Put_Pos (F, Arch);
      Put (F, ", ""arch"": ");
      Put_Str (F, Name_Table.Image (Get_Identifier (Arch)));
      Put (F, ",");

      New_Line (F);
      Put (F, "      ""ports"": [");
      Emit_Ports (F, Ent);
      Put (F, "],");
      New_Line (F);
      Put (F, "      ""generics"": [");
      Emit_Generics (F, Ent);
      Put (F, "],");
      New_Line (F);
      Put (F, "      ""signals"": [");
      Emit_Signals (F, Arch);
      Put (F, "],");
      New_Line (F);
      Put (F, "      ""constants"": [");
      Emit_Constant_Decls (F, Get_Declaration_Chain (Arch));
      Put (F, "],");
      New_Line (F);

      Put (F, "      ""processes"": [");
      First := True;
      Walk_Processes (F, Chain, First);
      Put (F, "],");
      New_Line (F);

      Put (F, "      ""assignments"": [");
      First := True;
      Walk_Assignments (F, Chain, First);
      Put (F, "],");
      New_Line (F);

      Put (F, "      ""generates"": [],");
      New_Line (F);

      Put (F, "      ""instances"": [");
      First := True;
      Walk_Instances (F, Chain, First);
      Put (F, "]}");
   end Emit_Module;

   --  ------------------------------------------------------------------
   --  Elaborated hierarchy (hierarchy[] from the instance tree).
   --  Mirrors the elaborator's own traversal (Gather_Processes_1):
   --  descend via Get_Sub_Instance / Get_Generate_Sub_Instance.
   --  ------------------------------------------------------------------

   --  Emit a Name_Id as a JSON string ("" if Null).
   procedure Put_Name (F : File_Type; Id : Name_Id) is
   begin
      if Id = Null_Identifier then
         Put_Str (F, "");
      else
         Put_Str (F, Name_Table.Image (Id));
      end if;
   end Put_Name;

   --  Resolve an instance to (entity name, arch name, body instance).
   --  For a component instantiation the body instance is the inner
   --  component instance (bound architecture); otherwise it is Inst.
   procedure Inst_Module_Arch
     (Inst : Synth_Instance_Acc;
      Mod_Name : out Name_Id;
      Arch_Name : out Name_Id;
      Body_Inst : out Synth_Instance_Acc)
   is
      S : Node;
   begin
      Mod_Name := Null_Identifier;
      Arch_Name := Null_Identifier;
      Body_Inst := Inst;
      if Inst = null then
         return;
      end if;
      S := Get_Source_Scope (Inst);
      if S = Null_Node then
         return;
      end if;
      if Get_Kind (S) = Iir_Kind_Component_Declaration then
         Mod_Name := Get_Identifier (S);
         declare
            CI : constant Synth_Instance_Acc :=
              Get_Component_Instance (Inst);
         begin
            if CI = null then
               return;
            end if;
            Body_Inst := CI;
            S := Get_Source_Scope (CI);
         end;
      end if;
      if Get_Kind (S) = Iir_Kind_Architecture_Body then
         Arch_Name := Get_Identifier (S);
         declare
            Ent : constant Node := Vhdl.Utils.Get_Entity (S);
         begin
            if Ent /= Null_Node then
               Mod_Name := Get_Identifier (Ent);
            end if;
         end;
      end if;
   end Inst_Module_Arch;

   --  Concurrent statement chain of an instance's elaborated scope.
   function Scope_Chain (Inst : Synth_Instance_Acc) return Node is
      S : Node;
   begin
      if Inst = null then
         return Null_Node;
      end if;
      S := Get_Source_Scope (Inst);
      if S = Null_Node then
         return Null_Node;
      end if;
      case Get_Kind (S) is
         when Iir_Kind_Architecture_Body
            | Iir_Kind_Generate_Statement_Body
            | Iir_Kind_Block_Statement =>
            return Get_Concurrent_Statement_Chain (S);
         when others =>
            return Null_Node;
      end case;
   end Scope_Chain;

   procedure Emit_Children
     (F : File_Type; Inst : Synth_Instance_Acc; First : in out Boolean);
   procedure Emit_Instance_Node
     (F : File_Type; Inst : Synth_Instance_Acc; Stmt : Node;
      First : in out Boolean);
   procedure Emit_Frame_Node
     (F : File_Type; Inst : Synth_Instance_Acc; Kind : String;
      Label : Name_Id; Index : Natural; First : in out Boolean);

   --  Emit the resolved generics of an instance: {name, type, value}
   --  with the elaborated discrete value when available.
   procedure Emit_Resolved_Generics
     (F : File_Type; Ent : Node; Inst : Synth_Instance_Acc)
   is
      Gen : Node;
      First : Boolean := True;
   begin
      if Ent = Null_Node or else Inst = null then
         return;
      end if;
      Gen := Get_Generic_Chain (Ent);
      while Gen /= Null_Node loop
         if Get_Kind (Gen) = Iir_Kind_Interface_Constant_Declaration then
            declare
               Has_Val : Boolean := False;
               V : Int64 := 0;
            begin
               begin
                  V := Read_Discrete (Get_Value (Inst, Gen));
                  Has_Val := True;
               exception
                  when others =>
                     Has_Val := False;
               end;
               if not First then
                  Put (F, ", ");
               end if;
               First := False;
               Put (F, "{""name"": ");
               Put_Str (F, Name_Table.Image (Get_Identifier (Gen)));
               Put (F, ", ""type"": ");
               Put_Str (F, Type_Image (Gen));
               if Has_Val then
                  Put (F, ", ""value"": " & Img64 (V));
               end if;
               Put (F, "}");
            end;
         end if;
         Gen := Get_Chain (Gen);
      end loop;
   end Emit_Resolved_Generics;

   procedure Emit_Instance_Node
     (F : File_Type; Inst : Synth_Instance_Acc; Stmt : Node;
      First : in out Boolean)
   is
      Mod_Name : Name_Id;
      Arch_Name : Name_Id;
      Body_Inst : Synth_Instance_Acc;
      Lbl : Name_Id;
      Ent : Node := Null_Node;
      Scope : Node;
      Cf : Boolean := True;
   begin
      Inst_Module_Arch (Inst, Mod_Name, Arch_Name, Body_Inst);
      Scope := Get_Source_Scope (Body_Inst);
      if Scope /= Null_Node
        and then Get_Kind (Scope) = Iir_Kind_Architecture_Body
      then
         Ent := Vhdl.Utils.Get_Entity (Scope);
      end if;
      if Stmt /= Null_Node then
         Lbl := Get_Label (Stmt);
      else
         Lbl := Mod_Name;
      end if;

      if not First then
         Put (F, ", ");
      end if;
      First := False;

      Put (F, "{""name"": ");
      Put_Name (F, Lbl);
      Put (F, ", ""module"": ");
      Put_Name (F, Mod_Name);
      if Arch_Name /= Null_Identifier then
         Put (F, ", ""arch"": ");
         Put_Name (F, Arch_Name);
      end if;
      Put (F, ", ""instance_label"": ");
      Put_Name (F, Lbl);
      Put (F, ", ""inst_pos"": ");
      if Stmt /= Null_Node then
         Put_Pos (F, Stmt);
      else
         Put_Pos (F, Get_Source_Scope (Body_Inst));
      end if;
      Put (F, ", ""generics"": [");
      Emit_Resolved_Generics (F, Ent, Body_Inst);
      Put (F, "], ""generic_map"": [");
      if Stmt /= Null_Node then
         Emit_Map (F, Get_Generic_Map_Aspect_Chain (Stmt));
      end if;
      Put (F, "], ""port_map"": [");
      if Stmt /= Null_Node then
         Emit_Map (F, Get_Port_Map_Aspect_Chain (Stmt));
      end if;
      Put (F, "], ""children"": [");
      Emit_Children (F, Body_Inst, Cf);
      Put (F, "]}");
   end Emit_Instance_Node;

   procedure Emit_Frame_Node
     (F : File_Type; Inst : Synth_Instance_Acc; Kind : String;
      Label : Name_Id; Index : Natural; First : in out Boolean)
   is
      Cf : Boolean := True;
   begin
      if not First then
         Put (F, ", ");
      end if;
      First := False;

      Put (F, "{""name"": ");
      if Kind = "for" and then Label /= Null_Identifier then
         Put_Str (F, Name_Table.Image (Label) & "(" & Img (Index) & ")");
      else
         Put_Name (F, Label);
      end if;
      Put (F, ", ""scope"": true, ""generate"": {""kind"": ");
      Put_Str (F, Kind);
      Put (F, ", ""label"": ");
      Put_Name (F, Label);
      if Kind = "for" then
         Put (F, ", ""index"": " & Img (Index));
      elsif Kind = "if" then
         Put (F, ", ""arm"": 0");
      end if;
      Put (F, "}, ""children"": [");
      Emit_Children (F, Inst, Cf);
      Put (F, "]}");
   end Emit_Frame_Node;

   procedure Emit_Children
     (F : File_Type; Inst : Synth_Instance_Acc; First : in out Boolean)
   is
      Stmt : Node := Scope_Chain (Inst);
   begin
      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  if Sub /= null then
                     Emit_Instance_Node (F, Sub, Stmt, First);
                  end if;
               end;
            when Iir_Kind_For_Generate_Statement =>
               begin
                  declare
                     It : constant Node :=
                       Get_Parameter_Specification (Stmt);
                     Rng : constant Type_Acc :=
                       Get_Subtype_Object (Inst, Get_Type (It));
                     Len : constant Natural :=
                       Natural (Get_Range_Length (Rng.Drange));
                     Gen_Inst : constant Synth_Instance_Acc :=
                       Get_Sub_Instance (Inst, Stmt);
                  begin
                     for I in 1 .. Len loop
                        declare
                           GBody : constant Synth_Instance_Acc :=
                             Get_Generate_Sub_Instance (Gen_Inst, I);
                           --  The actual VHDL loop value (e.g. 0..N-1),
                           --  not the 1-based iteration ordinal.
                           Idx : Integer := I - 1;
                        begin
                           begin
                              Idx := Integer
                                (Read_Discrete (Get_Value (GBody, It)));
                           exception
                              when others =>
                                 Idx := I - 1;
                           end;
                           Emit_Frame_Node
                             (F, GBody, "for", Get_Label (Stmt), Idx,
                              First);
                        end;
                     end loop;
                  end;
               exception
                  when others =>
                     null;
               end;
            when Iir_Kind_If_Generate_Statement
               | Iir_Kind_Case_Generate_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  if Sub /= null then
                     Emit_Frame_Node
                       (F, Sub, "if", Get_Label (Stmt), 0, First);
                  end if;
               end;
            when Iir_Kind_Block_Statement =>
               declare
                  Sub : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Inst, Stmt);
               begin
                  if Sub /= null then
                     Emit_Frame_Node
                       (F, Sub, "block", Get_Label (Stmt), 0, First);
                  end if;
               end;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Emit_Children;

   --  Hierarchical name of the top unit (its entity), or "".
   function Top_Name return String is
      S : Node;
   begin
      if Top_Instance = null then
         return "";
      end if;
      S := Get_Source_Scope (Top_Instance);
      if S /= Null_Node
        and then Get_Kind (S) = Iir_Kind_Architecture_Body
      then
         declare
            Ent : constant Node := Vhdl.Utils.Get_Entity (S);
         begin
            if Ent /= Null_Node then
               return Name_Table.Image (Get_Identifier (Ent));
            end if;
         end;
      end if;
      return "";
   end Top_Name;

   --  ------------------------------------------------------------------
   --  Industrial polish: canonical net graph (nets[] + cells[]) built
   --  from the elaborated Drivers/Sensitivity/Connect tables, plus
   --  comb-vs-clocked classification and provenance.
   --  ------------------------------------------------------------------

   --  The VHDL standard being elaborated, as a 2-char tag.
   function Std_Image return String is
   begin
      case Flags.Vhdl_Std is
         when Flags.Vhdl_87 => return "87";
         when Flags.Vhdl_93 => return "93";
         when Flags.Vhdl_00 => return "00";
         when Flags.Vhdl_02 => return "02";
         when Flags.Vhdl_08 => return "08";
         when Flags.Vhdl_19 => return "19";
      end case;
   end Std_Image;

   --  Canonical net of a signal: follow Collapsed_By to the root.  This
   --  unifies a port with the actual net it binds to.
   function Canon_Net (S : Signal_Index_Type) return Signal_Index_Type is
      R : Signal_Index_Type := S;
   begin
      loop
         exit when Signals_Table.Table (R).Collapsed_By = No_Signal_Index;
         R := Signals_Table.Table (R).Collapsed_By;
      end loop;
      return R;
   end Canon_Net;

   --  Label of an instantiation/scope statement, or Null.
   function Stmt_Label (St : Node) return Name_Id is
   begin
      if St = Null_Node then
         return Null_Identifier;
      end if;
      case Get_Kind (St) is
         when Iir_Kind_Component_Instantiation_Statement
            | Iir_Kind_For_Generate_Statement
            | Iir_Kind_If_Generate_Statement
            | Iir_Kind_Case_Generate_Statement
            | Iir_Kind_Block_Statement =>
            return Get_Label (St);
         when others =>
            return Null_Identifier;
      end case;
   end Stmt_Label;

   --  Dotted hierarchical path of an instance ("top.uut").
   function Inst_Path (Inst : Synth_Instance_Acc) return String is
   begin
      if Inst = null then
         return "";
      end if;
      if Inst = Top_Instance then
         return Top_Name;
      end if;
      declare
         Par : constant Synth_Instance_Acc := Get_Instance_Parent (Inst);
         Lbl : constant Name_Id :=
           Stmt_Label (Get_Statement_Scope (Inst));
         P : constant String := Inst_Path (Par);
      begin
         if Lbl = Null_Identifier then
            return P;
         elsif P = "" then
            return Name_Table.Image (Lbl);
         else
            return P & "." & Name_Table.Image (Lbl);
         end if;
      end;
   end Inst_Path;

   --  Index of a signal declaration within an instance, or No_Signal_Index.
   function Find_Signal (Decl : Node; Inst : Synth_Instance_Acc)
                        return Signal_Index_Type is
      By_Decl : Signal_Index_Type := No_Signal_Index;
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         if Signals_Table.Table (I).Decl = Decl then
            if Signals_Table.Table (I).Inst = Inst then
               return I;
            elsif By_Decl = No_Signal_Index then
               By_Decl := I;
            end if;
         end if;
      end loop;
      return By_Decl;
   end Find_Signal;

   --  Is DEF one of the std edge-detection functions?
   function Is_Edge_Def (Def : Iir_Predefined_Functions) return Boolean is
   begin
      case Def is
         when Iir_Predefined_Boolean_Rising_Edge
            | Iir_Predefined_Boolean_Falling_Edge
            | Iir_Predefined_Bit_Rising_Edge
            | Iir_Predefined_Bit_Falling_Edge
            | Iir_Predefined_Ieee_1164_Rising_Edge
            | Iir_Predefined_Ieee_1164_Falling_Edge =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Edge_Def;

   --  If E is an edge expression (rising_edge(clk), clk'event ...) return
   --  the clock signal declaration, else Null.
   function Clock_In_Expr (E : Node) return Node is
   begin
      if E = Null_Node then
         return Null_Node;
      end if;
      case Get_Kind (E) is
         when Iir_Kind_Function_Call =>
            declare
               Impl : constant Node := Get_Implementation (E);
            begin
               if Impl /= Null_Node
                 and then Is_Edge_Def (Get_Implicit_Definition (Impl))
               then
                  declare
                     Assoc : constant Node :=
                       Get_Parameter_Association_Chain (E);
                  begin
                     if Assoc /= Null_Node then
                        return Resolve_Obj (Get_Actual (Assoc));
                     end if;
                  end;
               end if;
            end;
            return Null_Node;
         when Iir_Kind_Event_Attribute =>
            return Resolve_Obj (Get_Prefix (E));
         when Iir_Kinds_Dyadic_Operator =>
            declare
               C : constant Node := Clock_In_Expr (Get_Left (E));
            begin
               if C /= Null_Node then
                  return C;
               end if;
               return Clock_In_Expr (Get_Right (E));
            end;
         when Iir_Kinds_Monadic_Operator =>
            return Clock_In_Expr (Get_Operand (E));
         when Iir_Kind_Parenthesis_Expression =>
            return Clock_In_Expr (Get_Expression (E));
         when others =>
            return Null_Node;
      end case;
   end Clock_In_Expr;

   --  Search a process body for an edge clock; return the clock signal
   --  declaration or Null.
   function Process_Clock (Chain : Node) return Node is
      Stmt : Node := Chain;
      C, R : Node;
   begin
      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when Iir_Kind_If_Statement =>
               declare
                  Cl : Node := Stmt;
               begin
                  while Cl /= Null_Node loop
                     C := Clock_In_Expr (Get_Condition (Cl));
                     if C /= Null_Node then
                        return C;
                     end if;
                     R := Process_Clock
                       (Get_Sequential_Statement_Chain (Cl));
                     if R /= Null_Node then
                        return R;
                     end if;
                     Cl := Get_Else_Clause (Cl);
                  end loop;
               end;
            when Iir_Kind_Wait_Statement =>
               C := Clock_In_Expr (Get_Condition_Clause (Stmt));
               if C /= Null_Node then
                  return C;
               end if;
            when Iir_Kind_Case_Statement =>
               declare
                  Alt : Node := Get_Case_Statement_Alternative_Chain (Stmt);
               begin
                  while Alt /= Null_Node loop
                     R := Process_Clock (Get_Associated_Chain (Alt));
                     if R /= Null_Node then
                        return R;
                     end if;
                     Alt := Get_Chain (Alt);
                  end loop;
               end;
            when Iir_Kind_For_Loop_Statement
               | Iir_Kind_While_Loop_Statement =>
               R := Process_Clock (Get_Sequential_Statement_Chain (Stmt));
               if R /= Null_Node then
                  return R;
               end if;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
      return Null_Node;
   end Process_Clock;

   --  Emit the comma-separated cell (elaborated process/assignment) ids
   --  that drive (Drivers_Mode) or read a net, aggregated over all the
   --  signals that collapse into NET.
   procedure Emit_Net_Procs
     (F : File_Type; Net : Signal_Index_Type; Drivers_Mode : Boolean)
   is
      Fr : Boolean := True;
      Seen : array (1 .. 8192) of Process_Index_Type :=
        (others => No_Process_Index);
      Ns : Natural := 0;

      procedure Add (P : Process_Index_Type) is
         Dup : Boolean := False;
      begin
         if P = No_Process_Index then
            return;
         end if;
         for K in 1 .. Ns loop
            if Seen (K) = P then
               Dup := True;
               exit;
            end if;
         end loop;
         if not Dup then
            if Ns < Seen'Last then
               Ns := Ns + 1;
               Seen (Ns) := P;
            end if;
            if not Fr then
               Put (F, ", ");
            end if;
            Fr := False;
            Put (F, Img (Integer (P)));
         end if;
      end Add;
   begin
      for J in Signals_Table.First .. Signals_Table.Last loop
         if Canon_Net (J) = Net then
            declare
               Idx : Driver_Index_Type;
            begin
               if Drivers_Mode then
                  if Signals_Table.Table (J).Kind = Signal_User then
                     Idx := Signals_Table.Table (J).Drivers;
                  else
                     Idx := No_Driver_Index;
                  end if;
                  while Idx /= No_Driver_Index loop
                     Add (Drivers_Table.Table (Idx).Proc);
                     Idx := Drivers_Table.Table (Idx).Prev_Sig;
                  end loop;
               else
                  Idx := Signals_Table.Table (J).Sensitivity;
                  while Idx /= No_Sensitivity_Index loop
                     Add (Sensitivity_Table.Table (Idx).Proc);
                     Idx := Sensitivity_Table.Table (Idx).Prev_Sig;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Emit_Net_Procs;

   --  Emit canonical net ids reached by walking a per-process driver or
   --  sensitivity chain (via Prev_Proc).
   procedure Emit_Proc_Nets
     (F : File_Type; Head : Driver_Index_Type; Drivers_Mode : Boolean)
   is
      Fr : Boolean := True;
      Seen : array (1 .. 8192) of Signal_Index_Type :=
        (others => No_Signal_Index);
      Ns : Natural := 0;
      Idx : Driver_Index_Type := Head;
      Net : Signal_Index_Type;
      Dup : Boolean;
   begin
      while Idx /= No_Driver_Index loop
         if Drivers_Mode then
            Net := Canon_Net (Drivers_Table.Table (Idx).Sig.Base);
         else
            Net := Canon_Net (Sensitivity_Table.Table (Idx).Sig.Base);
         end if;
         Dup := False;
         for K in 1 .. Ns loop
            if Seen (K) = Net then
               Dup := True;
               exit;
            end if;
         end loop;
         if not Dup then
            if Ns < Seen'Last then
               Ns := Ns + 1;
               Seen (Ns) := Net;
            end if;
            if not Fr then
               Put (F, ", ");
            end if;
            Fr := False;
            Put (F, Img (Integer (Net)));
         end if;
         if Drivers_Mode then
            Idx := Drivers_Table.Table (Idx).Prev_Proc;
         else
            Idx := Sensitivity_Table.Table (Idx).Prev_Proc;
         end if;
      end loop;
   end Emit_Proc_Nets;

   procedure Emit_Cells (F : File_Type) is
      First : Boolean := True;
   begin
      for P in Processes_Table.First .. Processes_Table.Last loop
         declare
            E : Proc_Record_Type renames Processes_Table.Table (P);
            Is_Assign : constant Boolean :=
              Get_Kind (E.Proc) in Iir_Kinds_Concurrent_Signal_Assignment;
            Clk : Node := Null_Node;
            Lbl : constant Name_Id := Get_Label (E.Proc);
         begin
            if not First then
               Put (F, ",");
            end if;
            First := False;
            New_Line (F);
            Put (F, "    {""id"": " & Img (Integer (P)));
            Put (F, ", ""kind"": ");
            Put_Str (F, (if Is_Assign then "assign" else "process"));
            Put (F, ", ""label"": ");
            if Lbl /= Null_Identifier then
               Put_Str (F, Name_Table.Image (Lbl));
            elsif Is_Assign then
               Put_Str (F, Obj_Name (Get_Target (E.Proc)));
            else
               Put (F, "null");
            end if;
            Put (F, ", ""path"": ");
            Put_Str (F, Inst_Path (E.Inst));
            Put (F, ", ""pos"": ");
            Put_Pos (F, E.Proc);

            if not Is_Assign then
               begin
                  Clk := Process_Clock
                    (Get_Sequential_Statement_Chain (E.Proc));
               exception
                  when others =>
                     Clk := Null_Node;
               end;
            end if;
            Put (F, ", ""clocked"": ");
            Put (F, (if Clk /= Null_Node then "true" else "false"));
            if Clk /= Null_Node then
               declare
                  Cn : constant Signal_Index_Type :=
                    Find_Signal (Clk, E.Inst);
               begin
                  if Cn /= No_Signal_Index then
                     Put (F, ", ""clock_net"": "
                          & Img (Integer (Canon_Net (Cn))));
                  end if;
               end;
            end if;

            Put (F, ", ""drives"": [");
            Emit_Proc_Nets (F, E.Drivers, True);
            Put (F, "], ""reads"": [");
            Emit_Proc_Nets (F, E.Sensitivity, False);
            Put (F, "]}");
         end;
      end loop;
   end Emit_Cells;

   procedure Emit_Nets (F : File_Type) is
      First : Boolean := True;
      Big : constant Boolean := Natural (Signals_Table.Last) > 20_000;
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
         begin
            if E.Kind = Signal_User
              and then E.Collapsed_By = No_Signal_Index
            then
               if not First then
                  Put (F, ",");
               end if;
               First := False;
               New_Line (F);
               Put (F, "    {""id"": " & Img (Integer (I)));
               Put (F, ", ""name"": ");
               Put_Str (F, Decl_Ident (E.Decl));
               Put (F, ", ""path"": ");
               Put_Str (F, Inst_Path (E.Inst));
               Put (F, ", ""is_port"": ");
               Put (F, (if Get_Kind (E.Decl)
                          = Iir_Kind_Interface_Signal_Declaration
                        then "true" else "false"));

               Put (F, ", ""aliases"": [");
               declare
                  Fr : Boolean := True;
               begin
                  for J in Signals_Table.First .. Signals_Table.Last loop
                     if J /= I
                       and then Signals_Table.Table (J).Collapsed_By
                                  /= No_Signal_Index
                       and then Canon_Net (J) = I
                     then
                        declare
                           Nm : constant String :=
                             Decl_Ident (Signals_Table.Table (J).Decl);
                        begin
                           if Nm /= "" then
                              if not Fr then
                                 Put (F, ", ");
                              end if;
                              Fr := False;
                              Put_Str (F, Nm);
                           end if;
                        end;
                     end if;
                  end loop;
               end;
               Put (F, "]");

               if Big then
                  Put (F, ", ""adjacency"": ""omitted_for_scale""");
               else
                  Put (F, ", ""drivers"": [");
                  Emit_Net_Procs (F, I, True);
                  Put (F, "], ""loads"": [");
                  Emit_Net_Procs (F, I, False);
                  Put (F, "]");
               end if;
               Put (F, "}");
            end if;
         end;
      end loop;
   end Emit_Nets;

   --  ------------------------------------------------------------------
   --  Top-level dump.
   --  ------------------------------------------------------------------

   --  Emit packages[] (work/user libraries only) with their constants.
   procedure Emit_Packages (F : File_Type) is
      First : Boolean := True;
      Lib, File, Unit, LU : Node;
   begin
      Lib := Libraries.Get_Libraries_Chain;
      while Lib /= Null_Node loop
         declare
            Lib_Name : constant String :=
              Name_Table.Image (Get_Identifier (Lib));
         begin
            if Lib_Name /= "std" and then Lib_Name /= "ieee" then
               File := Get_Design_File_Chain (Lib);
               while File /= Null_Node loop
                  Unit := Get_First_Design_Unit (File);
                  while Unit /= Null_Node loop
                     LU := Get_Library_Unit (Unit);
                     if LU /= Null_Node
                       and then Get_Kind (LU) = Iir_Kind_Package_Declaration
                     then
                        if not First then
                           Put (F, ",");
                        end if;
                        First := False;
                        New_Line (F);
                        Put (F, "    {""name"": ");
                        Put_Str (F, Name_Table.Image (Get_Identifier (LU)));
                        Put (F, ", ""file"": ");
                        Put_Str (F, Node_File (LU));
                        Put (F, ", ""pos"": ");
                        Put_Pos (F, LU);
                        Put (F, ", ""constants"": [");
                        Emit_Constant_Decls
                          (F, Get_Declaration_Chain (LU));
                        Put (F, "]}");
                     end if;
                     Unit := Get_Chain (Unit);
                  end loop;
                  File := Get_Chain (File);
               end loop;
            end if;
         end;
         Lib := Get_Chain (Lib);
      end loop;
   end Emit_Packages;

   procedure Dump (Filename : String) is
      F : File_Type;
      Lib : Iir;
      File : Iir;
      Unit : Iir;
      LU : Iir;
      Mod_First : Boolean := True;
   begin
      N_Files := 0;
      Create (F, Out_File, Filename);

      Put_Line (F, "{");
      Put_Line (F, "  ""schema"": ""flowtracer1.merged.v0"",");
      Put_Line
        (F, "  ""generated_by"": ""ghdl --flow (native)"",");
      Put (F, "  ""ghdl"": {""version"": ");
      Put_Str (F, Version.Ghdl_Ver);
      Put (F, ", ""hash"": ");
      Put_Str (F, Version.Ghdl_Hash);
      Put (F, ", ""std"": ");
      Put_Str (F, Std_Image);
      Put_Line (F, "},");
      Put_Line (F, "  ""lang"": ""vhdl"",");
      Put_Line (F, "  ""elaborated"": true,");
      Put_Line (F, "  ""hierarchy_prov"": ""elaborated"",");
      Put_Line (F, "  ""applied_fallbacks"": [],");
      Put (F, "  ""top"": ");
      declare
         T : constant String := Top_Name;
      begin
         if T = "" then
            Put (F, "null");
         else
            Put_Str (F, T);
         end if;
      end;
      Put_Line (F, ",");

      Put (F, "  ""modules"": [");
      Lib := Libraries.Get_Libraries_Chain;
      while Lib /= Null_Iir loop
         declare
            Lib_Name : constant String :=
              Name_Table.Image (Get_Identifier (Lib));
         begin
            if Lib_Name /= "std" and then Lib_Name /= "ieee" then
               File := Get_Design_File_Chain (Lib);
               while File /= Null_Iir loop
                  Unit := Get_First_Design_Unit (File);
                  while Unit /= Null_Iir loop
                     LU := Get_Library_Unit (Unit);
                     if LU /= Null_Iir
                       and then Get_Kind (LU) = Iir_Kind_Architecture_Body
                     then
                        if not Mod_First then
                           Put (F, ",");
                        end if;
                        Mod_First := False;
                        New_Line (F);
                        Emit_Module (F, LU);
                     end if;
                     Unit := Get_Chain (Unit);
                  end loop;
                  File := Get_Chain (File);
               end loop;
            end if;
         end;
         Lib := Get_Chain (Lib);
      end loop;
      New_Line (F);
      Put_Line (F, "  ],");

      Put (F, "  ""packages"": [");
      Emit_Packages (F);
      New_Line (F);
      Put_Line (F, "  ],");

      Put (F, "  ""hierarchy"": [");
      if Top_Instance /= null then
         declare
            Hf : Boolean := True;
         begin
            New_Line (F);
            Put (F, "    ");
            Emit_Instance_Node (F, Top_Instance, Null_Node, Hf);
            New_Line (F);
            Put (F, "  ");
         end;
      end if;
      Put_Line (F, "],");

      Put (F, "  ""nets"": [");
      Emit_Nets (F);
      New_Line (F);
      Put_Line (F, "  ],");

      Put (F, "  ""cells"": [");
      Emit_Cells (F);
      New_Line (F);
      Put_Line (F, "  ],");

      Put (F, "  ""files"": [");
      for K in 1 .. N_Files loop
         if K > 1 then
            Put (F, ", ");
         end if;
         Put_Str (F, Name_Table.Image (Files_Seen (K)));
      end loop;
      Put_Line (F, "]");

      Put_Line (F, "}");
      Close (F);
   end Dump;
end Simul.Flow;
