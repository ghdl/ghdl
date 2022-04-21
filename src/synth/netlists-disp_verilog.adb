--  Disp a netlist in verilog.
--  Copyright (C) 2021 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Types_Utils; use Types_Utils;
with Files_Map;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Locations;
with Netlists.Dump; use Netlists.Dump;

package body Netlists.Disp_Verilog is
   Flag_Merge_Lit : constant Boolean := True;
   Flag_Merge_Edge : constant Boolean := True;

   procedure Put_Type (W : Width) is
   begin
      if W > 1 then
         Put ("[");
         Put_Uns32 (W - 1);
         Put (":0] ");
      end if;
   end Put_Type;

   procedure Put_Name_Version (N : Sname) is
   begin
      Put_Uns32 (Get_Sname_Version (N));
   end Put_Name_Version;

   procedure Put_Name_1 (N : Sname)
   is
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      Prefix := Get_Sname_Prefix (N);
      if Prefix /= No_Sname then
         Put_Name_1 (Prefix);
         Put ("_");
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User =>
            Put_Id (Get_Sname_Suffix (N));
         when Sname_Artificial =>
            Put_Id (Get_Sname_Suffix (N));
         when Sname_Version =>
            Put ("n");
            Put_Name_Version (N);
      end case;
   end Put_Name_1;

   procedure Put_Name (N : Sname) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      if Get_Sname_Kind (N) = Sname_User
        and then Get_Sname_Prefix (N) = No_Sname
      then
         Put_Id (Get_Sname_Suffix (N));
      else
         Put_Name_1 (N);
      end if;
   end Put_Name;

   procedure Put_Interface_Name (N : Sname) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      --  Interface names are not versionned.
      if Get_Sname_Kind (N) in Sname_User .. Sname_Artificial  then
         Put_Name (N);
      else
         Put ("*err*");
      end if;
   end Put_Interface_Name;

   procedure Disp_Net_Name (N : Net) is
   begin
      if N = No_Net then
         Put ("<unassigned>");
         return;
      end if;

      declare
         Inst : constant Instance := Get_Net_Parent (N);
         Idx : constant Port_Idx := Get_Port_Idx (N);
         M : Module;
         Inst_Name : Sname;
         Port_Name : Sname;
      begin
         if Is_Self_Instance (Inst) then
            --  For ports of the current module, simply use the port name.
            Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
         else
            Inst_Name := Get_Instance_Name (Inst);
            Put_Name (Inst_Name);
            M := Get_Module (Inst);
            case Get_Id (M) is
               when Id_Signal
                 | Id_Isignal =>
                  --  No suffix for signals (it's 'o').
                  null;
               when others =>
                  Port_Name := Get_Output_Desc (M, Idx).Name;
                  Put ("_");
                  Put_Interface_Name (Port_Name);
            end case;
         end if;
      end;
   end Disp_Net_Name;

   procedure Disp_Pval (Pv : Pval)
   is
      Pvlen : constant Uns32 := Get_Pval_Length (Pv);
   begin
      if Pvlen = 0 then
         Put ('"');
         Put ('"');
      else
         Put_Uns32 (Pvlen);
         Put ("'b");
         Disp_Pval_Binary_Digits (Pv);
      end if;
   end Disp_Pval;

   procedure Disp_Instance_Gate (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      Idx : Port_Idx;
      Max_Idx : Port_Idx;
      Name : Sname;
      First : Boolean;
      Param : Param_Desc;
   begin
      Put ("  ");

      --  Gate name
      Name := Get_Module_Name (Imod);
      if Get_Id (Imod) < Id_User_None then
         Put (" gate_");
         pragma Assert (Get_Sname_Kind (Name) = Sname_Artificial
                          and then Get_Sname_Prefix (Name) = No_Sname);
         Put_Id (Get_Sname_Suffix (Name));
      else
         Put_Name (Name);
      end if;

      if Get_Nbr_Params (Imod) /= 0 then
         Put_Line (" #(");
         for P in 1 .. Get_Nbr_Params (Inst) loop
            Param := Get_Param_Desc (Imod, P - 1);
            if P > 1 then
               Put_Line (",");
            end if;
            Put ("    .");
            Put_Interface_Name (Param.Name);
            Put ("(");
            case Param.Typ is
               when Param_Uns32 =>
                  Put_Uns32 (Get_Param_Uns32 (Inst, P - 1));
               when Param_Types_Pval =>
                  Disp_Pval (Get_Param_Pval (Inst, P - 1));
               when Param_Invalid =>
                  Put ("*invalid*");
            end case;
            Put (")");
         end loop;
         Put_Line (")");
         Put ("    ");
      else
         Put (" ");
      end if;

      --  Instance name
      Name := Get_Instance_Name (Inst);
      if Get_Sname_Kind (Name) = Sname_Version then
         Put ("inst_");
         Put_Name_Version (Name);
      else
         Put_Name (Name);
      end if;
      Put_Line (" (");

      First := True;
      --  Inputs
      Idx := 0;
      Max_Idx := Get_Nbr_Inputs (Imod);
      for I of Inputs (Inst) loop
         if First then
            First := False;
         else
            Put_Line (",");
         end if;
         Put ("    ");
         if Idx < Max_Idx then
            Put (".");
            Put_Interface_Name (Get_Input_Desc (Imod, Idx).Name);
            Put ("(");
         end if;
         Disp_Net_Name (Get_Driver (I));
         if Idx < Max_Idx then
            Put (")");
            Idx := Idx + 1;
         end if;
      end loop;
      --  Outputs
      Idx := 0;
      for O of Outputs (Inst) loop
         if First then
            First := False;
         else
            Put_Line (",");
         end if;
         Put ("    .");
         Put_Interface_Name (Get_Output_Desc (Imod, Idx).Name);
         Idx := Idx + 1;
         Put ("(");
         declare
            I : Input;
         begin
            I := Get_First_Sink (O);
            if I /= No_Input then
               Disp_Net_Name (O);
            end if;
         end;
         Put (")");
      end loop;
      Put_Line (");");
   end Disp_Instance_Gate;

   procedure Disp_Binary_Lit (Va : Uns32; Zx : Uns32; Wd : Width) is
   begin
      Put_Uns32 (Wd);
      Put ("'b");
      Disp_Binary_Digits (Va, Zx, Natural (Wd));
   end Disp_Binary_Lit;

   procedure Disp_Const_Bit (Inst : Instance)
   is
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      Nd : constant Width := W / 32;
      Ld : constant Natural := Natural (W mod 32);
   begin
      Put_Uns32 (W);
      Put ("'b");
      if Ld > 0 then
         Disp_Binary_Digits (Get_Param_Uns32 (Inst, Param_Idx (Nd)), 0, Ld);
      end if;
      for I in reverse 1 .. Nd loop
         Disp_Binary_Digits
           (Get_Param_Uns32 (Inst, Param_Idx (I - 1)), 0, 32);
      end loop;
   end Disp_Const_Bit;

   procedure Disp_Const_Log (Inst : Instance)
   is
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      Nd : constant Width := W / 32;
      Ld : constant Natural := Natural (W mod 32);
   begin
      Put ('"');
      if Ld > 0 then
         Disp_Binary_Digits (Get_Param_Uns32 (Inst, Param_Idx (2 * Nd)),
                             Get_Param_Uns32 (Inst, Param_Idx (2 * Nd + 1)),
                             Ld);
      end if;
      for I in reverse 1 .. Nd loop
         Disp_Binary_Digits
           (Get_Param_Uns32 (Inst, Param_Idx (2 * (I - 1))),
            Get_Param_Uns32 (Inst, Param_Idx (2 * (I - 1)) + 1),
            32);
      end loop;
      Put ('"');
   end Disp_Const_Log;

   procedure Disp_X_Lit (W : Width; C : Character) is
   begin
      Put_Uns32 (W);
      Put ("'b");
      Put (C);
   end Disp_X_Lit;

   procedure Disp_Extract (Inst : Instance);

   procedure Disp_Constant_Inline (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      O : constant Net := Get_Output (Inst, 0);
   begin
      case Get_Id (Imod) is
         when Id_Const_UB32
           | Id_Const_SB32 =>
            Disp_Binary_Lit (Get_Param_Uns32 (Inst, 0), 0,  Get_Width (O));
         when Id_Const_UL32 =>
            Disp_Binary_Lit (Get_Param_Uns32 (Inst, 0),
                             Get_Param_Uns32 (Inst, 1),
                             Get_Width (O));
         when Id_Const_Z =>
            Disp_X_Lit (Get_Width (O), 'Z');
         when Id_Const_X =>
            Disp_X_Lit (Get_Width (O), 'X');
         when Id_Const_Bit =>
            Disp_Const_Bit (Inst);
         when Id_Const_Log =>
            Disp_Const_Log (Inst);
         when Id_Extract =>
            Disp_Extract (Inst);
         when others =>
            raise Internal_Error;
      end case;
   end Disp_Constant_Inline;

   procedure Disp_Const_Bit (Inst : Instance; Off : Uns32)
   is
      Val : Uns32;
      Zx : Uns32;
   begin
      case Get_Id (Inst) is
         when Id_Const_Bit =>
            Zx := 0;
            Val := Get_Param_Uns32 (Inst, Param_Idx (Off / 32));
            Val := Shift_Right (Val, Natural (Off mod 32)) and 1;
         when Id_Const_Log =>
            Zx := Get_Param_Uns32 (Inst, 2 * Param_Idx (Off / 32) + 1);
            Zx := Shift_Right (Zx, Natural (Off mod 32)) and 1;
            Val := Get_Param_Uns32 (Inst, 2 * Param_Idx (Off / 32));
            Val := Shift_Right (Val, Natural (Off mod 32)) and 1;
         when Id_Const_UB32 =>
            Zx := 0;
            if Off < 32 then
               Val := Get_Param_Uns32 (Inst, 0);
               Val := Shift_Right (Val, Natural (Off mod 32)) and 1;
            else
               Val := 0;
            end if;
         when Id_Const_UL32 =>
            if Off < 32 then
               Val := Get_Param_Uns32 (Inst, 0);
               Val := Shift_Right (Val, Natural (Off mod 32)) and 1;
               Zx := Get_Param_Uns32 (Inst, 1);
               Zx := Shift_Right (Zx, Natural (Off mod 32)) and 1;
            else
               Val := 0;
               Zx := 0;
            end if;
         when Id_Const_X =>
            Zx := 1;
            Val := 1;
         when others =>
            raise Internal_Error;
      end case;
      Put (Bchar (Zx * 2 + Val));
   end Disp_Const_Bit;

   procedure Disp_Memory_Init
     (Mem : Instance; Val : Net; W : Width; Depth : Width)
   is
      Inst : constant Instance := Get_Net_Parent (Val);
   begin
      case Get_Id (Inst) is
         when Id_Const_X =>
            return;
         when others =>
            null;
      end case;

      Put_Line ("  initial begin");
      for I in reverse 0 .. Depth - 1 loop
         Put ("    ");
         Put_Name (Get_Instance_Name (Mem));
         Put ("[");
         Put_Uns32 (I);
         Put ("] = ");
         Put_Uns32 (W);
         Put ("'b");
         for J in reverse 0 .. W - 1 loop
            Disp_Const_Bit (Inst, I * W + J);
         end loop;
         Put_Line (";");
      end loop;
      Put_Line ("    end");
   end Disp_Memory_Init;

   function Need_Name (Inst : Instance) return Boolean
   is
      Id : constant Module_Id := Get_Id (Inst);
   begin
      case Id is
         when Id_Extract
           | Id_Dyn_Extract
           | Id_Dyn_Insert
           | Id_Utrunc
           | Id_Strunc =>
            return True;
         when Id_User_None .. Module_Id'Last =>
            return True;
         when others =>
            return False;
      end case;
   end Need_Name;

   --  Return True if constant INST is connected to an instance that needs
   --  a name.  In that case, a signal will be created and driven.
   function Need_Signal (Inst : Instance) return Boolean
   is
      I : Input;
   begin
      I := Get_First_Sink (Get_Output (Inst, 0));
      while I /= No_Input loop
         if Need_Name (Get_Input_Parent (I)) then
            return True;
         end if;
         I := Get_Next_Sink (I);
      end loop;
      return False;
   end Need_Signal;

   --  Return TRUE if edge INST (posedge or negedge) is used outside clock
   --  inputs.
   function Need_Edge (Inst : Instance) return Boolean
   is
      I : Input;
      Parent : Instance;
   begin
      I := Get_First_Sink (Get_Output (Inst, 0));
      while I /= No_Input loop
         Parent := Get_Input_Parent (I);
         case Get_Id (Parent) is
            when Id_Dff
              | Id_Adff
              | Id_Idff
              | Id_Iadff =>
               if I /= Get_Input (Parent, 0) then
                  return True;
               end if;
            when Id_Mem_Rd_Sync
              | Id_Mem_Wr_Sync =>
               if I /= Get_Input (Parent, 2) then
                  return True;
               end if;
            when others =>
               return True;
         end case;
         I := Get_Next_Sink (I);
      end loop;
      return False;
   end Need_Edge;

   type Conv_Type is
     (Conv_None, Conv_Unsigned, Conv_Signed, Conv_Edge, Conv_Clock);

   procedure Disp_Net_Expr (N : Net; Inst : Instance; Conv : Conv_Type)
   is
      Net_Inst : Instance;
   begin
      if N = No_Net then
         Put ("<unassigned>");
         return;
      end if;

      Net_Inst := Get_Net_Parent (N);
      if Flag_Merge_Lit
        and then Get_Id (Net_Inst) in Constant_Module_Id
        and then not Need_Name (Inst)
      then
         case Conv is
            when Conv_None =>
               Disp_Constant_Inline (Net_Inst);
            when Conv_Unsigned =>
               Put ("$unsigned(");
               Disp_Constant_Inline (Net_Inst);
               Put (")");
            when Conv_Signed =>
               Put ("$signed(");
               Disp_Constant_Inline (Net_Inst);
               Put (")");
            when Conv_Edge
              | Conv_Clock =>
               --  Not expected: a constant is not an edge.
               raise Internal_Error;
         end case;
      else
         case Conv is
            when Conv_None =>
               Disp_Net_Name (N);
            when Conv_Edge =>
               case Edge_Module_Id (Get_Id (Net_Inst)) is
                  when Id_Posedge =>
                     Put ("posedge ");
                  when Id_Negedge =>
                     Put ("negedge ");
               end case;
               Disp_Net_Name (Get_Input_Net (Net_Inst, 0));
            when Conv_Clock =>
               Disp_Net_Name (Get_Input_Net (Net_Inst, 0));
            when Conv_Unsigned =>
               Put ("$unsigned(");
               Disp_Net_Name (N);
               Put (")");
            when Conv_Signed =>
               Put ("$signed(");
               Disp_Net_Name (N);
               Put (")");
         end case;
      end if;
   end Disp_Net_Expr;

   NL : constant Character := ASCII.LF;

   type Uns32_Array is array (Natural range <>) of Uns32;
   No_Uns32_Arr : constant Uns32_Array := (1 .. 0 => 0);

   --  Template:
   --  \[C]AN
   --   C: conversion  u: unsigned, s: signed, f: force logic
   --   A: argument    o: output, i: input, n: value, p: parameter, l: label
   --   N: argument number (0-9)
   procedure Disp_Template
     (S : String; Inst : Instance; Val : Uns32_Array := No_Uns32_Arr)
   is
      I : Positive;
      C : Character;
      Idx : Natural;
      N : Net;
      Conv : Conv_Type;
      V : Uns32;
   begin
      I := S'First;
      while I <= S'Last loop
         C := S (I);
         --  Escape character.
         if C = '\' then
            I := I + 1;
            --  Conversion (optional).
            case S (I) is
               when 'u' =>
                  Conv := Conv_Unsigned;
                  I := I + 1;
               when 's' =>
                  Conv := Conv_Signed;
                  I := I + 1;
               when 'e' =>
                  Conv := Conv_Edge;
                  I := I + 1;
               when 'c' =>
                  Conv := Conv_Clock;
                  I := I + 1;
               when others =>
                  Conv := Conv_None;
            end case;
            Idx := Character'Pos (S (I + 1)) - Character'Pos ('0');
            case S (I) is
               when 'o' =>
                  pragma Assert (Conv = Conv_None);
                  N := Get_Output (Inst, Port_Idx (Idx));
                  Disp_Net_Name (N);
               when 'i' =>
                  N := Get_Input_Net (Inst, Port_Idx (Idx));
                  Disp_Net_Expr (N, Inst, Conv);
               when 'n' =>
                  V := Val (Idx);
                  Put_Uns32 (V);
               when 'p' =>
                  V := Get_Param_Uns32 (Inst, Param_Idx (Idx));
                  case Conv is
                     when Conv_None
                       | Conv_Unsigned =>
                        Put_Uns32 (V);
                     when Conv_Signed =>
                        Put_Int32 (To_Int32 (V));
                     when Conv_Edge
                       | Conv_Clock =>
                        raise Internal_Error;
                  end case;
               when 'l' =>
                  pragma Assert (Idx = 0);
                  pragma Assert (Conv = Conv_None);
                  Put_Name (Get_Instance_Name (Inst));
               when others =>
                  raise Internal_Error;
            end case;

            I := I + 2;
         else
            Put (C);
            I := I + 1;
         end if;
      end loop;
   end Disp_Template;

   procedure Disp_Extract (Inst : Instance)
   is
      O : constant Net := Get_Output (Inst, 0);
      I : constant Net := Get_Input_Net (Inst, 0);
      Wd : constant Width := Get_Width (O);
      Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
   begin
      Disp_Template ("\i0", Inst);
      if Get_Width (I) > 1 then
         --  If width is 1, the signal is declared as a scalar and
         --  therefore cannot be indexed.
         if Wd > 1 then
            Disp_Template ("[\n0:\n1]", Inst,
                           (0 => Off + Wd - 1, 1 => Off));
         elsif Wd = 1 then
            Disp_Template ("[\n0]", Inst, (0 => Off));
         else
            Disp_Template (" (-1 downto 0)", Inst);
         end if;
      end if;
   end Disp_Extract;

   procedure Disp_Memory (Mem : Instance)
   is
      Ports : constant Net := Get_Output (Mem, 0);
      Port : Net;
      Port_Inst : Instance;
      S : Net;
      Data_W : Width;
      Depth : Uns32;
   begin
      --  Display a process, with as sensitivity elements:
      --    * write clocks
      --    * read address
      --  As statements:
      Data_W := 0;
      Port := Ports;
      loop
         Port_Inst := Get_Input_Parent (Get_First_Sink (Port));
         case Get_Id (Port_Inst) is
            when Id_Mem_Wr_Sync =>
               --  Clock
               S := Get_Input_Net (Port_Inst, 2);
               --  Strip the edge.
               S := Get_Input_Net (Get_Net_Parent (S), 0);
               Data_W := Get_Width (Get_Input_Net (Port_Inst, 4));
            when Id_Mem_Rd =>
               --  Address
               S := Get_Input_Net (Port_Inst, 1);
               Data_W := Get_Width (Get_Output (Port_Inst, 1));
            when Id_Mem_Rd_Sync =>
               --  Clock
               S := Get_Input_Net (Port_Inst, 2);
               --  Strip the edge.
               S := Get_Input_Net (Get_Net_Parent (S), 0);
               Data_W := Get_Width (Get_Output (Port_Inst, 1));
            when Id_Memory
              | Id_Memory_Init =>
               exit;
            when others =>
               raise Internal_Error;
         end case;
         Port := Get_Output (Port_Inst, 0);
      end loop;

      Depth := Get_Width (Ports) / Data_W;

      --  Declare the memory.
      Put ("  reg ");
      Put_Type (Data_W);
      Put_Name (Get_Instance_Name (Mem));
      Put_Type (Depth);
      Put_Line("; // memory");

      --  Initialization
      if Get_Id (Mem) = Id_Memory_Init then
         declare
            Val : Net;
            Val_Inst : Instance;
         begin
            Val := Get_Input_Net (Mem, 1);
            Val_Inst := Get_Net_Parent (Val);
            if Get_Id (Val_Inst) = Id_Isignal then
               Val := Get_Input_Net (Val_Inst, 1);
            end if;
            Disp_Memory_Init (Mem, Val, Data_W, Depth);
         end;
      end if;

      Port := Ports;
      loop
         Port_Inst := Get_Input_Parent (Get_First_Sink (Port));
         case Get_Id (Port_Inst) is
            when Id_Mem_Wr_Sync =>
               Disp_Template
                 ("  always @(\ei2)" & NL &
                  "    if (\i3)" & NL, Port_Inst);
               Disp_Template
                 ("      \l0", Mem);
               Disp_Template ("[\i1] <= \i4;" & NL, Port_Inst);
            when Id_Mem_Rd =>
               Disp_Template ("  assign \o1 = ", Port_Inst);
               Disp_Template ("\l0", Mem);
               Disp_Template ("[\i1];" & NL, Port_Inst);
            when Id_Mem_Rd_Sync =>
               Disp_Template
                 ("  always @(\ei2)" & NL &
                  "    if (\i3)" & NL &
                  "      \o1 <= ", Port_Inst);
               Disp_Template ("\l0", Mem);
               Disp_Template ("[\i1];" & NL, Port_Inst);
            when Id_Memory
              | Id_Memory_Init =>
               exit;
            when others =>
               raise Internal_Error;
         end case;
         Port := Get_Output (Port_Inst, 0);
      end loop;
   end Disp_Memory;

   procedure Disp_Pmux (Inst : Instance)
   is
      Def : constant Net := Get_Input_Net (Inst, 0);
      W : constant Width := Get_Width (Def);
   begin
      Disp_Template ("  always @*" & NL &
                       "    case (\i0)" & NL, Inst);
      for I in 1 .. W loop
         Put ("      ");
         --  One hot encoding.
         Put_Uns32 (W);
         Put ("'b");
         for J in 1 .. W loop
            if I = J then
               Put ('1');
            else
               Put ('0');
            end if;
         end loop;
         Disp_Template (": \o0 <= ", Inst);
         Disp_Net_Expr
           (Get_Input_Net (Inst, Port_Idx (2 + W - I)), Inst, Conv_None);
         Put_Line (";");
      end loop;
      Disp_Template ("      default: \o0 <= \i1;" & NL, Inst);
      Disp_Template ("    endcase" & NL, Inst);
   end Disp_Pmux;

   procedure Disp_Instance_Inline (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      Loc : constant Location_Type := Locations.Get_Location (Inst);
      Id : constant Module_Id := Get_Id (Imod);
   begin
      if Loc /= No_Location then
         declare
            File : Name_Id;
            Line : Positive;
            Col : Natural;
         begin
            Files_Map.Location_To_Position (Loc, File, Line, Col);
            Put ("  /* ");
            Put_Id (File);
            Put (':');
            Put_Uns32 (Uns32 (Line));
            Put (':');
            Put_Uns32 (Uns32 (Col));
            Put ("  */");
            New_Line;
         end;
      end if;
      case Id is
         when Id_Memory
           |  Id_Memory_Init =>
            Disp_Memory (Inst);
         when Id_Mem_Rd
           | Id_Mem_Rd_Sync
           | Id_Mem_Wr_Sync =>
            null;
         when Id_Output =>
            Disp_Template ("  assign \o0 = \i0; // (output)" & NL, Inst);
         when Id_Inout
            | Id_Iinout =>
            --  Gates inout are special: output 1 must be connected to an
            --  output (with the is_inout flag set) of the module.
            Disp_Template ("  assign \o1 = \i0; // (inout - port)" & NL, Inst);
            Disp_Template ("  assign \o0 = ", Inst);
            declare
               Inp : constant Input := Get_First_Sink (Get_Output (Inst, 1));
               Iinst : constant Instance := Get_Input_Parent (Inp);
            begin
               Put_Name (Get_Output_Name (Get_Module (Iinst),
                                          Get_Port_Idx (Inp)));
            end;
            Put ("; // (inout - read)" & NL);
         when Id_Signal =>
            Disp_Template ("  assign \o0 = \i0; // (signal)" & NL, Inst);
         when Id_Isignal =>
            declare
               Inet : constant Net := Get_Input_Net (Inst, 0);
            begin
               if Inet /= No_Net then
                  Disp_Template ("  always @*" & NL &
                                 "    \o0 = \i0; // (isignal)" & NL, Inst);
               end if;
               Disp_Template ("  initial" & NL &
                              "    \o0 <= \i1;" & NL, Inst);
            end;
         when Id_Port =>
            Disp_Template ("  \o0 <= \i0; -- (port)" & NL, Inst);
         when Id_Nop =>
            Disp_Template ("  \o0 <= \i0; -- (nop)" & NL, Inst);
         when Id_Enable =>
            Disp_Template ("  \o0 <= \i0; -- (enable)" & NL, Inst);
         when Id_Not =>
            Disp_Template ("  assign \o0 = ~\i0;" & NL, Inst);
         when Id_Neg =>
            Disp_Template ("  assign \o0 = -\i0;" & NL, Inst);
         when Id_Abs=>
            Disp_Template ("  \o0 <= std_logic_vector(abs \si0);" & NL, Inst);
         when Id_Extract =>
            Disp_Template ("  assign \o0 = ", Inst);
            Disp_Extract (Inst);
            Put_Line (";");
         when Id_Memidx =>
            declare
               Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
            begin
               Disp_Template
                 ("  assign \o0 = \i0 * \p0;" & NL, Inst, (0 => Step));
            end;
         when Id_Addidx =>
            declare
               W0 : constant Width := Get_Width (Get_Input_Net (Inst, 0));
               W1 : constant Width := Get_Width (Get_Input_Net (Inst, 1));
            begin
               if W0 > W1 then
                  Disp_Template
                    ("  \o0 <= std_logic_vector (\ui0 + resize(\ui1, \n0));"
                       & NL, Inst, (0 => W0));
               elsif W0 < W1 then
                  Disp_Template
                    ("  \o0 <= std_logic_vector (resize (\ui0, \n0) + \ui1);"
                       & NL, Inst, (0 => W1));
               else
                  pragma Assert (W0 = W1);
                  Disp_Template
                    ("  \o0 <= std_logic_vector (\ui0 + \ui1);"
                       & NL, Inst);
               end if;
            end;
         when Id_Dyn_Extract =>
            declare
               O : constant Net := Get_Output (Inst, 0);
               Wd : constant Width := Get_Width (O);
               Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
            begin
               Disp_Template
                 ("  assign \o0 = \i0[\i1 + \n0 -: \n1]; //(dyn_extract)" & NL,
                  Inst, (0 => Off, 1 => Wd));
            end;
         when Id_Dyn_Insert
           | Id_Dyn_Insert_En =>
            declare
               --  I0: Input, I1: Value, I2: position
               --  P0: offset
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 1));
            begin
               Put ("  always @* begin // (dyn_insert)" & NL);
               Disp_Template ("    \o0 <= \i0;" & NL, Inst);
               if Id = Id_Dyn_Insert_En then
                  --  TODO: fix indentation.
                  Disp_Template ("    if (\i3)" & NL, Inst);
               end if;
               Disp_Template
                 ("    \o0 [\i2 + \p0 -: \n0] <= \i1;" & NL,
                  Inst, (0 => Iw - 1));
               Disp_Template ("  end" & NL, Inst);
            end;
         when Id_Const_UB32
           | Id_Const_UL32
           | Id_Const_Z
           | Id_Const_X =>
            Disp_Template ("  \o0 <= ", Inst);
            Disp_Constant_Inline (Inst);
            Put_Line (";");
         when Id_Const_Bit =>
            null;
         when Id_Adff
           | Id_Iadff =>
            Disp_Template ("  always @(\ei0 or posedge \i2)" & NL &
                           "    if (\i2)" & NL &
                           "      \o0 <= \i3;" & NL &
                           "    else" & NL &
                           "      \o0 <= \i1;" & NL, Inst);
         when Id_Dff
            | Id_Idff =>
            Disp_Template ("  always @(\ei0)" & NL &
                             "    \o0 <= \i1;" & NL, Inst);
            if Id = Id_Idff then
               Disp_Template ("  initial" & NL &
                              "    \o0 <= \i2;" & NL, Inst);
            end if;
         when Id_Mux2 =>
            Disp_Template ("  assign \o0 = \i0 ? \i2 : \i1;" & NL, Inst);
         when Id_Mux4 =>
            Disp_Template ("  always @*" & NL &
                           "    case (\i0)" & NL &
                           "      2'b00: \o0 <= \i1;" & NL &
                           "      2'b01: \o0 <= \i2;" & NL &
                           "      2'b10: \o0 <= \i3;" & NL &
                           "      2'b11: \o0 <= \i4;" & NL &
                           "    endcase" & NL, Inst);
         when Id_Pmux =>
            Disp_Pmux (Inst);
         when Id_Add =>
            Disp_Template ("  assign \o0 = \i0 + \i1;" & NL, Inst);
         when Id_Sub =>
            Disp_Template ("  assign \o0 = \i0 - \i1;" & NL, Inst);
         when Id_Umin =>
            Disp_Template ("  assign \o0 = (\i0 < \i1) ? \i0 : \i1;" & NL,
                           Inst);
         when Id_Smin =>
            Disp_Template ("  \o0 <= \i0 when \si0 < \si1 else \i1;" & NL,
                           Inst);
         when Id_Umax =>
            Disp_Template ("  assign \o0 = (\i0 > \i1) ? \i0 : \i1;" & NL,
                           Inst);
         when Id_Smax =>
            Disp_Template ("  \o0 <= \i0 when \si0 > \si1 else \i1;" & NL,
                           Inst);
         when Id_Umul =>
            Disp_Template ("  assign \o0 = \i0 * \i1; // umul" & NL, Inst);
         when Id_Smul =>
            Disp_Template ("  assign \o0 = \i0 * \i1; // smul" & NL, Inst);
         when Id_Smod =>
            Disp_Template ("  assign \o0 = \i0 % \i1; // smod" & NL, Inst);
         when Id_Srem =>
            Disp_Template ("  assign \o0 = \i0 % \i1; // srem" & NL, Inst);
         when Id_Umod =>
            Disp_Template ("  assign \o0 = \i0 % \i1; // umod" & NL, Inst);
         when Id_Sdiv =>
            Disp_Template ("  assign \o0 = \i0 / \i1; // sdiv" & NL, Inst);
         when Id_Udiv =>
            Disp_Template ("  assign \o0 = \i0 / \i1; // udiv" & NL, Inst);
         when Id_Lsl =>
            Disp_Template ("  assign \o0 = \i0 << \i1;" & NL, Inst);
         when Id_Lsr =>
            Disp_Template ("  assign \o0 = \i0 >> \i1;" & NL, Inst);
         when Id_Asr =>
            Disp_Template ("  assign \o0 = \si0 >> \i1;" & NL, Inst);
         when Id_Rol =>
            Disp_Template
              ("  \o0 <= std_logic_vector "
                 & "(rotate_left (\ui0, to_integer (\ui1)));" & NL, Inst);

         when Id_Ult =>
            Disp_Template ("  assign \o0 = \ui0 < \ui1;" & NL, Inst);
         when Id_Ule =>
            Disp_Template ("  assign \o0 = \ui0 <= \ui1;" & NL, Inst);
         when Id_Ugt =>
            Disp_Template ("  assign \o0 = \ui0 > \ui1;" & NL, Inst);
         when Id_Uge =>
            Disp_Template ("  assign \o0 = \ui0 >= \ui1;" & NL, Inst);
         when Id_Slt =>
            Disp_Template ("  assign \o0 = \si0 < \si1;" & NL, Inst);
         when Id_Sle =>
            Disp_Template ("  assign \o0 = \si0 <= \si1;" & NL, Inst);
         when Id_Sgt =>
            Disp_Template ("  assign \o0 = \si0 > \si1;" & NL, Inst);
         when Id_Sge =>
            Disp_Template ("  assign \o0 = \si0 >= \si1;" & NL, Inst);
         when Id_Eq =>
            Disp_Template ("  assign \o0 = \i0 == \i1;" & NL, Inst);
         when Id_Ne =>
            Disp_Template ("  assign \o0 = \i0 != \i1;" & NL, Inst);
         when Id_Or =>
            Disp_Template ("  assign \o0 = \i0 | \i1;" & NL, Inst);
         when Id_And =>
            Disp_Template ("  assign \o0 = \i0 & \i1;" & NL, Inst);
         when Id_Xor =>
            Disp_Template ("  assign \o0 = \i0 ^ \i1;" & NL, Inst);
         when Id_Nor =>
            Disp_Template ("  assign \o0 = ~(\i0 | \i1);" & NL, Inst);
         when Id_Nand =>
            Disp_Template ("  assign \o0 = ~(\i0 & \i1);" & NL, Inst);
         when Id_Xnor =>
            Disp_Template ("  assign \o0 = ~(\i0 ^ \i1);" & NL, Inst);

         when Id_Concat2 =>
            Disp_Template ("  assign \o0 = {\i0, \i1};" & NL, Inst);
         when Id_Concat3 =>
            Disp_Template ("  assign \o0 = {\i0, \i1, \i2};" & NL, Inst);
         when Id_Concat4 =>
            Disp_Template ("  assign \o0 = {\i0, \i1, \i2, \i3};" & NL, Inst);
         when Id_Concatn =>
            Disp_Template ("  assign \o0 = {\i0", Inst);
            for I in 1 .. Get_Nbr_Inputs (Inst) - 1 loop
               Disp_Template (", ", Inst);
               Disp_Net_Expr (Get_Input_Net (Inst, I), Inst, Conv_None);
            end loop;
            Disp_Template("};" & NL, Inst);
         when Id_Utrunc
           | Id_Strunc =>
            declare
               W : constant Width := Get_Width (Get_Output (Inst, 0));
            begin
               if W = 0 then
                  --  Do not try to slice the input, as it can be a single
                  --  wire.
                  Disp_Template ("  assign \o0 = """"", Inst);
               else
                  Disp_Template ("  assign \o0 = \i0", Inst);
                  if W = 1 then
                     Disp_Template ("[0]", Inst);
                  else
                     Disp_Template ("[\n0:0]", Inst, (0 => W - 1));
                  end if;
               end if;
               Disp_Template (";  // trunc" & NL, Inst);
            end;
         when Id_Uextend =>
            declare
               Ow : constant Width := Get_Width (Get_Output (Inst, 0));
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 0));
            begin
               pragma Assert (Ow > Iw);
               Disp_Template ("  assign \o0 = {", Inst);
               Put_Uns32 (Ow - Iw);
               Disp_Template ("'b0, \i0};  //  uext" & NL, Inst);
            end;
         when Id_Sextend =>
            declare
               Ow : constant Width := Get_Width (Get_Output (Inst, 0));
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 0));
            begin
               pragma Assert (Ow > Iw);
               Disp_Template ("  assign \o0 = {{\n0{\i0",
                              Inst, (0 => Ow - Iw));
               if Iw > 1 then
                  Disp_Template ("[\n0]", Inst, (0 => Iw - 1));
               end if;
               Disp_Template ("}}, \i0}; // sext" & NL, Inst);
            end;
         when Id_Red_Or =>
            Disp_Template ("  assign \o0 = |(\i0);" & NL, Inst);
         when Id_Red_And =>
            Disp_Template ("  assign \o0 = &(\i0);" & NL, Inst);
         when Id_Red_Xor =>
            Disp_Template ("  assign \o0 = ^(\i0);" & NL, Inst);

         when Id_Posedge =>
            Disp_Template ("  assign \o0 = 1'b0; // posedge" & NL, Inst);
         when Id_Negedge =>
            Disp_Template ("  assign \o0 = 1'b0; // negedge" & NL, Inst);
         when Id_Tri =>
            Disp_Template ("  assign \o0 = \i0 ? \i1 : \n0'bz;" & NL,
                           Inst, (0 => Get_Width (Get_Output (Inst, 0))));
         when Id_Assert =>
            Disp_Template
              ("  always @*" & NL &
               "    if (!\i0)" & NL &
               "      $fatal(1, ""assertion failure \l0"");" & NL, Inst);
         when Id_Assume =>
            Disp_Template
              ("  \l0: assert \i0 = '1' severity warning; --  assume" & NL,
               Inst);
         when Id_Cover =>
            Disp_Template
              ("  \l0: assert \i0 = '1' severity note; --  cover" & NL,
               Inst);
         when Id_Assert_Cover =>
            Disp_Template
              ("  always @*" & NL &
               "    if (!\i0)" & NL &
               "      $fatal(1, ""assertion(cover) failure \l0"");" & NL,
               Inst);
         when Id_Resolver =>
            Disp_Template
              ("  assign \o0 = \i0;" & NL, Inst);
            Disp_Template
              ("  assign \o0 = \i1;" & NL, Inst);
         when others =>
            Disp_Instance_Gate (Inst);
      end case;
   end Disp_Instance_Inline;

   procedure Disp_Module_Declarations (M : Module)
   is
      Id : Module_Id;
   begin
      for Inst of Instances (M) loop
         Id := Get_Id (Inst);
         case Id is
            when Id_Memory
              | Id_Memory_Init =>
               --  For memories: skip the chain.
               null;
            when Id_Mem_Wr_Sync =>
               --  For memories: skip the chain.
               null;
            when Id_Mem_Rd
              | Id_Mem_Rd_Sync =>
               --  For memories: skip the chain.
               declare
                  N : constant Net := Get_Output (Inst, 1);
               begin
                  if Id = Id_Mem_Rd_Sync then
                     Put ("  reg ");
                  else
                     Put ("  wire ");
                  end if;
                  Put_Type (Get_Width (N));
                  Disp_Net_Name (N);
                  Put_Line ("; // mem_rd");
               end;
            when others =>
               if Is_Self_Instance (Inst)
                 or else (Flag_Merge_Lit
                            and then Id in Constant_Module_Id
                            and then Id < Id_User_None
                            and then not Need_Signal (Inst))
                 or else (Flag_Merge_Edge
                            and then Id in Edge_Module_Id
                            and then not Need_Edge (Inst))
               then
                  --  Not displayed.
                  null;
               else
                  --  Check location is present.
                  if Locations.Get_Location (Inst) = No_Location then
                     case Id is
                        when Id_Const_UB32
                           | Id_Const_SB32
                           | Id_Const_UL32
                           | Id_Const_Bit
                           | Id_Const_Log
                           | Id_Const_Z
                           | Id_Const_X
                           | Id_Const_0
                           | Id_Concat2
                           | Id_Concat3
                           | Id_Concat4
                           | Id_Concatn
                           | Id_Extract =>
                           null;
                        when others =>
                           raise Internal_Error;
                     end case;
                  end if;

                  --  Display reg/wire for each output.
                  for N of Outputs (Inst) loop
                     case Id is
                        when Id_Dff
                          | Id_Idff
                          | Id_Adff
                          | Id_Iadff
                          | Id_Isignal =>
                           --  As expected
                           Put ("  reg ");
                        when Id_Mux4
                           | Id_Pmux
                           | Id_Dyn_Insert
                           | Id_Dyn_Insert_En =>
                           --  Implemented by a process
                           Put ("  reg ");
                        when Constant_Module_Id =>
                           Put ("  localparam ");
                        when others =>
                           Put ("  wire ");
                     end case;
                     Put_Type (Get_Width (N));
                     Disp_Net_Name (N);
                     if Id in Constant_Module_Id then
                        Put (" = ");
                        Disp_Constant_Inline (Inst);
                     end if;
                     Put_Line (";");
                  end loop;
               end if;
         end case;
      end loop;
   end Disp_Module_Declarations;

   procedure Disp_Module_Statements (M : Module)
   is
      Self_Inst : constant Instance := Get_Self_Instance (M);
   begin
      --  Output assignments.
      declare
         Idx : Port_Idx;
      begin
         Idx := 0;
         for I of Inputs (Self_Inst) loop
            Put ("  assign ");
            Put_Name (Get_Output_Desc (M, Idx).Name);
            Put (" = ");
            Disp_Net_Name (Get_Driver (I));
            Put_Line (";");
            Idx := Idx + 1;
         end loop;
      end;

      for Inst of Instances (M) loop
         case Get_Id (Inst) is
            when Constant_Module_Id =>
               if not Flag_Merge_Lit then
                  Disp_Instance_Inline (Inst);
               end if;
            when Edge_Module_Id =>
               if (not Flag_Merge_Edge) or else Need_Edge (Inst) then
                  Disp_Instance_Inline (Inst);
               end if;
            when others =>
               Disp_Instance_Inline (Inst);
         end case;
      end loop;
   end Disp_Module_Statements;

   procedure Disp_Module_Port (Desc : Port_Desc;
                               Attrs : Attribute;
                               First : in out Boolean)
   is
      Attr : Attribute;
   begin
      if First then
         Put ("  (");
         First := False;
      else
         Put_Line (",");
         Put ("   ");
      end if;

      if Attrs /= No_Attribute then
         Put ("(* ");
         Attr := Attrs;
         loop
            Put_Id (Get_Attribute_Name (Attr));
            Put ('=');
            Disp_Pval (Get_Attribute_Pval (Attr));
            Attr := Get_Attribute_Next (Attr);
            exit when Attr = No_Attribute;
            Put (", ");
         end loop;
         Put (" *) ");
      end if;

      case Desc.Dir is
         when Port_In =>
            Put ("input  ");
         when Port_Out =>
            Put ("output ");
         when Port_Inout =>
            Put ("inout  ");
      end case;
      Put_Type (Desc.W);
      Put_Name (Desc.Name);
   end Disp_Module_Port;

   procedure Disp_Module_Ports (M : Module)
   is
      First : Boolean;
      Desc : Port_Desc;
      Attr : Attribute;
   begin
      First := True;
      for I in 1 .. Get_Nbr_Inputs (M) loop
         Desc := Get_Input_Desc (M, I - 1);
         Attr := Get_Input_Port_First_Attribute (M, I - 1);
         Disp_Module_Port (Desc, Attr, First);
      end loop;
      for I in 1 .. Get_Nbr_Outputs (M) loop
         Desc := Get_Output_Desc (M, I - 1);
         Attr := Get_Output_Port_First_Attribute (M, I - 1);
         Disp_Module_Port (Desc, Attr, First);
      end loop;
      if not First then
         Put (")");
      end if;
      Put_Line (";");
   end Disp_Module_Ports;

   procedure Disp_Module_Parameters (M : Module)
   is
      Nbr : constant Param_Nbr := Get_Nbr_Params (M);
      Desc : Param_Desc;
   begin
      if Nbr = 0 then
         return;
      end if;
      for I in 1 .. Nbr loop
         if I = 1 then
            Put_Line ("  #(parameter");
         else
            Put_Line (",");
         end if;
         Desc := Get_Param_Desc (M, I - 1);
         Put ("    ");
         Put_Name (Desc.Name);
      end loop;
      Put_Line (")");
   end Disp_Module_Parameters;

   procedure Disp_Verilog_Module (M : Module)
   is
      Self_Inst : constant Instance := Get_Self_Instance (M);
   begin
      --  Module id and name.
      Put ("module ");
      Put_Name (Get_Module_Name (M));
      New_Line;

      Disp_Module_Parameters (M);

      Disp_Module_Ports (M);

      if Self_Inst /= No_Instance then
         Disp_Module_Declarations (M);
         Disp_Module_Statements (M);
      end if;
      Put_Line ("endmodule");
      New_Line;
   end Disp_Verilog_Module;

   procedure Disp_Verilog (M : Module; Is_Top : Boolean) is
   begin
      --  Disp in reverse order.
      declare
         Num : Natural;
      begin
         Num := 0;
         for S of Sub_Modules (M) loop
            if Get_Id (S) >= Id_User_None then
               Num := Num + 1;
            end if;
         end loop;

         declare
            type Module_Array is array (1 .. Num) of Module;
            Modules : Module_Array;
         begin
            Num := 0;
            for S of Sub_Modules (M) loop
               if Get_Id (S) >= Id_User_None then
                  Num := Num + 1;
                  Modules (Num) := S;
               end if;
            end loop;

            for I in reverse Modules'Range loop
               Disp_Verilog (Modules (I), False);
            end loop;
         end;
      end;

      if not Is_Top then
         Disp_Verilog_Module (M);
      end if;
   end Disp_Verilog;

   procedure Disp_Verilog (M : Module) is
   begin
      Disp_Verilog (M, True);
   end Disp_Verilog;
end Netlists.Disp_Verilog;
