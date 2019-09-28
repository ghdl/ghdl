--  Routine to dump (for debugging purpose) a netlist.
--  Copyright (C) 2017 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Types_Utils; use Types_Utils;
with Name_Table; use Name_Table;
with Files_Map;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Locations;

package body Netlists.Disp_Vhdl is
   Flag_Merge_Lit : constant Boolean := True;

   --  Like Put, but without the leading space (if any).
   procedure Put_Trim (S : String) is
   begin
      if S'First <= S'Last and then S (S'First) = ' ' then
         Put (S (S'First + 1 .. S'Last));
      else
         Put (S);
      end if;
   end Put_Trim;

   procedure Put_Uns32 (V : Uns32) is
   begin
      Put_Trim (Uns32'Image (V));
   end Put_Uns32;

   procedure Put_Int32 (V : Int32) is
   begin
      Put_Trim (Int32'Image (V));
   end Put_Int32;

   procedure Put_Type (W : Width) is
   begin
      if W = 1 then
         Put ("std_logic");
      else
         Put ("std_logic_vector (");
         if W = 0 then
            Put ("-1");
         else
            Put_Uns32 (W - 1);
         end if;
         Put (" downto 0)");
      end if;
   end Put_Type;

   procedure Put_Id (N : Name_Id) is
   begin
      Put (Name_Table.Image (N));
   end Put_Id;

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
            Put (Image (Get_Sname_Suffix (N)));
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
         Put (Name_Table.Image (Get_Sname_Suffix (N)));
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

      if Get_Sname_Kind (N) in Sname_User .. Sname_Artificial
        and then Get_Sname_Prefix (N) = No_Sname
      then
         Put (Name_Table.Image (Get_Sname_Suffix (N)));
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
         Inst : constant Instance := Get_Parent (N);
         Idx : constant Port_Idx := Get_Port_Idx (N);
         Inst_Name : Sname;
         Port_Name : Sname;
      begin
         if Is_Self_Instance (Inst) then
            Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
         else
            Inst_Name := Get_Name (Inst);
            Put_Name (Inst_Name);
            case Get_Sname_Kind (Inst_Name) is
               when Sname_Version =>
                  Port_Name := Get_Output_Desc (Get_Module (Inst), Idx).Name;
                  Put ("_");
                  Put_Interface_Name (Port_Name);
               when Sname_User =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
      end;
   end Disp_Net_Name;

   procedure Disp_Instance_Gate (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      Idx : Port_Idx;
      Max_Idx : Port_Idx;
      P_Idx : Param_Idx;
      Name : Sname;
      First : Boolean;
   begin
      Put ("  ");
      Name := Get_Name (Inst);
      if Get_Sname_Kind (Name) = Sname_Version then
         Put ("inst_");
         Put_Name_Version (Name);
      else
         Put_Name (Name);
      end if;
      --  Gate name
      Name := Get_Name (Imod);
      if Get_Id (Imod) < Id_User_None then
         Put (" : gsynth.gate_");
         pragma Assert (Get_Sname_Kind (Name) = Sname_Artificial
                          and then Get_Sname_Prefix (Name) = No_Sname);
         Put_Id (Get_Sname_Suffix (Name));
      else
         Put (" : entity work.");
         Put_Name (Name);
      end if;

      if Get_Nbr_Params (Imod) /= 0 then
         Put_Line (" generic map (");
         First := True;
         Idx := 0;
         for P in Params (Inst) loop
            if First then
               First := False;
            else
               Put_Line (",");
            end if;
            Put ("    ");
            P_Idx := Get_Param_Idx (P);
            Put_Interface_Name (Get_Param_Desc (Imod, P_Idx).Name);
            Put (" => ");
            Put_Uns32 (Get_Param_Uns32 (Inst, P_Idx));
         end loop;
         Put_Line (")");
         Put_Line ("    port map (");
      else
         Put_Line (" port map (");
      end if;

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
            Put_Interface_Name (Get_Input_Desc (Imod, Idx).Name);
            Idx := Idx + 1;
            Put (" => ");
         end if;
         Disp_Net_Name (Get_Driver (I));
      end loop;
      --  Outputs
      Idx := 0;
      for O of Outputs (Inst) loop
         if First then
            First := False;
         else
            Put_Line (",");
         end if;
         Put ("    ");
         Put_Interface_Name (Get_Output_Desc (Imod, Idx).Name);
         Idx := Idx + 1;
         Put (" => ");
         declare
            I : Input;
            O_Inst : Instance;
         begin
            I := Get_First_Sink (O);
            if I /= No_Input then
               O_Inst := Get_Parent (I);
            else
               O_Inst := No_Instance;
            end if;
            if O_Inst /= No_Instance
              and then Get_Id (O_Inst) = Id_Port
            then
               Disp_Net_Name (Get_Output (O_Inst, 0));
            else
               Disp_Net_Name (O);
            end if;
         end;
      end loop;
      Put_Line (");");
   end Disp_Instance_Gate;

   Bchar : constant array (Uns32 range 0 .. 3) of Character := "01ZX";

   function Get_Lit_Quote (Wd : Width) return Character is
   begin
      if Wd = 1 then
         return ''';
      else
         return '"';
      end if;
   end Get_Lit_Quote;

   procedure Disp_Binary_Digits (Va : Uns32; Zx : Uns32; W : Natural) is
   begin
      for I in 1 .. W loop
         Put (Bchar (((Va / 2**(W - I)) and 1)
                     + ((Zx / 2**(W - I)) and 1) * 2));
      end loop;
   end Disp_Binary_Digits;

   procedure Disp_Binary_Lit (Va : Uns32; Zx : Uns32; Wd : Width)
   is
      Q : constant Character := Get_Lit_Quote (Wd);
   begin
      Put (Q);
      Disp_Binary_Digits (Va, Zx, Natural (Wd));
      Put (Q);
   end Disp_Binary_Lit;

   procedure Disp_Const_Bit (Inst : Instance)
   is
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      Nd : constant Width := W / 32;
      Ld : constant Natural := Natural (W mod 32);
   begin
      Put ('"');
      if Ld > 0 then
         Disp_Binary_Digits (Get_Param_Uns32 (Inst, Param_Idx (Nd)), 0, Ld);
      end if;
      for I in reverse 1 .. Nd loop
         Disp_Binary_Digits
           (Get_Param_Uns32 (Inst, Param_Idx (I - 1)), 0, 32);
      end loop;
      Put ('"');
   end Disp_Const_Bit;

   procedure Disp_X_Lit (W : Width)
   is
      Q : constant Character := Get_Lit_Quote (W);
   begin
      if W <= 8 then
         Put (Q);
         Put ((1 .. Natural (W) => 'X'));
         Put (Q);
      else
         Put ('(');
         Put_Trim (Uns32'Image (W - 1));
         Put (" downto 0 => 'X')");
      end if;
   end Disp_X_Lit;

   procedure Disp_Extract (Inst : Instance);

   procedure Disp_Constant_Inline (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      O : constant Net := Get_Output (Inst, 0);
   begin
      case Get_Id (Imod) is
         when Id_Const_UB32 =>
            Disp_Binary_Lit (Get_Param_Uns32 (Inst, 0), 0,  Get_Width (O));
         when Id_Const_UL32 =>
            Disp_Binary_Lit (Get_Param_Uns32 (Inst, 0),
                             Get_Param_Uns32 (Inst, 1),
                             Get_Width (O));
         when Id_Const_Z =>
            Put ('(');
            Put_Uns32 (Get_Width (O) - 1);
            Put (" downto 0 => 'Z')");
         when Id_Const_X =>
            Put ('(');
            Put_Uns32 (Get_Width (O) - 1);
            Put (" downto 0 => 'X')");
         when Id_Const_Bit =>
            Disp_Const_Bit (Inst);
         when Id_Const_Log =>
            raise Internal_Error;
         when Id_Extract =>
            Disp_Extract (Inst);
         when others =>
            raise Internal_Error;
      end case;
   end Disp_Constant_Inline;

   function Need_Name (Inst : Instance) return Boolean
   is
      Id : constant Module_Id := Get_Id (Inst);
   begin
      case Id is
         when Id_Extract
           | Id_Dyn_Extract
           | Id_Dyn_Insert =>
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
         if Need_Name (Get_Parent (I)) then
            return True;
         end if;
         I := Get_Next_Sink (I);
      end loop;
      return False;
   end Need_Signal;

   type Conv_Type is (Conv_None, Conv_Unsigned, Conv_Signed);

   procedure Disp_Net_Expr (N : Net; Inst : Instance; Conv : Conv_Type)
   is
      Net_Inst : Instance;
   begin
      if N = No_Net then
         Put ("<unassigned>");
         return;
      end if;

      Net_Inst := Get_Parent (N);
      if Flag_Merge_Lit
        and then Is_Const_Module (Get_Id (Net_Inst))
        and then not Need_Name (Inst)
      then
         case Conv is
            when Conv_None =>
               Disp_Constant_Inline (Net_Inst);
            when Conv_Unsigned =>
               Put ("unsigned'(");
               Disp_Constant_Inline (Net_Inst);
               Put (")");
            when Conv_Signed =>
               Put ("signed'(");
               Disp_Constant_Inline (Net_Inst);
               Put (")");
         end case;
      else
         case Conv is
            when Conv_None =>
               Disp_Net_Name (N);
            when Conv_Unsigned =>
               Put ("unsigned (");
               Disp_Net_Name (N);
               Put (")");
            when Conv_Signed =>
               Put ("signed (");
               Disp_Net_Name (N);
               Put (")");
         end case;
      end if;
   end Disp_Net_Expr;

   NL : constant Character := ASCII.LF;

   type Uns32_Array is array (Natural range <>) of Uns32;
   No_Uns32_Arr : constant Uns32_Array := (1 .. 0 => 0);

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
            if S (I) = 'u' then
               Conv := Conv_Unsigned;
               I := I + 1;
            elsif S (I) = 's' then
               Conv := Conv_Signed;
               I := I + 1;
            else
               Conv := Conv_None;
            end if;
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
            Disp_Template (" (\n0 downto \n1)", Inst,
                           (0 => Off + Wd - 1, 1 => Off));
         else
            Disp_Template (" (\n0)", Inst, (0 => Off));
         end if;
      end if;
   end Disp_Extract;

   procedure Disp_Instance_Inline (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      Loc : constant Location_Type := Locations.Get_Location (Inst);
   begin
      if Loc /= No_Location then
         declare
            File : Name_Id;
            Line : Positive;
            Col : Natural;
         begin
            Files_Map.Location_To_Position (Loc, File, Line, Col);
            Put ("  -- ");
            Put_Id (File);
            Put (':');
            Put_Uns32 (Uns32 (Line));
            Put (':');
            Put_Uns32 (Uns32 (Col));
            New_Line;
         end;
      end if;
      case Get_Id (Imod) is
         when Id_Output =>
            Disp_Template ("  \o0 <= \i0; -- (output)" & NL, Inst);
         when Id_Signal =>
            Disp_Template ("  \o0 <= \i0; -- (signal)" & NL, Inst);
         when Id_Isignal =>
            Disp_Template ("  \o0 <= \i0; -- (isignal)" & NL, Inst);
         when Id_Port =>
            null;
         when Id_Not =>
            Disp_Template ("  \o0 <= not \i0;" & NL, Inst);
         when Id_Extract =>
            Disp_Template ("  \o0 <= ", Inst);
            Disp_Extract (Inst);
            Put_Line (";");
         when Id_Dyn_Extract =>
            declare
               O : constant Net := Get_Output (Inst, 0);
               Wd : constant Width := Get_Width (O);
               Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               Off : constant Uns32 := Get_Param_Uns32 (Inst, 1);
            begin
               Disp_Template ("  \o0 <= \i0 (to_integer (\ui1)", Inst);
               if Step /= 1 then
                  Disp_Template (" * \n0", Inst, (0 => Step));
               end if;
               if Off /= 0 then
                  Disp_Template (" + \n0", Inst, (0 => Off));
               end if;
               if Wd > 1 then
                  Disp_Template (" + \n0 - 1 downto to_integer (\ui1)",
                                 Inst, (0 => Wd));
                  if Step /= 1 then
                     Disp_Template (" * \n0", Inst, (0 => Step));
                  end if;
                  if Off /= 0 then
                     Disp_Template (" + \n0", Inst, (0 => Off));
                  end if;
               end if;
               Put_Line (");");
            end;
         when Id_Dyn_Insert =>
            declare
               --  I0: Input, I1: Value, I2: position
               --  P0: Step, P1: offset
               I0 : constant Net := Get_Input_Net (Inst, 0);
               I1 : constant Net := Get_Input_Net (Inst, 1);
               I2 : constant Net := Get_Input_Net (Inst, 2);
               Iarr : constant Net_Array (0 .. 2) := (I0, I1, I2);
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 1));
               First : Boolean;
            begin
               Put ("  process (");
               First := True;
               for I in Iarr'Range loop
                  if Get_Id (Get_Parent (Iarr (I))) not in Constant_Module_Id
                  then
                     if First then
                        First := False;
                     else
                        Put (", ");
                     end if;
                     Disp_Net_Name (Iarr (I));
                  end if;
               end loop;
               Put (")" & NL);
               Disp_Template
                 ("  begin" & NL &
                  "    \o0 <= \i0;" & NL &
                  "    \o0 (",
                  Inst);
               if Iw > 1 then
                  Disp_Template
                    ("to_integer (\ui2) * \p0 + (\sp1 + \n0)" & NL &
                       "        downto ",
                     Inst, (0 => Iw - 1));
               end if;
               Disp_Template
                 ("to_integer (\ui2) * \p0 + (\sp1))" &
                  " <= \i1;" & NL &
                  "  end process;" & NL,
                  Inst);
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
         when Id_Adff =>
            Disp_Template ("  process (\i0, \i2)" & NL &
                           "  begin" & NL &
                           "    if \i2 = '1' then" & NL &
                           "      \o0 <= \i3;" & NL &
                           "    elsif rising_edge (\i0) then" & NL &
                           "      \o0 <= \i1;" & NL &
                           "    end if;" & NL &
                           "  end process;" & NL, Inst);
         when Id_Dff
           | Id_Idff =>
            Disp_Template ("  process (\i0)" & NL &
                           "  begin" & NL &
                           "    if rising_edge (\i0) then" & NL &
                           "      \o0 <= \i1;" & NL &
                           "    end if;" & NL &
                           "  end process;" & NL, Inst);
         when Id_Mux2 =>
            Disp_Template ("  \o0 <= \i1 when \i0 = '0' else \i2;" & NL, Inst);
         when Id_Mux4 =>
            Disp_Template ("  with \i0 select \o0 <=" & NL &
                           "    \i1 when ""00""," & NL &
                           "    \i2 when ""01""," & NL &
                           "    \i3 when ""10""," & NL &
                           "    \i4 when ""11""," & NL, Inst);
            Put ("    ");
            Disp_X_Lit (Get_Width (Get_Output (Inst, 0)));
            Put_Line (" when others;");
         when Id_Add =>
            Disp_Template ("  \o0 <= std_logic_vector (\ui0 + \ui1);" & NL,
                           Inst);
         when Id_Sub =>
            Disp_Template ("  \o0 <= std_logic_vector (\ui0 - \ui1);" & NL,
                           Inst);
         when Id_Umul =>
            Disp_Template
              ("  \o0 <= std_logic_vector (resize (\ui0 * \ui1, \n0));" & NL,
               Inst, (0 => Get_Width (Get_Output (Inst, 0))));
         when Id_Smul =>
            Disp_Template
              ("  \o0 <= std_logic_vector (resize (\si0 * \si1, \n0));" & NL,
               Inst, (0 => Get_Width (Get_Output (Inst, 0))));
         when Id_Ult =>
            Disp_Template ("  \o0 <= '1' when \ui0 < \ui1 else '0';" & NL,
                           Inst);
         when Id_Ule =>
            Disp_Template ("  \o0 <= '1' when \ui0 <= \ui1 else '0';" & NL,
                           Inst);
         when Id_Ugt =>
            Disp_Template ("  \o0 <= '1' when \ui0 > \ui1 else '0';" & NL,
                           Inst);
         when Id_Uge =>
            Disp_Template ("  \o0 <= '1' when \ui0 >= \ui1 else '0';" & NL,
                           Inst);
         when Id_Slt =>
            Disp_Template ("  \o0 <= '1' when \si0 < \si1 else '0';" & NL,
                           Inst);
         when Id_Sle =>
            Disp_Template ("  \o0 <= '1' when \si0 <= \si1 else '0';" & NL,
                           Inst);
         when Id_Sgt =>
            Disp_Template ("  \o0 <= '1' when \si0 > \si1 else '0';" & NL,
                           Inst);
         when Id_Sge =>
            Disp_Template ("  \o0 <= '1' when \si0 >= \si1 else '0';" & NL,
                           Inst);
         when Id_Eq =>
            Disp_Template ("  \o0 <= '1' when \i0 = \i1 else '0';" & NL, Inst);
         when Id_Ne =>
            Disp_Template ("  \o0 <= '1' when \i0 /= \i1 else '0';" & NL,
                           Inst);
         when Id_Or =>
            Disp_Template ("  \o0 <= \i0 or \i1;" & NL, Inst);
         when Id_And =>
            Disp_Template ("  \o0 <= \i0 and \i1;" & NL, Inst);
         when Id_Xor =>
            Disp_Template ("  \o0 <= \i0 xor \i1;" & NL, Inst);
         when Id_Concat2 =>
            Disp_Template ("  \o0 <= \i0 & \i1;" & NL, Inst);
         when Id_Concat3 =>
            Disp_Template ("  \o0 <= \i0 & \i1 & \i2;" & NL, Inst);
         when Id_Concat4 =>
            Disp_Template ("  \o0 <= \i0 & \i1 & \i2 & \i3;" & NL, Inst);
         when Id_Concatn =>
            Disp_Template ("  \o0 <= \i0", Inst);
            for I in 1 .. Get_Nbr_Inputs (Inst) - 1 loop
               Disp_Template (" & ", Inst);
               Disp_Net_Expr (Get_Input_Net (Inst, I), Inst, Conv_None);
            end loop;
            Disp_Template(";" & NL, Inst);
         when Id_Utrunc
           | Id_Strunc =>
            declare
               W : constant Width := Get_Width (Get_Output (Inst, 0));
            begin
               Disp_Template ("  \o0 <= \i0 (\n0 downto 0);  --  trunc" & NL,
                              Inst, (0 => W - 1));
            end;
         when Id_Uextend =>
            declare
               Ow : constant Width := Get_Width (Get_Output (Inst, 0));
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 0));
            begin
               pragma Assert (Iw > 0);
               pragma Assert (Ow > Iw);
               Disp_Template ("  \o0 <= """, Inst);
               Put ((1 .. Natural (Ow - Iw) => '0'));
               Disp_Template (""" & \i0;  --  uext" & NL, Inst);
            end;
         when Id_Lsr =>
            Disp_Template
              ("  \o0 <= std_logic_vector "
                 & "(shift_right (\ui0, to_integer(\ui1)));" & NL, Inst);
         when Id_Red_Or =>
            declare
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 0));
            begin
               if Iw > 1 then
                  Disp_Template
                    ("  \o0 <= '1' when \i0 /= (\n0 downto 0 => '0') else '0';"
                       & NL, Inst, (0 => Iw - 1));
               else
                  Disp_Template
                    ("  \o0 <= \i1; -- reduce or" & NL, Inst);
               end if;
            end;
         when Id_Red_And =>
            declare
               Iw : constant Width := Get_Width (Get_Input_Net (Inst, 0));
            begin
               if Iw > 1 then
                  Disp_Template
                    ("  \o0 <= '1' when \i0 = (\n0 downto 0 => '1') else '0';"
                       & NL, Inst, (0 => Iw - 1));
               else
                  Disp_Template
                    ("  \o0 <= \i1; -- reduce and" & NL, Inst);
               end if;
            end;
         when Id_Edge =>
            Disp_Template
              ("  \o0 <= '1' when rising_edge (\i0) else '0';" & NL, Inst);
         when Id_Assert =>
            Disp_Template
              ("  \l0: assert \i0 = '1' severity error;" & NL, Inst);
         when Id_Assume =>
            Disp_Template
              ("  \l0: assert \i0 = '1' severity warning; --  assume" & NL,
               Inst);
         when Id_Cover =>
            Disp_Template
              ("  \l0: assert \i0 = '1' severity note; --  cover" & NL,
               Inst);
         when others =>
            Disp_Instance_Gate (Inst);
      end case;
   end Disp_Instance_Inline;

   procedure Disp_Architecture_Declarations (M : Module)
   is
      Id : Module_Id;
   begin
      --  Display signal declarations.
      --  There are as many signals as gate outputs.
      for Inst of Instances (M) loop
         Id := Get_Id (Inst);
         if not Is_Self_Instance (Inst)
           and then not (Flag_Merge_Lit
                           and then Id in Constant_Module_Id
                           and then not Need_Signal (Inst))
           and then Id < Id_User_None
         then
            for N of Outputs (Inst) loop
               if Id in Constant_Module_Id then
                  Put ("  constant ");
               else
                  Put ("  signal ");
               end if;
               Disp_Net_Name (N);
               Put (" : ");
               Put_Type (Get_Width (N));
               case Id is
                  when Id_Idff =>
                     Put (" := ");
                     Disp_Constant_Inline
                       (Get_Parent (Get_Input_Net (Inst, 2)));
                  when Constant_Module_Id =>
                     Put (" := ");
                     Disp_Constant_Inline (Inst);
                  when others =>
                     null;
               end case;
               Put_Line (";");
            end loop;
         end if;
      end loop;
   end Disp_Architecture_Declarations;

   procedure Disp_Architecture_Statements (M : Module)
   is
      Self_Inst : constant Instance := Get_Self_Instance (M);
   begin
      --  Output assignments.
      declare
         Idx : Port_Idx;
      begin
         Idx := 0;
         for I of Inputs (Self_Inst) loop
            Put ("  ");
            Put_Name (Get_Output_Desc (M, Idx).Name);
            Put (" <= ");
            Disp_Net_Name (Get_Driver (I));
            Put_Line (";");
            Idx := Idx + 1;
         end loop;
      end;

      for Inst of Instances (M) loop
         if not (Flag_Merge_Lit
                   and then Is_Const_Module (Get_Id (Inst)))
         then
            Disp_Instance_Inline (Inst);
         end if;
      end loop;
   end Disp_Architecture_Statements;

   procedure Disp_Architecture (M : Module)
   is
      Self_Inst : constant Instance := Get_Self_Instance (M);
   begin
      if Self_Inst = No_Instance then
         --  Not defined.
         return;
      end if;

      Put ("architecture rtl of ");
      Put_Name (Get_Name (M));
      Put_Line (" is");

      --  Dummy display:
      --  * generate one signal per net
      --  * generate instances

      Disp_Architecture_Declarations (M);

      Put_Line ("begin");

      Disp_Architecture_Statements (M);

      Put_Line ("end rtl;");
      New_Line;
   end Disp_Architecture;

   procedure Disp_Entity_Ports (M : Module)
   is
      First : Boolean;
   begin
      First := True;
      for P of Ports_Desc (M) loop
         if First then
            Put_Line ("  port (");
            First := False;
         else
            Put_Line (";");
         end if;
         Put ("    ");
         Put_Name (P.Name);
         Put (" : ");
         case P.Dir is
            when Port_In =>
               Put ("in");
            when Port_Out =>
               Put ("out");
            when Port_Inout =>
               Put ("inout");
         end case;
         Put (' ');
         Put_Type (P.W);
      end loop;
      if not First then
         Put_Line (");");
      end if;
   end Disp_Entity_Ports;

   procedure Disp_Entity (M : Module) is
   begin
      --  Module id and name.
      Put_Line ("library ieee;");
      Put_Line ("use ieee.std_logic_1164.all;");
      Put_Line ("use ieee.numeric_std.all;");
      New_Line;
      Put ("entity ");
      Put_Name (Get_Name (M));
      Put_Line (" is");

      Disp_Entity_Ports (M);

      Put ("end entity ");
      Put_Name (Get_Name (M));
      Put_Line (";");
      New_Line;
   end Disp_Entity;

   procedure Disp_Vhdl (M : Module; Is_Top : Boolean) is
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
               Disp_Vhdl (Modules (I), False);
            end loop;
         end;
      end;

      if not Is_Top then
         Disp_Entity (M);
         Disp_Architecture (M);
      end if;
   end Disp_Vhdl;

   procedure Disp_Vhdl (M : Module) is
   begin
      Disp_Vhdl (M, True);
   end Disp_Vhdl;
end Netlists.Disp_Vhdl;
