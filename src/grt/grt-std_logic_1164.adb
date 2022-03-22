--  GHDL Run Time (GRT) std_logic_1664 subprograms.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Lib;
with Grt.Errors; use Grt.Errors;
with Grt.Severity; use Grt.Severity;

package body Grt.Std_Logic_1164 is
   Assert_DC_Msg : constant String :=
     "STD_LOGIC_1164: '-' operand for matching ordering operator";

   Assert_DC_Msg_Bound : constant Std_String_Bound :=
     (Dim_1 => (Left => 1, Right => Assert_DC_Msg'Length, Dir => Dir_To,
                Length => Assert_DC_Msg'Length));

   Assert_DC_Msg_Str : aliased constant Std_String :=
     (Base => To_Std_String_Basep (Assert_DC_Msg'Address),
      Bounds => To_Std_String_Boundp (Assert_DC_Msg_Bound'Address));

   Filename : constant String := "std_logic_1164.vhdl" & NUL;
   Loc : aliased constant Ghdl_Location :=
     (Filename => To_Ghdl_C_String (Filename'Address),
      Line => 58,
      Col => 3);

   procedure Assert_Not_Match
   is
      use Grt.Lib;
   begin
      Ghdl_Ieee_Assert_Failed
        (To_Std_String_Ptr (Assert_DC_Msg_Str'Address), Error_Severity,
         To_Ghdl_Location_Ptr (Loc'Address));
   end Assert_Not_Match;

   function Ghdl_Std_Ulogic_Match_Eq (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      return Std_Ulogic'Pos (Match_Eq_Table (Left, Right));
   end Ghdl_Std_Ulogic_Match_Eq;

   function Ghdl_Std_Ulogic_Match_Ne (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      return Std_Ulogic'Pos (Not_Table (Match_Eq_Table (Left, Right)));
   end Ghdl_Std_Ulogic_Match_Ne;

   function Ghdl_Std_Ulogic_Match_Lt (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      if Left = '-' or Right = '-' then
         Assert_Not_Match;
      end if;
      return Std_Ulogic'Pos (Match_Lt_Table (Left, Right));
   end Ghdl_Std_Ulogic_Match_Lt;

   function Ghdl_Std_Ulogic_Match_Le (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      if Left = '-' or Right = '-' then
         Assert_Not_Match;
      end if;
      return Std_Ulogic'Pos (Or_Table (Match_Lt_Table (Left, Right),
                                       Match_Eq_Table (Left, Right)));
   end Ghdl_Std_Ulogic_Match_Le;

   function Ghdl_Std_Ulogic_Match_Ge (L, R : Ghdl_E8) return Ghdl_E8
   is
      Lt : constant Ghdl_E8 := Ghdl_Std_Ulogic_Match_Lt(L, R);
   begin
      return Std_Ulogic'Pos (Not_Table (Std_Ulogic'Val(Lt))) ;
   end Ghdl_Std_Ulogic_Match_Ge;

   function Ghdl_Std_Ulogic_Match_Gt (L, R : Ghdl_E8) return Ghdl_E8
   is
      Le : constant Ghdl_E8 := Ghdl_Std_Ulogic_Match_Le(L, R);
   begin
      return Std_Ulogic'Pos (Not_Table (Std_Ulogic'Val(Le))) ;
   end Ghdl_Std_Ulogic_Match_Gt;

   Assert_Arr_Msg : constant String :=
     "parameters of '?=' array operator are not of the same length";

   Assert_Arr_Msg_Bound : constant Std_String_Bound :=
     (Dim_1 => (Left => 1, Right => Assert_Arr_Msg'Length, Dir => Dir_To,
                Length => Assert_Arr_Msg'Length));

   Assert_Arr_Msg_Str : aliased constant Std_String :=
     (Base => To_Std_String_Basep (Assert_Arr_Msg'Address),
      Bounds => To_Std_String_Boundp (Assert_Arr_Msg_Bound'Address));

   function Ghdl_Std_Ulogic_Array_Match_Eq (L : Ghdl_Ptr;
                                            L_Len : Ghdl_Index_Type;
                                            R : Ghdl_Ptr;
                                            R_Len : Ghdl_Index_Type)
                                           return Ghdl_I32
   is
      use Grt.Lib;
      L_Arr : constant Ghdl_E8_Array_Base_Ptr :=
        To_Ghdl_E8_Array_Base_Ptr (L);
      R_Arr : constant Ghdl_E8_Array_Base_Ptr :=
        To_Ghdl_E8_Array_Base_Ptr (R);
      Res : Std_Ulogic := '1';
   begin
      if L_Len /= R_Len then
         Ghdl_Ieee_Assert_Failed
           (To_Std_String_Ptr (Assert_Arr_Msg_Str'Address), Error_Severity,
            To_Ghdl_Location_Ptr (Loc'Address));
         return Std_Ulogic'Pos ('0');
      end if;

      for I in 1 .. L_Len loop
         declare
            Le : constant Std_Ulogic := Std_Ulogic'Val (L_Arr (I - 1));
            Re : constant Std_Ulogic := Std_Ulogic'Val (R_Arr (I - 1));
         begin
            Res := And_Table (Res, Match_Eq_Table (Le, Re));
         end;
      end loop;
      return Std_Ulogic'Pos (Res);
   end Ghdl_Std_Ulogic_Array_Match_Eq;

   function Ghdl_Std_Ulogic_Array_Match_Ne (L : Ghdl_Ptr;
                                            L_Len : Ghdl_Index_Type;
                                            R : Ghdl_Ptr;
                                            R_Len : Ghdl_Index_Type)
                                           return Ghdl_I32 is
   begin
      return Std_Ulogic'Pos
        (Not_Table (Std_Ulogic'Val
                      (Ghdl_Std_Ulogic_Array_Match_Eq (L, L_Len, R, R_Len))));
   end Ghdl_Std_Ulogic_Array_Match_Ne;
end Grt.Std_Logic_1164;
