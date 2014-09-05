--  GHDL Run Time (GRT) std_logic_1664 subprograms.
--  Copyright (C) 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Grt.Lib;

package body Grt.Std_Logic_1164 is
   Assert_Msg : constant String :=
     "STD_LOGIC_1164: '-' operand for matching ordering operator";

   Assert_Msg_Bound : constant Std_String_Bound :=
     (Dim_1 => (Left => 1, Right => Assert_Msg'Length, Dir => Dir_To,
                Length => Assert_Msg'Length));

   Assert_Msg_Str : aliased constant Std_String :=
     (Base => To_Std_String_Basep (Assert_Msg'Address),
      Bounds => To_Std_String_Boundp (Assert_Msg_Bound'Address));

   Filename : constant String := "std_logic_1164.vhdl" & NUL;
   Loc : aliased constant Ghdl_Location :=
     (Filename => To_Ghdl_C_String (Filename'Address),
      Line => 58,
      Col => 3);

   procedure Assert_Not_Match (V : Std_Ulogic)
   is
      use Grt.Lib;
   begin
      if V = '-' then
         --  FIXME: assert disabled for ieee.
         Ghdl_Assert_Failed
           (To_Std_String_Ptr (Assert_Msg_Str'Address), Error_Severity,
            To_Ghdl_Location_Ptr (Loc'Address), null);
      end if;
   end Assert_Not_Match;

   function Ghdl_Std_Ulogic_Match_Eq (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      Assert_Not_Match (Left);
      Assert_Not_Match (Right);
      return Std_Ulogic'Pos (Match_Eq_Table (Left, Right));
   end Ghdl_Std_Ulogic_Match_Eq;

   function Ghdl_Std_Ulogic_Match_Ne (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      Assert_Not_Match (Left);
      Assert_Not_Match (Right);
      return Std_Ulogic'Pos (Not_Table (Match_Eq_Table (Left, Right)));
   end Ghdl_Std_Ulogic_Match_Ne;

   function Ghdl_Std_Ulogic_Match_Lt (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      Assert_Not_Match (Left);
      Assert_Not_Match (Right);
      return Std_Ulogic'Pos (Match_Lt_Table (Left, Right));
   end Ghdl_Std_Ulogic_Match_Lt;

   function Ghdl_Std_Ulogic_Match_Le (L, R : Ghdl_E8) return Ghdl_E8
   is
      Left : constant Std_Ulogic := Std_Ulogic'Val (L);
      Right : constant Std_Ulogic := Std_Ulogic'Val (R);
   begin
      Assert_Not_Match (Left);
      Assert_Not_Match (Right);
      return Std_Ulogic'Pos (Or_Table (Match_Lt_Table (Left, Right),
                                       Match_Eq_Table (Left, Right)));
   end Ghdl_Std_Ulogic_Match_Le;
end Grt.Std_Logic_1164;
