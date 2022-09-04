--  std_logic_arith
--  Copyright (C) 2022 Tristan Gingold
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

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;

package Synth.Ieee.Std_Logic_Arith is

   --  "+"
   function Add_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Add_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Add_Uns_Int_Uns (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp;
   function Add_Sgn_Int_Sgn (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp;

   function Add_Uns_Log_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Add_Sgn_Log_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;

   --  "-"
   function Sub_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Uns_Int_Uns (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp;
   function Sub_Int_Uns_Uns (L : Int64; R : Memtyp; Loc : Location_Type)
                            return Memtyp;
   function Sub_Sgn_Int_Sgn (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp;
   function Sub_Int_Sgn_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                            return Memtyp;
   function Sub_Uns_Log_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Sgn_Log_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Log_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Sub_Log_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;

   --  Unary
   function Neg_Sgn_Sgn (L : Memtyp; Loc : Location_Type) return Memtyp;
   function Abs_Sgn_Sgn (L : Memtyp; Loc : Location_Type) return Memtyp;

   --  "*"
   function Mul_Uns_Uns_Uns (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Mul_Sgn_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Mul_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;
   function Mul_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type) return Memtyp;

   --  Comparison
   function Compare_Uns_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Order_Type;
   function Compare_Uns_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Order_Type;
   function Compare_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Order_Type;

end Synth.Ieee.Std_Logic_Arith;
