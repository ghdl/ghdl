--  Expressions synthesis.
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

with Types; use Types;
with Simul.Environments; use Simul.Environments;
with Synth.Values; use Synth.Values;
with Iirs; use Iirs;

package Synth.Expr is
   function Is_Const (Val : Value_Acc) return Boolean;
   function Get_Width (Val : Value_Acc) return Uns32;

   procedure To_Logic (Lit : Iir_Value_Literal_Acc;
                       Val : out Uns32;
                       Xz : out Uns32);

   function Bit_Extract (Val : Value_Acc; Off : Uns32) return Value_Acc;

   function Synth_Expression_With_Type
     (Syn_Inst : Synth_Instance_Acc; Expr : Iir; Expr_Type : Iir)
     return Value_Acc;

   function Synth_Expression (Syn_Inst : Synth_Instance_Acc; Expr : Iir)
                             return Value_Acc;
end Synth.Expr;
