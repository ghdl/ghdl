--  Expressions synthesis for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Ada.Unchecked_Conversion;

with Types; use Types;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Elab.Memtype; use Elab.Memtype;

with Verilog.Types; use Verilog.Types;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Bignums; use Verilog.Bignums;

with Synth.Verilog_Context; use Synth.Verilog_Context;
with Synth.Verilog_Values; use Synth.Verilog_Values;

package Synth.Verilog_Exprs is
   function To_Logvec_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Logvec_Ptr);
   function To_Logic_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Logic_Ptr);
   function To_Bitvec_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Bitvec_Ptr);
--   function To_Bit_Ptr is
--      new Ada.Unchecked_Conversion (Memory_Ptr, Bit_Ptr);

   function Get_Type_Bitwidth (N : Node) return Width;

   function Memory2net (Ctxt : Context_Acc; Mem : Memory_Ptr; Typ : Node)
                       return Net;

   function Synth_Expression (Inst : Synth_Instance_Acc; N : Node)
                             return Valtyp;

   --  Synthesize a condition.  If not static, automatically converted to
   --  a single bit.
   function Synth_Condition (Inst : Synth_Instance_Acc; N : Node)
                            return Valtyp;

   --  Read a single logic value.
   function Read_Logic (Mem : Memory_Ptr) return Logic_Type;

   --  Extract bits at OFF from VAL.
   --  It is expected that VAL has enough bits for OFF and width of RES_TYP.
   --  (used to split a value for assignment to concatenation)
   function Synth_Extract (Inst : Synth_Instance_Acc;
                           Val : Valtyp;
                           Off : Uns32;
                           Res_Typ : Node) return Valtyp;

   type Name_Offsets is record
      --  Offset for a net.
      Net_Off : Uns32;

      --  Memory and bit offsets.
      --  The memory offset is offet to the vector, while the bit offset
      --  is the offset within the vector.
      Mem_Off : Size_Type;
      Bit_Off : Bit_Offset;
   end record;

   procedure Synth_Name (Inst : Synth_Instance_Acc;
                         N : Node;
                         Base : out Valtyp;
                         Doff : out Net;
                         Off : out Name_Offsets);
end Synth.Verilog_Exprs;
