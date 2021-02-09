--  Semantic analysis pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Sem_Psl is
   function Is_Psl_Boolean_Type (Atype : Iir) return Boolean;
   function Is_Psl_Bit_Type (Atype : Iir) return Boolean;
   function Is_Psl_Bitvector_Type (Atype : Iir) return Boolean;

   function Sem_Prev_Builtin (Call : Iir; Atype : Iir) return Iir;

   --  For stable, rose and fell builtins.
   function Sem_Clock_Builtin (Call : Iir) return Iir;

   --  For onehot and onehot0.
   function Sem_Onehot_Builtin (Call : Iir) return Iir;

   procedure Sem_Psl_Declaration (Stmt : Iir);
   procedure Sem_Psl_Endpoint_Declaration (Stmt : Iir);

   --  May return a non-psl concurrent assertion statement iff CAN_REWRITE is
   --  true.
   function Sem_Psl_Assert_Directive
     (Stmt : Iir; Can_Rewrite : Boolean) return Iir;

   procedure Sem_Psl_Assume_Directive (Stmt : Iir);
   procedure Sem_Psl_Cover_Directive (Stmt : Iir);
   procedure Sem_Psl_Restrict_Directive (Stmt : Iir);
   procedure Sem_Psl_Default_Clock (Stmt : Iir);
   function Sem_Psl_Name (Name : Iir) return Iir;

   procedure Sem_Psl_Verification_Unit (Unit : Iir);
end Vhdl.Sem_Psl;
