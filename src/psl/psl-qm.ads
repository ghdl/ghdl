--  PSL - Small QM reduction
--  Copyright (C) 2002-2016 Tristan Gingold
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

with PSL.Nodes; use PSL.Nodes;
with Interfaces; use Interfaces;

package PSL.QM is
   type Primes_Set (<>) is private;

   function Build_Primes (N : Node) return Primes_Set;

   function Build_Node (Ps : Primes_Set) return Node;

   function Reduce (N : Node) return Node;

   --  The maximum number of terms that this package can handle.
   --  The algorithm is in O(2**n)
   Max_Terms : constant Natural := 12;

   type Term_Assoc_Type is array (1 .. Max_Terms) of Node;
   Term_Assoc : Term_Assoc_Type := (others => Null_Node);
   Nbr_Terms : Natural := 0;

   procedure Reset;

   procedure Disp_Primes_Set (Ps : Primes_Set);
private
   --  Scalar type used to represent a vector of booleans for terms.
   subtype Vector_Type is Unsigned_16;
   pragma Assert (Vector_Type'Modulus >= 2 ** Max_Terms);

   --  States of a vector of term.
   --  If SET is 0, this is a don't care: the term has no influence.
   --  If SET is 1, the value of the term is in VAL.
   type Prime_Type is record
      Val : Unsigned_16;
      Set : Unsigned_16;
   end record;

   subtype Len_Type is Natural range 0 .. 2 ** Max_Terms;

   type Set_Type is array (Natural range <>) of Prime_Type;

   --  A set of primes is a collection of at most MAX prime.
   type Primes_Set (Max : Len_Type) is record
      Nbr : Len_Type := 0;
      Set : Set_Type (1 .. Max);
   end record;
end PSL.QM;
