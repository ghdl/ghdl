--  Generic package to handle chains.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Iirs; use Iirs;

--  The generic package Chain_Handling can be used to build or modify
--  chains.
--  The formals are the subprograms to get and set the first element
--   from the parent.
generic
   with function Get_Chain_Start (Parent : Iir) return Iir;
   with procedure Set_Chain_Start (Parent : Iir; First : Iir);
package Iir_Chain_Handling is

   --  Building a chain:
   --  Initialize (set LAST to NULL_IIR).
   procedure Build_Init (Last : out Iir);
   --  Set LAST with the last element of the chain.
   --  This is an initialization for an already built chain.
   procedure Build_Init (Last : out Iir; Parent : Iir);

   --  Append element EL to the chain, whose parent is PARENT and last
   --   element LAST.
   procedure Append (Last : in out Iir; Parent : Iir; El : Iir);

   --  Append a subchain whose first element is ELS to a chain, whose
   --   parent is PARENT and last element LAST.
   --   The Parent field of each elements of Els is set to PARENT.
   --  Note: the Append procedure declared just above is an optimization
   --   of this subprogram if ELS has no next element.  However, the
   --   above subprogram does not set the Parent field of EL.
   procedure Append_Subchain (Last : in out Iir; Parent : Iir; Els : Iir);
end Iir_Chain_Handling;
