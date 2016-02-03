--  PSL - Utils
--  Copyright (C) 2002-2016 Tristan Gingold
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

package PSL.NFAs.Utils is
   --  Sort outgoing edges by expression.
   procedure Sort_Src_Edges (S : NFA_State);
   procedure Sort_Src_Edges (N : NFA);

   procedure Sort_Dest_Edges (S : NFA_State);
   procedure Sort_Dest_Edges (N : NFA);

   --  Move incoming edges of S1 to S, remove S1 and its outgoing edges.
   procedure Merge_State_Dest (N : NFA; S : NFA_State; S1 : NFA_State);

   procedure Merge_State_Src (N : NFA; S : NFA_State; S1 : NFA_State);

   --  Return True if N or a child of N is EOS.
   --  N must be a boolean expression.
   function Has_EOS (N : Node) return Boolean;

   --  Raise Program_Error if N is not internally coherent.
   procedure Check_NFA (N : NFA);
end PSL.NFAs.Utils;
