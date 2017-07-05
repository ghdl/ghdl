--  Walk in iirs nodes.
--  Copyright (C) 2009 Tristan Gingold
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

with Iirs; use Iirs;

package Iirs_Walk is
   type Walk_Status is
     (
      --  Continue to walk.
      Walk_Continue,

      --  Stop walking in the subtree, continue in the parent tree.
      Walk_Up,

      --  Abort the walk.
      Walk_Abort);

   type Walk_Cb is access function (El : Iir) return Walk_Status;

   --  Walk on all elements of CHAIN.
   function Walk_Chain (Chain : Iir; Cb : Walk_Cb) return Walk_Status;


   function Walk_Assignment_Target (Target : Iir; Cb : Walk_Cb)
                                   return Walk_Status;

   --  Walk on all stmts and sub-stmts of CHAIN.
   function Walk_Sequential_Stmt_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status;

   --  Walk on all design units of library or design file PARENT.
   function Walk_Design_Units (Parent : Iir; Cb : Walk_Cb) return Walk_Status;

   --  Walk on all concurrent statements (and sub statements) of CHAIN.
   function Walk_Concurrent_Statements_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status;

end Iirs_Walk;
