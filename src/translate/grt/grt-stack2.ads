--  GHDL Run Time (GRT) - secondary stack.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with System;
with Grt.Types; use Grt.Types;

--  Secondary stack management.
package Grt.Stack2 is
   type Stack2_Ptr is new System.Address;
   Null_Stack2_Ptr : constant Stack2_Ptr := Stack2_Ptr (System.Null_Address);

   type Mark_Id is new Integer_Address;

   function Mark (S : Stack2_Ptr) return Mark_Id;
   procedure Release (S : Stack2_Ptr; Mark : Mark_Id);
   function Allocate (S : Stack2_Ptr; Size : Ghdl_Index_Type)
     return System.Address;
   function Create return Stack2_Ptr;

   --  Check S is empty.
   procedure Check_Empty (S : Stack2_Ptr);
end Grt.Stack2;
