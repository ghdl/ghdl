--  GHDL Run Time (GRT) - secondary stack.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--  The secondary stack is used by vhdl to return object from function whose
--  type is unconstrained.  This is less efficient than returning the object
--  on the stack, but compatible with any ABI.
--
--  The management is very simple: mark and release.  Allocate reserved a
--  chunk of memory from the secondary stack, Release deallocate all the
--  memory allocated since the mark.

package Grt.Stack2 is
   --  Designate a secondary stack.
   type Stack2_Ptr is private;

   --  Indicator for a non-existing secondary stack.  Create never return that
   --  value.
   Null_Stack2_Ptr : constant Stack2_Ptr;

   --  Type of a mark.
   type Mark_Id is private;

   --  Get the current mark, which indicate a current amount of allocated
   --  memory.
   function Mark (S : Stack2_Ptr) return Mark_Id;

   --  Deallocate (free) all the memory allocated since MARK.
   procedure Release (S : Stack2_Ptr; Mark : Mark_Id);

   --  Allocate SIZE bytes (aligned on the maximum alignment) on stack S.
   function Allocate (S : Stack2_Ptr; Size : Ghdl_Index_Type)
                     return System.Address;

   --  Create a secondary stack.
   function Create return Stack2_Ptr;

   --  Return True iff S is null or empty.
   function Is_Empty (S : Stack2_Ptr) return Boolean;

   --  May be used to debug.
   procedure Dump_Stack2 (S : Stack2_Ptr);
private
   type Stack2_Ptr is new System.Address;
   Null_Stack2_Ptr : constant Stack2_Ptr := Stack2_Ptr (System.Null_Address);

   type Mark_Id is new Integer_Address;
end Grt.Stack2;
