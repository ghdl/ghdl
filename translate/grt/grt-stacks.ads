--  GHDL Run Time (GRT) - process stacks.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with System; use System;
with Ada.Unchecked_Conversion;

package Grt.Stacks is
   --  Instance is the parameter of the process procedure.
   --  This is in fact a fully opaque type whose content is private to the
   --  process.
   type Instance is limited private;
   type Instance_Acc is access all Instance;
   pragma Convention (C, Instance_Acc);

   --  A process is identified by a procedure having a single private
   --  parameter (its instance).
   type Proc_Acc is access procedure (Self : Instance_Acc);
   pragma Convention (C, Proc_Acc);

   function To_Address is new Ada.Unchecked_Conversion
     (Instance_Acc, System.Address);

   type Stack_Type is new Address;
   Null_Stack : constant Stack_Type := Stack_Type (Null_Address);

   --  Initialize the stacks package.
   --  This may adjust stack sizes.
   --  Must be called after grt.options.decode.
   procedure Stack_Init;

   --  Create a new stack, which on first execution will call FUNC with
   --  an argument ARG.
   function Stack_Create (Func : Proc_Acc; Arg : Instance_Acc)
                         return Stack_Type;

   --  Resume stack TO and save the current context to the stack pointed by
   --  CUR.
   procedure Stack_Switch (To : Stack_Type; From : Stack_Type);

   --  Delete stack STACK, which must not be currently executed.
   procedure Stack_Delete (Stack : Stack_Type);

   --  Error during stack handling:
   --  Cannot grow the stack.
   procedure Error_Grow_Failed;
   pragma No_Return (Error_Grow_Failed);

   --  Invalid memory access detected (other than dereferencing a NULL access).
   procedure Error_Memory_Access;
   pragma No_Return (Error_Memory_Access);

   --  A NULL access is dereferenced.
   procedure Error_Null_Access;
   pragma No_Return (Error_Null_Access);
private
   type Instance is null record;

   pragma Import (C, Stack_Init, "grt_stack_init");
   pragma Import (C, Stack_Create, "grt_stack_create");
   pragma Import (C, Stack_Switch, "grt_stack_switch");
   pragma Import (C, Stack_Delete, "grt_stack_delete");

   pragma Export (C, Error_Grow_Failed, "grt_stack_error_grow_failed");
   pragma Export (C, Error_Memory_Access, "grt_stack_error_memory_access");
   pragma Export (C, Error_Null_Access, "grt_stack_error_null_access");
end Grt.Stacks;
