--  GHDL Run Time (GRT) - mono-thread version.
--  Copyright (C) 2005 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Signals; use Grt.Signals;
with Grt.Stack2; use Grt.Stack2;
with Grt.Stacks; use Grt.Stacks;

package Grt.Unithread is
   procedure Init;
   procedure Finish;

   type Parallel_Subprg_Acc is access procedure;
   procedure Run_Parallel (Subprg : Parallel_Subprg_Acc);

   --  Return the old value of LIST.all and store EL into LIST.all.
   function Atomic_Insert (List : access Ghdl_Signal_Ptr; El : Ghdl_Signal_Ptr)
                          return Ghdl_Signal_Ptr;

   --  Return the old value.
   function Atomic_Inc (Val : access Natural) return Natural;

   --  Set and get the current process being executed by the thread.
   procedure Set_Current_Process (Proc : Process_Acc);
   function Get_Current_Process return Process_Acc;

   --  The secondary stack for the thread.
   function Get_Stack2 return Stack2_Ptr;
   procedure Set_Stack2 (St : Stack2_Ptr);

   --  The main stack.  This is initialized by STACK_INIT.
   --  The return point.
   function Get_Main_Stack return Stack_Type;
   procedure Set_Main_Stack (St : Stack_Type);
private
   pragma Inline (Run_Parallel);
   pragma Inline (Atomic_Insert);
   pragma Inline (Atomic_Inc);
   pragma Inline (Get_Stack2);
   pragma Inline (Set_Stack2);

   pragma Inline (Get_Main_Stack);
   pragma Export (C, Set_Main_Stack, "grt_set_main_stack");

   pragma Inline (Set_Current_Process);
   pragma Inline (Get_Current_Process);

end Grt.Unithread;
