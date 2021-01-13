--  GHDL Run Time (GRT) - mono-thread version.
--  Copyright (C) 2005 - 2014 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Signals; use Grt.Signals;
with Grt.Stack2; use Grt.Stack2;

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
   pragma Export (C, Set_Current_Process, "__ghdl_set_current_process");
   function Get_Current_Process return Process_Acc;

   --  The stack2 for all sensitized process.  Since they cannot have
   --  wait statements, the stack2 is always empty when the process is
   --  suspended.
   function Get_Common_Stack2 return Stack2_Ptr;
private
   pragma Inline (Run_Parallel);
   pragma Inline (Atomic_Insert);
   pragma Inline (Atomic_Inc);

   pragma Inline (Set_Current_Process);
   pragma Inline (Get_Current_Process);

   pragma Inline (Get_Common_Stack2);
end Grt.Unithread;
