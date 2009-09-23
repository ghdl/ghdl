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

package body Grt.Unithread is
   procedure Init is
   begin
      null;
   end Init;

   procedure Finish is
   begin
      null;
   end Finish;

   procedure Run_Parallel (Subprg : Parallel_Subprg_Acc) is
   begin
      Subprg.all;
   end Run_Parallel;

   function Atomic_Insert (List : access Ghdl_Signal_Ptr; El : Ghdl_Signal_Ptr)
                          return Ghdl_Signal_Ptr
   is
      Prev : Ghdl_Signal_Ptr;
   begin
      Prev := List.all;
      List.all := El;
      return Prev;
   end Atomic_Insert;

   function Atomic_Inc (Val : access Natural) return Natural
   is
      Res : Natural;
   begin
      Res := Val.all;
      Val.all := Val.all + 1;
      return Res;
   end Atomic_Inc;

   Current_Process : Process_Acc;

   --  Called by linux.c
   function Grt_Get_Current_Process return Process_Acc;
   pragma Export (C, Grt_Get_Current_Process);

   function Grt_Get_Current_Process return Process_Acc is
   begin
      return Current_Process;
   end Grt_Get_Current_Process;


   procedure Set_Current_Process (Proc : Process_Acc) is
   begin
      Current_Process := Proc;
   end Set_Current_Process;

   function Get_Current_Process return Process_Acc is
   begin
      return Current_Process;
   end Get_Current_Process;

   Stack2 : Stack2_Ptr;

   function Get_Stack2 return Stack2_Ptr is
   begin
      return Stack2;
   end Get_Stack2;

   procedure Set_Stack2 (St : Stack2_Ptr) is
   begin
      Stack2 := St;
   end Set_Stack2;

   Main_Stack : Stack_Type;

   function Get_Main_Stack return Stack_Type is
   begin
      return Main_Stack;
   end Get_Main_Stack;

   procedure Set_Main_Stack (St : Stack_Type) is
   begin
      Main_Stack := St;
   end Set_Main_Stack;
end Grt.Unithread;
