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

   Common_Stack2 : constant Stack2_Ptr := Create;

   function Get_Common_Stack2 return Stack2_Ptr is
   begin
      return Common_Stack2;
   end Get_Common_Stack2;
end Grt.Unithread;
