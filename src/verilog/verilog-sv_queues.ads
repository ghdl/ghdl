--  Verilog queues
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with System;
with Ada.Unchecked_Conversion;
with Types; use Types;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Sv_Queues is
   type Sv_Queue is private;

   type Sv_Queue_Ptr is access all Sv_Queue;

   function To_Sv_Queue_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Sv_Queue_Ptr);

   Unlimited : constant Uns32 := Uns32'Last;

   --  Create a new queue.
   function Queue_New (El_Size : Storage_Index;
                       Limit : Uns32;
                       Capacity : Uns32)
                      return Sv_Queue;

   --  Return the current size of the queue.
   function Queue_Size (Q : Sv_Queue) return Uns32;

   --  Return the address of element at index IDX, or No_Data_Ptr if out of
   --  bounds.
   function Queue_Index (Q : Sv_Queue; Idx : Int32) return Data_Ptr;

   --  Allocate one element for Q (at the end), and return its address.
   function Queue_Push_Back (Q : Sv_Queue) return Data_Ptr;

   procedure Queue_Assign (Dest : Sv_Queue; Src : Sv_Queue);

   procedure Delete (Arr : in out Sv_Queue);
private
   type Sv_Queue_Type is record
      --  Size of one element.
      El_Size : Storage_Index;
      --  Limit (from declaration).
      Limit : Uns32;
      --  Maximum number of elements in the buffer.
      Capacity : Uns32;
      --  Current number of elements.
      Size : Uns32;
      --  Offset of the first element.
      First : Uns32;
      --  The data.
      Base : Data_Ptr;
   end record;

   type Sv_Queue is access all Sv_Queue_Type;
end Verilog.Sv_Queues;
