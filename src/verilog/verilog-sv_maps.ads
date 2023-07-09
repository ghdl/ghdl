--  Verilog associative arrays
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

--  with Verilog.Types; use Verilog.Types;
with Verilog.Storages; use Verilog.Storages;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sv_Maps is
   type Sv_Map is private;

   function New_Sv_Map (Atype : Node) return Sv_Map;

   procedure Set_Map (Map : Sv_Map; Idx : Data_Ptr; Val : Data_Ptr);
   function Get_Map (Map : Sv_Map; Idx : Data_Ptr) return Data_Ptr;

   type Sv_Map_Ptr is access all Sv_Map;
   function To_Sv_Map_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Sv_Map_Ptr);

   --  Iterate over all elements; for the 'foreach' statement.
   type Sv_Map_Iterator is private;
   procedure Iterator_Init (It : out Sv_Map_Iterator; Map : Sv_Map);
   function Iterator_Done (It : Sv_Map_Iterator) return Boolean;
   function Iterator_Get_Index (It : Sv_Map_Iterator) return Data_Ptr;
   procedure Iterator_Next (It : in out Sv_Map_Iterator);
private
   type Map_Node;
   type Map_Node_Acc is access Map_Node;

   type Map_Node is record
      Left, Right : Map_Node_Acc;
      Parent : Map_Node_Acc;
      Idx: Data_Ptr;
      Value : Data_Ptr;
   end record;

   type Sv_Map_Type is record
      Ref : Natural;

      --  Size of one element.
      El_Size : Storage_Index;

      El_Type : Node;

      --  Index type.  Will be Null_Node for wildcard index.
      Idx_Type : Node;

      --  Number of elements.
      Len : Uns32;

      Default : Data_Ptr;

      Top : Map_Node_Acc;
   end record;

   type Sv_Map is access all Sv_Map_Type;

   type Sv_Map_Iterator is record
      --  The next node.
      N : Map_Node_Acc;
   end record;
end Verilog.Sv_Maps;
