--  Verilog semantic analyzer (types)
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

with Types; use Types;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Sem_Types is
   --  IEEE 1800-2017 6.4 Singular and aggregate types
   --  Types hierarchy:
   --  data types:
   --    aggregate
   --      unpacked structure
   --      unpacked union
   --      unpacked array
   --        fixed-sized unpacked array
   --        variable-sized unpacked array (cd table 7.1)
   --          dynamic arrays
   --          associated arrays
   --          queue
   --    singular
   --      integral types (cf 6.11)
   --        integer data type
   --          logic, bit, reg, integer, time, int, shortint, longint, byte
   --        packed array
   --        packed structure
   --        packed union
   --        enum
   --        time
   --      string
   --      handle (for class, interface)
   --      real, shortreal, realtime
   --      void
   --      chandle
   --      event
   --      other types (?)

   --  IEEE1800-2017 6.24.3 Bit-stream casting
   --  bit-stream types:
   --    any integral, packed or string type
   --    unpacked arrays, structures or classes of the preceding types
   --    dynamically sized arrays (dynamic, associative or queues) of any of
   --      the preceding types.

   --  Create base types.
   --  Must be called before parse_file.
   procedure Create_Basetypes;

   --  IEEE 1800-2012 6.11.1 Integral types
   --  The term integral is used thoughout this standard to refer to the data
   --  types that can represent a single basic integer data type, packed array,
   --  packed structure, packed union, enum variable, or time variable.
   function Is_Integral_Type (Atype : Node) return Boolean;

   --  Real or shortreal.
   function Is_Float_Type (Atype : Node) return Boolean;

   function Is_Class_Type (Atype : Node) return Boolean;
   function Is_Class_Or_Null_Type (Atype : Node) return Boolean;

   --  IEEE 1800-2017 7.4 Packed and unpacked arrays
   --  Unpacked arrays may be fixed-size arrays, dynamic arrays, associative
   --  arrays or queues.
   function Is_Unpacked_Array_Type (Atype : Node) return Boolean;

   --  Get the equivalent type for signed [MSB:LSB] EL.
   function Get_Packed_Array_Type
     (Msb : Int32; Lsb : Int32; El : Node; Signed : Boolean) return Node;
   function Get_Array_Type
     (Msb : Int32; Lsb : Int32; El : Node) return Node;

   --  Return the base type of an integral type: bit, logic or packed array.
   --  Return NULL_NODE if not an integral type.
   function Get_Base_Integral_Type (Atype : Node) return Node;

   --  Get the equivalent queue type for element EL.
   function Get_Queue_Type (El : Node; Sz : Int32) return Node;

   --  Return TRUE iff SUB_CLASS is a sub-class of (inherit of) PARENT_CLASS.
   function Is_Subclass_Of (Sub_Class : Node; Parent_Class : Node)
                           return Boolean;

   procedure Sem_Class_Instance (Inst : Node);
   procedure Analyze_Class_Instance (Klass : Node);

   --  Return True iff N is a name of a type.
   function Is_Type_Name (N : Node) return Boolean;

   procedure Sem_Data_Type (Atype : Node);

   --  For arrays of modport/interface.
   function Sem_Unpacked_Dimension (Arr : Node) return Node;

   --  1800-2017 6.22.1 Matching types.
   --  Return True iff L and R are matching data types.
   --  Matching types are used to check that virtual method overrides match
   --  the method (8.20), parameterized classes (8.26), assignment patterns
   --  (10.9), port connection rules for nettypes (23.3.3), tasks and functions
   --  in interfaces (25.7).
   function Are_Matching_Types (L, R : Node) return Boolean;
   pragma Inline (Are_Matching_Types);

   --  1800-2017 6.22.2 Equivalent types.
   --  Return True iff L and R are equivalent data types.
   --  Equivalent types are used for 'ref' arguments (13.5.2).  The rules are
   --  written such that two equivalent types have the same representation if
   --  they could be passed by reference.  Selection of a packed array cannot
   --  be used for ref arguments, so logic/bit may have a different
   --  representation than logic/bit vector.
   function Are_Equivalent_Types (L, R : Node) return Boolean;

   --  1800-2017 6.22.3 Assignment compatible.
   function Insert_Assignment_Compatible
     (Dst_Type : Node; Src : Node; Loc : Node) return Node;

   --  Facility to iterate over all instantiated classes.
   --  Usage:
   --     It : Instance_Class_Iterator
   --     ...
   --     Init_Instance_Class_Iterator (It);
   --     loop
   --       C := Get_Instance_Class_Iterator (It);
   --       exit when C = Null_Node;
   --       ...
   --       Next_Instance_Class_Iterator (It);
   --     end loop;
   type Instance_Class_Iterator is private;
   procedure Init_Instance_Class_Iterator
     (It : out Instance_Class_Iterator);
   procedure Next_Instance_Class_Iterator
     (It : in out Instance_Class_Iterator);
   function Get_Instance_Class_Iterator (It : Instance_Class_Iterator)
                                        return Node;
private
   type Instance_Class_Iterator is record
      Idx : Natural;
   end record;
end Verilog.Sem_Types;
