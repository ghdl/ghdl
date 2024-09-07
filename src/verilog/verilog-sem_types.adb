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

with Ada.Unchecked_Conversion;
with Interning;
with Name_Table;
with Std_Names;
with Str_Table;
with Hash; use Hash;
with Errorout; use Errorout;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Types; use Verilog.Types;
with Verilog.Standard; use Verilog.Standard;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Errors; use Verilog.Errors;
--  with Verilog.Sem; use Verilog.Sem;
with Verilog.Sem_Decls; use Verilog.Sem_Decls;
with Verilog.Sem_Expr; use Verilog.Sem_Expr;
with Verilog.Sem_Names; use Verilog.Sem_Names;
with Verilog.Sem_Eval; use Verilog.Sem_Eval;
with Verilog.Sem_Instances;
with Verilog.Allocates;

--  General strategy:
--  There is only one node for each equivalent type.
--  FIXME: how to deal with DPI (where byte is different from bit[7:0]) ?
--
--  Forward typedef:
--  Multiple stages.
--  FIXME: constant functions.
--  1) During Resolve_Static_Names, names for types, typedef and constant are
--     resolved.  After this point, any type can be analyzed at any moment.
--     The only errors detected are redefinition of an identifier.
--  2) Sem_Type is called when a type need to be analyzed.  This performs the
--     main work, however classes are not analyzed (as the size of a reference
--     is known).  At this point, size of types is known (except for classes).
--  3) Sem_Class_in_Type analyzes a class or classes referenced (directly or
--     indirectly) by a type.  This is needed to know methods available from
--     a class handle.  This creates class instances.

package body Verilog.Sem_Types is
   procedure Sem_Associative_Array
     (Atype : Node; El_Type : Node; Idx_Type : Node);

   --  The types are internalized (there is one node for all equal types).

   --  Common to packed and unpacked arrays.

   type Array_Params is record
      Msb : Int32;
      Lsb : Int32;
      El : Node;
      Signed : Boolean;
   end record;

   function Array_Hash (Params : Array_Params) return Hash_Value_Type
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Int32, Uns32);
      function To_Uns32 is new Ada.Unchecked_Conversion (Node, Uns32);
      V : Uns32;
   begin
      V := Rotate_Left (To_Uns32 (Params.Msb), 10)
        xor Rotate_Right (To_Uns32 (Params.Lsb), 10)
        xor To_Uns32 (Params.El)
        xor Uns32 (Boolean'Pos (Params.Signed));
      return Hash_Value_Type (V);
   end Array_Hash;

   --  Packed arrays.

   function Packed_Array_Equal (N : Node; Params : Array_Params)
                               return Boolean is
   begin
      return Get_Msb_Cst (N) = Params.Msb
        and then Get_Lsb_Cst (N) = Params.Lsb
        and then Get_Type_Element_Type (N) = Params.El
        and then Get_Signed_Flag (N) = Params.Signed;
   end Packed_Array_Equal;

   function Packed_Array_Build (Params : Array_Params) return Node
   is
      Res : Node;
   begin
      --  Create a new node.
      case Get_Kind (Params.El) is
         when N_Logic_Type
           | N_Log_Packed_Array_Cst =>
            Res := Create_Node (N_Log_Packed_Array_Cst);
         when N_Bit_Type
           | N_Bit_Packed_Array_Cst =>
            Res := Create_Node (N_Bit_Packed_Array_Cst);
         when others =>
            raise Internal_Error;
      end case;
      Set_Msb_Cst (Res, Params.Msb);
      Set_Lsb_Cst (Res, Params.Lsb);
      Set_Type_Element_Type (Res, Params.El);
      Set_Signed_Flag (Res, Params.Signed);
      Set_Type_Width
        (Res, Width_Type (Compute_Length (Params.Msb, Params.Lsb))
           * Get_Type_Width (Params.El));
      Set_Stride_Width (Res, Get_Type_Width (Params.El));

      return Res;
   end Packed_Array_Build;

   package Packed_Array_Interning is new Interning
     (Key_Type => Array_Params,
      Object_Type => Node,
      Hash => Array_Hash,
      Build => Packed_Array_Build,
      Equal => Packed_Array_Equal);

   function Get_Packed_Array_Type
     (Msb : Int32; Lsb : Int32; El : Node; Signed : Boolean) return Node is
   begin
      return Packed_Array_Interning.Get
        ((Msb => Msb, Lsb => Lsb, El => El, Signed => Signed));
   end Get_Packed_Array_Type;

   --  Unpacked arrays.

   function Array_Equal (N : Node; Params : Array_Params) return Boolean is
   begin
      return Get_Msb_Cst (N) = Params.Msb
        and then Get_Lsb_Cst (N) = Params.Lsb
        and then Get_Type_Element_Type (N) = Params.El;
   end Array_Equal;

   function Array_Build (Params : Array_Params) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Array_Cst);
      Set_Msb_Cst (Res, Params.Msb);
      Set_Lsb_Cst (Res, Params.Lsb);
      Set_Type_Element_Type (Res, Params.El);
      return Res;
   end Array_Build;

   package Array_Interning is new Interning
     (Key_Type => Array_Params,
      Object_Type => Node,
      Hash => Array_Hash,
      Build => Array_Build,
      Equal => Array_Equal);

   function Get_Array_Type (Msb : Int32; Lsb : Int32; El : Node) return Node is
   begin
      return Array_Interning.Get
        ((Msb => Msb, Lsb => Lsb, El => El, Signed => False));
   end Get_Array_Type;

   --  Dynamic arrays.

   function Dynamic_Array_Hash (Params : Node) return Hash_Value_Type
   is
      function To_Hash is
         new Ada.Unchecked_Conversion (Node, Hash_Value_Type);
   begin
      return To_Hash (Params);
   end Dynamic_Array_Hash;

   function Dynamic_Array_Build (Params : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Dynamic_Array_Cst);
      Set_Type_Element_Type (Res, Params);
      return Res;
   end Dynamic_Array_Build;

   function Dynamic_Array_Equal (N : Node; Params : Node) return Boolean is
   begin
      return Get_Type_Element_Type (N) = Params;
   end Dynamic_Array_Equal;

   package Dynamic_Array_Interning is new Interning
     (Key_Type => Node,
      Object_Type => Node,
      Hash => Dynamic_Array_Hash,
      Build => Dynamic_Array_Build,
      Equal => Dynamic_Array_Equal);

   function Get_Dynamic_Array_Type (El : Node) return Node is
   begin
      return Dynamic_Array_Interning.Get (El);
   end Get_Dynamic_Array_Type;

   --  Associative arrays

   type Assoc_Array_Params is record
      El_Type : Node;
      Idx_Type : Node;
   end record;

   function Assoc_Array_Hash (Params : Assoc_Array_Params)
                             return Hash_Value_Type
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Node, Uns32);
   begin
      return Hash_Value_Type
        (To_Uns32 (Params.El_Type) xor To_Uns32 (Params.Idx_Type));
   end Assoc_Array_Hash;

   function Assoc_Array_Build (Params : Assoc_Array_Params) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Associative_Array_Cst);
      Set_Type_Index_Type (Res, Params.Idx_Type);
      Set_Type_Element_Type (Res, Params.El_Type);
      return Res;
   end Assoc_Array_Build;

   function Assoc_Array_Equal (N : Node; Params : Assoc_Array_Params)
                              return Boolean is
   begin
      return Get_Type_Element_Type (N) = Params.El_Type
        and then Get_Type_Index_Type (N) = Params.Idx_Type;
   end Assoc_Array_Equal;

   package Assoc_Array_Interning is new Interning
     (Key_Type => Assoc_Array_Params,
      Object_Type => Node,
      Hash => Assoc_Array_Hash,
      Build => Assoc_Array_Build,
      Equal => Assoc_Array_Equal);

   function Get_Assoc_Array_Type (El : Node; Idx : Node) return Node is
   begin
      return Assoc_Array_Interning.Get (Assoc_Array_Params'(El, Idx));
   end Get_Assoc_Array_Type;

   --  Queues.

   type Queue_Params is record
      El_Type : Node;
      Sz : Int32;
   end record;

   function Queue_Hash (Params : Queue_Params) return Hash_Value_Type
   is
      function To_Uns32 is new Ada.Unchecked_Conversion (Node, Uns32);
      function To_Uns32 is new Ada.Unchecked_Conversion (Int32, Uns32);
   begin
      return Hash_Value_Type
        (To_Uns32 (Params.El_Type) xor To_Uns32 (Params.Sz));
   end Queue_Hash;

   function Queue_Build (Params : Queue_Params) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Queue_Cst);
      Set_Type_Element_Type (Res, Params.El_Type);
      Set_Maximum_Size_Cst (Res, Params.Sz);
      return Res;
   end Queue_Build;

   function Queue_Equal (N : Node; Params : Queue_Params) return Boolean is
   begin
      return Get_Type_Element_Type (N) = Params.El_Type
        and then Get_Maximum_Size_Cst (N) = Params.Sz;
   end Queue_Equal;

   package Queue_Interning is new Interning
     (Key_Type => Queue_Params,
      Object_Type => Node,
      Hash => Queue_Hash,
      Build => Queue_Build,
      Equal => Queue_Equal);

   function Get_Queue_Type (El : Node; Sz : Int32) return Node is
   begin
      return Queue_Interning.Get (Queue_Params'(El, Sz));
   end Get_Queue_Type;

   --  Types: integral, real/shortreal, void, chandle, string, event,
   --   aggregates, class.
   --  Aggegregate types: unpacked struct, unpacked array, unpacked union,
   --    dynamic array, associative array, queue.

   function Is_Integral_Type (Atype : Node) return Boolean is
   begin
      case Nkinds_Types (Get_Kind (Atype)) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            return True;
         when N_Logic_Type
           | N_Bit_Type =>
            return True;
         when N_Enum_Type =>
            return True;
         when N_Packed_Struct_Type =>
            return True;
         when N_Real_Type
           | N_Shortreal_Type
           | N_String_Type
           | N_Event_Type
           | N_Void_Type
           | N_Struct_Type
           | N_Union_Type
           | N_Packed_Union_Type
           | N_Array_Cst
           | N_Queue_Cst
           | N_Chandle_Type
           | Nkinds_Class
--           | N_Typedef_Class
           | N_Associative_Array_Cst
           | N_Dynamic_Array_Cst
           | N_Null_Type
           | N_Nature =>
            return False;
         when N_Error_Type =>
            return False;
         when N_Packed_Array
           | N_Array
           | N_Dynamic_Array
           | N_Queue
           | N_Associative_Array
           | N_Class_Instance
           | N_Virtual_Interface =>
            raise Internal_Error;
      end case;
   end Is_Integral_Type;

   function Get_Base_Integral_Type (Atype : Node) return Node is
   begin
      case Nkinds_Types (Get_Kind (Atype)) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            return Atype;
         when N_Logic_Type
           | N_Bit_Type =>
            return Atype;
         when N_Enum_Type =>
            return Get_Enum_Base_Type (Atype);
         when N_Packed_Struct_Type =>
            return Get_Packed_Base_Type (Atype);
         when N_Real_Type
           | N_Shortreal_Type
           | N_String_Type
           | N_Event_Type
           | N_Void_Type
           | N_Struct_Type
           | N_Array_Cst
           | N_Queue
           | Nkinds_Class
--           | N_Typedef_Class
           | N_Associative_Array
           | N_Dynamic_Array =>
            return Null_Node;
         when others =>
            Error_Kind ("get_base_integral_type", Atype);
      end case;
   end Get_Base_Integral_Type;

   function Is_Class_Type (Atype : Node) return Boolean is
   begin
      return Get_Kind (Atype) in Nkinds_Class;
   end Is_Class_Type;

   function Is_Class_Or_Null_Type (Atype : Node) return Boolean is
   begin
      case Get_Kind (Atype) is
         when Nkinds_Class
           | N_Null_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Class_Or_Null_Type;

   function Is_Float_Type (Atype : Node) return Boolean is
   begin
      case Nkinds_Types (Get_Kind (Atype)) is
         when N_Real_Type
           | N_Shortreal_Type =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Float_Type;

   function Is_Unpacked_Array_Type (Atype : Node) return Boolean is
   begin
      case Get_Kind (Atype) is
         when N_Array_Cst
           | N_Dynamic_Array_Cst
           | N_Associative_Array
           | N_Queue_Cst =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Unpacked_Array_Type;

   procedure Sem_Packed_Array_Type (Atype : Node)
   is
      Msb_Expr, Lsb_Expr : Node;
      Msb, Lsb : Int32;
      Res : Node;
      El : Node;
   begin
      --  IEEE 1364-2005 3.3.1 Specifying vectors
      --  Both the msb constant expression and the lsb constant expression
      --  shall be constant integer expressions.  The msb and lsb constant
      --  expressions can be any integer value -- positive, negative or zero.

      Msb_Expr := Get_Msb (Atype);
      Lsb_Expr := Get_Lsb (Atype);
      Msb_Expr := Sem_Expression (Msb_Expr, Null_Node);
      Lsb_Expr := Sem_Expression (Lsb_Expr, Null_Node);

      if not Get_Is_Constant (Msb_Expr) then
         Error_Msg_Sem_Non_Constant (Msb_Expr, "msb must be constant");
         Msb := 7;
      else
         Msb := Sem_Constant_Integer_Expression (Msb_Expr);
      end if;
      if not Get_Is_Constant (Lsb_Expr) then
         Error_Msg_Sem_Non_Constant (Lsb_Expr, "lsb must be constant");
         Lsb := 0;
      else
         Lsb := Sem_Constant_Integer_Expression (Lsb_Expr);
      end if;

      El := Get_Element_Data_Type (Atype);
      if Get_Type_Owner (Atype) then
         Sem_Data_Type (El);
      end if;
      El := Get_Expr_Type (El);
      if El = Null_Node then
         Res := Create_Node (N_Error);
         Location_Copy (Res, Atype);
      else
         Res := Get_Packed_Array_Type (Msb, Lsb, El, Get_Signed_Flag (Atype));
      end if;

      Set_Expr_Type (Atype, Res);
   end Sem_Packed_Array_Type;

   function Is_Type_Name (N : Node) return Boolean
   is
      Decl : Node;
   begin
      case Get_Kind (N) is
--         when N_Name =>
--            Decl := Get_Decl (Get_Identifier (N));
         when N_Scoped_Name
           | N_Name =>
            Decl := Get_Declaration (N);
         when N_Class_Instance =>
            return True;
         when others =>
            return False;
      end case;

      if Decl = Null_Node then
         return False;
      end if;

      case Get_Kind (Decl) is
         when N_Typedef
           | N_Class
           | N_Type_Parameter
           | N_Typedef_Class =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Type_Name;

   procedure Sem_Unpacked_Dimension
     (Msb_Expr, Lsb_Expr : Node; Msb : out Int32; Lsb : out Int32)
   is
      Msb_Expr1, Lsb_Expr1 : Node;
   begin
      Msb_Expr1 := Sem_Expression (Msb_Expr, Null_Node);
      if Lsb_Expr /= Null_Node then
         Lsb_Expr1 := Sem_Expression (Lsb_Expr, Null_Node);
      end if;

      Msb := Sem_Constant_Integer_Expression (Msb_Expr1);
      if Lsb_Expr /= Null_Node then
         Lsb := Sem_Constant_Integer_Expression (Lsb_Expr1);
      else
         Lsb := 0;
         Msb := Msb - 1;
      end if;
   end Sem_Unpacked_Dimension;

   function Sem_Unpacked_Dimension (Arr : Node) return Node
   is
      pragma Assert (Get_Kind (Arr) = N_Array);
      Msb_Expr, Lsb_Expr : Node;
      Msb, Lsb : Int32;
      Res : Node;
   begin
      Msb_Expr := Get_Msb (Arr);
      Lsb_Expr := Get_Lsb (Arr);

      Sem_Unpacked_Dimension (Msb_Expr, Lsb_Expr, Msb, Lsb);

      --  TODO: use array interning ?

      Res := Create_Node (N_Array_Cst);
      Location_Copy (Res, Arr);
      Set_Msb_Cst (Res, Msb);
      Set_Lsb_Cst (Res, Lsb);
      Set_Type_Element_Type (Res, Get_Element_Data_Type (Arr));
      Set_Type_Owner (Res, Get_Type_Owner (Arr));

      Free_Node (Arr);
      return Res;
   end Sem_Unpacked_Dimension;

   procedure Sem_Array_Type (Atype : Node)
   is
      Msb_Expr, Lsb_Expr : Node;
      Msb, Lsb : Int32;
      Res : Node;
      El : Node;
      Idx : Node;
   begin
      --  Analyze element type.  As it can be a typedef, always analyze it.
      El := Get_Element_Data_Type (Atype);
      if Get_Type_Owner (Atype) then
         Sem_Data_Type (El);
      end if;
      El := Get_Expr_Type (El);

      Msb_Expr := Get_Msb (Atype);
      Lsb_Expr := Get_Lsb (Atype);

      if Is_Type_Name (Msb_Expr) then
         --  Ah, this is not an array type but an associative array.
         if Lsb_Expr /= Null_Node then
            Error_Msg_Sem
              (+Lsb_Expr, "':' is not allowed for associative array");
         end if;
         --  The index type is owned (as this is a name).
         Idx := Msb_Expr;
         Sem_Data_Type (Idx);
         Sem_Associative_Array (Atype, El, Get_Expr_Type (Idx));
         return;
      end if;

      Sem_Unpacked_Dimension (Msb_Expr, Lsb_Expr, Msb, Lsb);

      --  Intern.
      Res := Get_Array_Type (Msb, Lsb, El);
      Set_Expr_Type (Atype, Res);
   end Sem_Array_Type;

   --  1800-2017 7.5 Dynamic arrays
   procedure Sem_Dynamic_Array_Type (Atype : Node)
   is
      El_Type : Node;
      Res : Node;
   begin
      --  1800-2017 7.5 Dynamic arrays
      --  Dynamic arrays support all variable data types as element types,
      --  including arrays.
      El_Type := Get_Element_Data_Type (Atype);
      if Get_Type_Owner (Atype) then
         Sem_Data_Type (El_Type);
      end if;
      El_Type := Get_Expr_Type (El_Type);

      Res := Get_Dynamic_Array_Type (El_Type);
      Set_Expr_Type (Atype, Res);
   end Sem_Dynamic_Array_Type;

   procedure Sem_Member (Member : Node)
   is
      Mtype : Node;
      Expr : Node;
   begin
      Sem_Decl_Data_Type (Member);
      Mtype := Get_Data_Type (Member);

      Expr := Get_Expression (Member);
      if Expr /= Null_Node then
         Expr := Sem_Expression (Expr, Mtype);
         Set_Expression (Member, Expr);
      end if;
   end Sem_Member;

   procedure Sem_Struct_Type (Atype : Node)
   is
      Member : Node;
      Idx : Int32;
   begin
      Member := Get_Members (Atype);
      Idx := 0;
      while Member /= Null_Node loop
         Sem_Member (Member);

         Idx := Idx + 1;
         Set_Member_Index (Member, Idx);

         Member := Get_Chain (Member);
      end loop;

      Set_Nbr_Members (Atype, Idx);
      Set_Expr_Type (Atype, Atype);
   end Sem_Struct_Type;

   procedure Sem_Packed_Struct_Type (Atype : Node)
   is
      Member : Node;
      Mtype : Node;
      El_Type : Node;
      El_Len : Width_Type;
      Len : Uns32;
      El : Node;
      Base : Node;
      Idx : Int32;
   begin
      --  1800-2017 7.2.1 Packed structures
      --  If all data types within a packed structure are 2-state, the
      --  structure as a whole is treated as a 2-state vector.
      El_Type := Unsigned_Bit_Type;
      Len := 0;

      Member := Get_Members (Atype);
      Idx := 0;
      while Member /= Null_Node loop
         Sem_Member (Member);

         Idx := Idx + 1;
         Set_Member_Index (Member, Idx);

         --  1800-2017 7.2.1 Packed structures
         --  Only packed data types and the integer data types summarized in
         --  Table 6-8 shall be legal in packed structures.
         Mtype := Get_Type_Data_Type (Member);
         if Mtype /= Null_Node then
            case Get_Kind (Mtype) is
               when N_Log_Packed_Array_Cst
                  | N_Bit_Packed_Array_Cst =>
                  El := Get_Type_Element_Type (Mtype);
                  El_Len := Get_Type_Width (Mtype);
               when N_Logic_Type
                 | N_Bit_Type =>
                  El := Mtype;
                  El_Len := 1;
               when N_Packed_Struct_Type =>
                  El := Get_Type_Element_Type (Get_Packed_Base_Type (Mtype));
                  El_Len := Get_Type_Width (Mtype);
               when N_Enum_Type =>
                  El := Get_Type_Element_Type (Get_Enum_Base_Type (Mtype));
                  El_Len := Get_Type_Width (Mtype);
               when others =>
                  Error_Msg_Sem
                    (+Member,
                     "member of packed structure must be an integral type");
                  El := Unsigned_Bit_Type;
                  El_Len := 1;
            end case;
            Len := Len + Uns32 (El_Len);

            --  This is not the final value as it is in reverse order.
            Set_Packed_Member_Offset (Member, Len);

            --  1800-2017 7.2.1 Packed structures
            --  If any data type within a packed structure is 4-state, the
            --  structure as a whole is treated as a 4-state vector.
            if Get_Kind (El) = N_Logic_Type then
               El_Type := Unsigned_Logic_Type;
            end if;
         end if;
         Member := Get_Chain (Member);
      end loop;

      Set_Type_Width (Atype, Width_Type (Len));
      Set_Nbr_Members (Atype, Idx);

      --  1800-2017 7.2.1 Packed structures
      --  The first member specified is the most significant and subsequent
      --  members follow in decreasing significance.
      Member := Get_Members (Atype);
      while Member /= Null_Node loop
         Set_Packed_Member_Offset
           (Member, Len - Get_Packed_Member_Offset (Member));
         Member := Get_Chain (Member);
      end loop;

      Base := Get_Packed_Array_Type
        (Int32 (Len - 1), 0, El_Type, Get_Signed_Flag (Atype));
      Set_Packed_Base_Type (Atype, Base);

      Set_Expr_Type (Atype, Atype);
   end Sem_Packed_Struct_Type;

   procedure Sem_Queue_Type (Atype : Node)
   is
      El : Node;
      Sz : Node;
      Sz_Int : Int32;
      Res : Node;
   begin
      El := Get_Element_Data_Type (Atype);
      if Get_Type_Owner (Atype) then
         Sem_Data_Type (El);
      end if;
      El := Get_Expr_Type (El);

      Sz := Get_Maximum_Size_Expr (Atype);
      if Sz /= Null_Node then
         Sz := Sem_Expression (Sz, Null_Node);
         Sz_Int := Sem_Constant_Integer_Expression (Sz);

         --  1800-2017 7.10 Queues
         --  constant_expression shall evaluate to a positive integer value.
         if Sz_Int <= 0 then
            Error_Msg_Sem (+Sz, "queue maximum size must be positive");
            Sz_Int := -1;
         end if;
      else
         Sz_Int := -1;
      end if;

      Res := Get_Queue_Type (El, Sz_Int);
      Set_Expr_Type (Atype, Res);
   end Sem_Queue_Type;

   procedure Sem_Enum_Type (Atype : Node)
   is
      Base_Type : Node;
      Base : Node;
      El : Node;
      Expr : Node;
      Val : Node;
   begin
      Base := Get_Enum_Base_Data_Type (Atype);
      if Base /= Null_Node and then Get_Type_Owner (Atype) then
         Sem_Data_Type (Base);
      end if;
      if Base /= Null_Node then
         Base_Type := Get_Expr_Type (Base);

         --  1800-2017 6.19 Enumerations
         --  enum_base_type ::=
         --      integer_atom_type [signing ]
         --    | integer_vector_type [signing ] [ packed_dimension ]
         --    | type_identifier [ packed_dimension ] (15)
         -- 15) A type_identifier shall be legal as an enum_base_type if it
         --     denotes an integer_atom_type, with which an additional
         --     packed dimension is not permitted, or an integer_vector_type.
         case Get_Kind (Base_Type) is
            when N_Log_Packed_Array_Cst
              | N_Bit_Packed_Array_Cst =>
               null;
            when N_Logic_Type
              | N_Bit_Type =>
               null;
            when others =>
               Error_Msg_Sem (+Atype, "enum base type must be a vector type");
               Base_Type := Signed_Int_Type;
         end case;
      else
         --  1800-2017 6.19 Enumerations
         --  In the absence of data type declaration, the default data type
         --  shall be INT.
         Base_Type := Signed_Int_Type;
      end if;
      Set_Enum_Base_Type (Atype, Base_Type);
      Set_Type_Width (Atype, Get_Type_Width (Base_Type));
      Set_Signed_Flag (Atype, Get_Signed_Flag (Base_Type));

      El := Get_Enum_Names (Atype);
      Val := Null_Node;
      while El /= Null_Node loop
         --  Ranges have been expanded by sem_name.
         pragma Assert (Get_Kind (El) = N_Enum_Name);

         Set_Expr_Type (El, Atype);

         Expr := Get_Expression (El);
         if Expr /= Null_Node then
            --  1800-2017 6.19 Enumerations
            --  If the integer value expression is a sized literal constant,
            --  it shall be an error if the size is different from the enum
            --  base type, even if the value is within the representable
            --  range.
            if Get_Kind (Expr) = N_Number
              and then Get_Number_Size (Expr) /= 0
              and then Get_Number_Size (Expr) /= Get_Type_Width (Base_Type)
            then
               Error_Msg_Sem
                 (+Expr, "size of number is different from enum base type");
            end if;

            Expr := Sem_Constant_Expression (Expr, Null_Node);

            if Expr /= Null_Node then
               --  1800-2017 6.19 Enumerations
               --  An enumerated name with x or z assignments assigned to an
               --  enum with no explicit data type or an explicit 2-state
               --  declaration shall be a syntax error.
               --
               --  An enum declaration of a 4-state type, such as integer, tha
               --  includes one or more names with x or z assignments shall be
               --  permitted.
               if Get_Kind (Base_Type) = N_Bit_Packed_Array_Cst
                 and then Has_Number_X_Z (Expr)
               then
                  Error_Msg_Sem (+Expr, "2-state enum cannot have x/z");
               end if;

               --  1800-2017 6.19 Enumerations
               --  Any enumeration value that is outside the representable
               --  range of the enum base type shall be an error.  For an
               --  unsigned base type, this occurs if the case truncates the
               --  value and any of the discarded bits are nonzero.  For a
               --  signed base type, this occurs if the case truncates the
               --  value and any of the discarded bits are not equal to the
               --  sign bit of the result.

               --  TODO: check VAL is within the bounds of Base_Type.

               Set_Expr_Type (Expr, Atype);
            end if;
         else
            if Val = Null_Node then
               --  1800-2017 6.19 Enumerations
               --  The values [can be cast to integer types and] increment from
               --  an initial value of 0.
               --
               --  If the first name is not assigned a value, it is given the
               --  initial value of 0.
               --
               --  GHDL: repeated to be clear!
               Expr := Build_Number (0, Atype, Get_Location (El));
            else
               --  1800-2017 6.19 Enumerations
               --  An unassigned enumerated name that follows an enum name with
               --  x or z assignments shall be a syntax error.
               if Has_Number_X_Z (Val) then
                  Error_Msg_Sem (+Val, "previous value cannot have x/z");
               end if;

               --  1800-2017 6.19 Enumerations
               --  It shall be an error to automatically increment the maximum
               --  representable value of the enum.
               --  TODO: check VAL is not the highest value of Base_Type.

               --  1800-2017 6.19 Enumerations
               --  The values [can be cast to integer types and] increment from
               --  an initial value of 0.
               --
               --  A name without a value is automatically assigned an
               --  increment of the value of the previous name.
               Expr := Build_Add (Val, 1, Get_Location (El));
            end if;
         end if;

         --  1800-2017 6.19 Enumerations
         --  The value after the cast is the value used for the name, including
         --  in the uniqueness check and automatic incrementing to get a value
         --  for the next name.

         Set_Expression (El, Expr);
         Val := Expr;

         El := Get_Chain (El);
      end loop;

      --  1800-2017 6.19 Enumerations
      --  Both the enumeration names and their integer values shall be unique.
      --  It shall be an error to set two values to the same name or to set the
      --  same value to two names, regardless of whether the values are set
      --  explicitly or by automatic incrementing.
      --  TODO: check value uniqueness.

      Set_Expr_Type (Atype, Atype);
   end Sem_Enum_Type;

   --  1800-2017 7.8 Associative arrays
   procedure Sem_Associative_Array
     (Atype : Node; El_Type : Node; Idx_Type : Node)
   is
      Res : Node;
   begin
      if Idx_Type /= Null_Node then
         case Get_Kind (Idx_Type) is
            when N_String_Type =>
               --  1800-2017 7.8.2 String index
               null;
            when N_Class
              | N_Typedef_Class
              | N_Class_Instance =>
               --  1800-2017 7.8.3 Class index
               null;
            when N_Enum_Type
              | Nkinds_Vector_Types
              | N_Packed_Struct_Type =>
               --  1800-2017 7.8.4 Integral index.
               null;
            when others =>
               Error_Kind ("sem_associative_array", Idx_Type);
         end case;
      end if;

      Res := Get_Assoc_Array_Type (El_Type, Idx_Type);
      Set_Expr_Type (Atype, Res);
   end Sem_Associative_Array;

   procedure Sem_Associative_Array (Atype : Node)
   is
      El_Type : Node;
      Idx_Type : Node;
   begin
      --  1800-2017 7.8 Associative arrays
      --  data_type is the data type of the array elements.  Can be any type
      --  allowed for fixed-size arrays.
      --
      --  1800-2017 7.4.2 Unpacked arrays
      --  Unpacked arrays can be made of any data type.
      El_Type := Get_Element_Data_Type (Atype);
      if Get_Type_Owner (Atype) then
         Sem_Data_Type (El_Type);
      end if;
      El_Type := Get_Expr_Type (El_Type);

      --  FIXME: unify type.

      Idx_Type := Get_Index_Data_Type (Atype);
      if Idx_Type = Null_Node then
         --  1800-2017 7.8 Associative arrays
         --  If * is specified, then the array is indexed by any integral
         --  expression of arbitrary type.

         --  1800-2017 7.8.1 Wildcard index type
         null;
      else
         if Get_Type_Owner_2 (Atype) then
            Sem_Data_Type (Idx_Type);
         end if;
         Idx_Type := Get_Expr_Type (Idx_Type);
      end if;

      Sem_Associative_Array (Atype, El_Type, Idx_Type);
   end Sem_Associative_Array;

   --  1800-2017 6.22.1 Matching types
   function Is_Matching_Type (T1 : Node; T2 : Node) return Boolean is
   begin
      if T1 = T2 then
         return True;
      end if;
      if Get_Kind (T1) /= Get_Kind (T2) then
         return False;
      end if;

      case Get_Kind (T1) is
         when N_Class
           | N_Instantiated_Class =>
            --  We know that T1 /= T2.
            return False;
         when N_Bit_Packed_Array_Cst
           | N_Log_Packed_Array_Cst =>
            --  f) Two array types match if they are both packed or both
            --     unpacked, are the same kind of array (fixed size, dynamic,
            --     associative or qeueue), have matching index types (for
            --     associative arrays), and have matching element types.
            --     Fixed-size arrays shall also have the same left and right
            --     range bounds.
            return False;
         when others =>
            Error_Kind ("is_matching_type", T1);
      end case;
   end Is_Matching_Type;

   function Is_Same_Value (V1 : Node; V2 : Node) return Boolean is
   begin
      case Get_Kind (V1) is
         when N_String_Literal =>
            if Get_Kind (V2) /= N_String_Literal then
               return False;
            end if;
            declare
               use Str_Table;
               Len : constant Uns32 := Get_String_Size (V1);
               Lid, Rid : String8_Id;
            begin
               if Get_String_Size (V2) /= Len then
                  return False;
               end if;
               Lid := Get_String_Id (V1);
               Rid := Get_String_Id (V2);
               for I in 1 .. Nat32 (Len) loop
                  if Element_String8 (Lid, I) /= Element_String8 (Rid, I) then
                     return False;
                  end if;
               end loop;
               return True;
            end;
         when others =>
            Error_Kind ("is_same_value", V1);
      end case;
   end Is_Same_Value;

   --  Class

   type Class_Params is record
      --  The 'generic' class.
      Class_Type : Node;
      --  The chain of parameters.
      Params : Node;
      --  Location of the instance.
      Loc : Location_Type;
   end record;

   function Class_Hash (Params : Class_Params) return Hash_Value_Type is
   begin
      --  FIXME: improve
      return Hash_Value_Type (Params.Class_Type);
   end Class_Hash;

   function Class_Equal (N : Node; Params : Class_Params) return Boolean
   is
      Param1, Param2 : Node;
   begin
      pragma Assert (Get_Kind (N) = N_Instantiated_Class);
      if Get_Parent (N) /= Params.Class_Type then
         return False;
      end if;

      Param1 := Get_Parameter_Port_Chain (N);
      Param2 := Params.Params;

      --  1800-2017 8.25 Parameterized classes
      --  Two sets of parameters shall be unique unless all parameters are the
      --  same, as defined by the following rules:
      --  a) A parameter is a type parameter and the two types are matching
      --     types.
      --  b) A parameter is a value parameter and both their type and their
      --     value are the same.
      while Param1 /= Null_Node loop
         pragma Assert (Param2 /= Null_Node);
         case Get_Kind (Param1) is
            when N_Parameter =>
               pragma Assert (Get_Kind (Param2) = N_Parameter);
               if not Is_Matching_Type (Get_Data_Type (Param1),
                                        Get_Data_Type (Param2))
               then
                  return False;
               end if;
               if not Is_Same_Value (Get_Expression (Param1),
                                     Get_Expression (Param2))
               then
                  return False;
               end if;
            when N_Type_Parameter =>
               if not Is_Matching_Type (Get_Parameter_Type (Param1),
                                        Get_Parameter_Type (Param2))
               then
                  return False;
               end if;
            when others =>
               Error_Kind ("class_equal", Param1);
         end case;
         Param1 := Get_Chain (Param1);
         Param2 := Get_Chain (Param2);
      end loop;

      return True;
   end Class_Equal;

   function Class_Build (Params : Class_Params) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Instantiated_Class);
      Set_Parameter_Port_Chain (Res, Params.Params);
      Sem_Instances.Instantiate_Class (Res, Params.Class_Type);
      Set_Parent (Res, Params.Class_Type);
      Set_Location (Res, Params.Loc);

      --  To be analyzed, but after being interned as this class may refer to
      --  itself.

      return Res;
   end Class_Build;

   package Class_Interning is new Interning
     (Key_Type => Class_Params,
      Object_Type => Node,
      Hash => Class_Hash,
      Build => Class_Build,
      Equal => Class_Equal);

   procedure Maybe_Mutate_Parameter_Value
     (Value : in out Node; Param : Node; Head : in out Node; Prev : Node)
   is
      N_Value : Node;
   begin
      --  A type parameter may have been parsed as an expression.
      if Get_Kind (Param) = N_Parameter
        or else Get_Kind (Value) = N_Parameter_Value_Type
      then
         return;
      end if;

      N_Value := Create_Node (N_Parameter_Value_Type);
      Location_Copy (N_Value, Value);
      Set_Identifier (N_Value, Get_Identifier (Value));
      Set_Chain (N_Value, Get_Chain (Value));
      Set_Data_Type (N_Value, Get_Expression (Value));
      Set_Type_Owner (N_Value, True);

      if Value = Head then
         Head := N_Value;
      else
         Set_Chain (Prev, N_Value);
      end if;

      Free_Node (Value);
      Value := N_Value;
   end Maybe_Mutate_Parameter_Value;

   procedure Sem_Class_Instance (Inst : Node)
   is
      Klass : constant Node := Get_Declaration (Get_Class_Name (Inst));
      pragma Assert (not Get_Type_Analyzed_Flag (Klass));
      Params_Decl : constant Node := Get_Parameter_Port_Chain (Klass);

      --  Count number of parameters.
      Nbr_Params : constant Nat32 := Nat32 (Get_Chain_Length (Params_Decl));

      --  Create the array of assocs
      Assocs : Node_Array (1 .. Nbr_Params);

      Values : Node;

      Params_Inst : Node;
      Has_Error : Boolean;
      Value : Node;
      Prev : Node;
      Param : Node;
      Id : Name_Id;
      Idx : Nat32;
      Res : Node;
   begin
      --  Default is error...
      Set_Expr_Type (Inst, Error_Type);

      Values := Get_Parameter_Values (Inst);

      --  Associate actual with formals
      Assocs := (others => Null_Node);

      --  Note: there are no specific rules for the parameters.

      --  1800-2017 13.5.4 Argument binding by name
      --  If both positional and named arguments are specified in a single
      --  subroutine call, then all the positional arguments shall come
      --  before the named arguments.

      --  1800-2017 23.3.2 Module instantiation syntax
      --  Parameter assignments can be named or ordered.

      --  First, by position.
      Idx := Assocs'First;
      Value := Values;
      Prev := Null_Node;
      Param := Params_Decl;
      while Value /= Null_Node loop
         --  Association by name are handled in the second loop.
         exit when Get_Identifier (Value) /= Null_Identifier;

         if Idx > Assocs'Last then
            Error_Msg_Sem (+Value, "extra parameter ignored");
            return;
         else
            Maybe_Mutate_Parameter_Value (Value, Param, Values, Prev);
            Assocs (Idx) := Value;
         end if;

         Idx := Idx + 1;
         Prev := Value;
         Value := Get_Chain (Value);
         Param := Get_Chain (Param);
      end loop;

      --  Then by name.
      while Value /= Null_Node loop
         Id := Get_Identifier (Value);
         if Id = Null_Identifier then
            Error_Msg_Sem (+Value, "positional parameter after named one");
            return;
         else
            Param := Params_Decl;
            Idx := Assocs'First;
            while Param /= Null_Node loop
               exit when Get_Identifier (Param) = Id;
               Idx := Idx + 1;
               Param := Get_Chain (Param);
            end loop;

            if Param = Null_Node then
               Error_Msg_Sem (+Value, "no parameter %i in declaration", +Id);
               return;
            elsif Assocs (Idx) /= Null_Node then
               Error_Msg_Sem (+Value, "parameter %i already associated", +Id);
               return;
            else
               Maybe_Mutate_Parameter_Value (Value, Param, Values, Prev);

               Assocs (Idx) := Value;
            end if;
         end if;

         Prev := Value;
         Value := Get_Chain (Value);
      end loop;

      --  Parameter values may have been mutated.
      Set_Parameter_Values (Inst, Values);

      --  Check missing association
      Has_Error := False;
      Param := Params_Decl;
      for I in Assocs'Range loop
         if Assocs (I) = Null_Node then
            case Get_Kind (Param) is
               when N_Type_Parameter =>
                  if Get_Default_Type (Param) = Null_Node then
                     Error_Msg_Sem (+Inst, "no association for %i", +Param);
                     Has_Error := True;
                  end if;
               when N_Parameter =>
                  if Get_Expression (Param) = Null_Node then
                     Error_Msg_Sem (+Inst, "no association for %i", +Param);
                     Has_Error := True;
                  end if;
               when others =>
                  Error_Kind ("sem_class_instance(default)", Param);
            end case;
         end if;

         Param := Get_Chain (Param);
      end loop;
      pragma Assert (Value = Null_Node);
      if Has_Error then
         return;
      end if;

      --  Analyze actuals.
      Params_Inst := Sem_Instances.Instantiate_Parameters (Params_Decl);
      Param := Params_Inst;
      for I in Assocs'Range loop
         case Get_Kind (Param) is
            when N_Type_Parameter =>
               declare
                  Atype : Node;
               begin
                  if Assocs (I) /= Null_Node then
                     Atype := Get_Data_Type (Assocs (I));

                     if Get_Type_Owner (Assocs (I)) then
                        Sem_Data_Type (Atype);
                     end if;
                     --  FIXME: free default type
                  else
                     Atype := Get_Default_Type (Param);
                     if Get_Type_Owner (Param) then
                        Sem_Data_Type (Atype);
                     end if;
                  end if;
                  Atype := Get_Expr_Type (Atype);
                  if Atype = Null_Node then
                     Has_Error := True;
                  end if;
                  Set_Parameter_Type (Param, Atype);
               end;
            when N_Parameter =>
               declare
                  Atype : Node;
                  Expr : Node;
               begin
                  --  Analyze the optional formal type (which may depend on
                  --  previous parameters).
                  Atype := Get_Data_Type (Param);
                  if Atype /= Null_Node then
                     if Get_Type_Owner (Param) then
                        Sem_Data_Type (Atype);
                     end if;
                     Atype := Get_Expr_Type (Atype);
                     if Atype = Null_Node then
                        Has_Error := True;
                     end if;
                  end if;

                  --  Analyze the actual expression.
                  if Assocs (I) /= Null_Node then
                     Expr := Get_Expression (Assocs (I));
                     --  FIXME: free default expression
                  else
                     Expr := Get_Expression (Param);
                  end if;
                  Expr := Sem_Constant_Expression (Expr, Atype);
                  if Expr = Null_Node then
                     Has_Error := True;
                  end if;
                  Set_Parameter_Expression (Param, Expr);
                  if Atype = Null_Node then
                     Atype := Get_Expr_Type (Expr);
                  end if;
                  Set_Param_Type (Param, Atype);

                  Verilog.Allocates.Allocate_Parameter (Param, Expr);
               end;
            when others =>
               Error_Kind ("sem_class_instance(assoc)", Param);
         end case;
         Param := Get_Chain (Param);
      end loop;
      pragma Assert (Param = Null_Node);

      if Has_Error then
         return;
      end if;

      --  Find unique instance, return it if is exists
      Res := Class_Interning.Get ((Klass,
                                   Params_Inst,
                                   Get_Location (Inst)));

      --  TODO: free params_inst if needed.
      Set_Expr_Type (Inst, Res);
      Set_Declaration (Inst, Res);

      if not Get_Type_Analyzed_Flag (Res) then
         Sem_Decls.Sem_Class_Type (Res);
         --  Sem.Sem_Class_Type_Methods (Res);
      end if;
   end Sem_Class_Instance;

   procedure Analyze_Class_Instance (Klass : Node) is
   begin
      if not Get_Type_Analyzed_Flag (Klass) then
         --  TODO: free params_inst if needed.
         --  finish instantiation, analyze it.
         Sem_Decls.Sem_Class_Type (Klass);
      end if;
   end Analyze_Class_Instance;

   procedure Init_Instance_Class_Iterator
     (It : out Instance_Class_Iterator) is
   begin
      It := (Idx => Natural (Class_Interning.First_Index));
   end Init_Instance_Class_Iterator;

   procedure Next_Instance_Class_Iterator
     (It : in out Instance_Class_Iterator) is
   begin
      It.Idx := It.Idx + 1;
   end Next_Instance_Class_Iterator;

   function Get_Instance_Class_Iterator (It : Instance_Class_Iterator)
                                        return Node
   is
      use Class_Interning;
      Idx : constant Index_Type := Index_Type (It.Idx);
   begin
      if Idx <= Last_Index then
         return Get_By_Index (Idx);
      else
         return Null_Node;
      end if;
   end Get_Instance_Class_Iterator;

   procedure Sem_Type_Name (Name : Node)
   is
      Decl : Node;
   begin
      Decl := Sem_Name (Name);
      pragma Assert (Decl = Name);
      Decl := Get_Declaration (Name);
      if Decl = Null_Node then
         Set_Expr_Type (Name, Error_Type);
         return;
      end if;
      case Get_Kind (Decl) is
         when N_Typedef =>
            if not Get_Resolved_Flag (Decl) then
               Sem_Typedef_Type (Decl);
            end if;
            Set_Expr_Type (Name, Get_Type_Data_Type (Decl));
         when N_Typedef_Forward =>
            declare
               Fwd_Type : Node;
            begin
               Fwd_Type := Get_Forward_Type (Decl);
               --  FIXME: not defined...
               Set_Expr_Type (Name, Fwd_Type);
            end;
         when N_Typedef_Class =>
            declare
               Fwd_Type : Node;
            begin
               Fwd_Type := Get_Forward_Type (Decl);
               --  FIXME: not a class, not defined...
               Set_Expr_Type (Name, Fwd_Type);
            end;
         when N_Typedef_Struct =>
            declare
               Fwd_Type : Node;
            begin
               Fwd_Type := Get_Forward_Type (Decl);
               --  FIXME: not a typedef, not defined...
               if not Get_Resolved_Flag (Fwd_Type) then
                  Sem_Typedef_Type (Fwd_Type);
               end if;
               Set_Expr_Type (Name, Get_Type_Data_Type (Fwd_Type));
            end;
         when N_Class =>
            Set_Expr_Type (Name, Decl);
         when N_Type_Parameter =>
            Set_Expr_Type (Name, Get_Parameter_Type (Decl));
         when N_Modport =>
            Set_Expr_Type (Name, Decl);
         when N_Predefined_Typedef =>
            pragma Assert (Decl = Error_Typedef);
            Set_Expr_Type (Name, Get_Expr_Type (Decl));
         when N_Discipline =>
            Set_Expr_Type (Name, Decl);
         when others =>
            Error_Kind ("sem_type_name", Decl);
      end case;
   end Sem_Type_Name;

   procedure Sem_Data_Type (Atype : Node)
   is
      Res : Node;
   begin
      pragma Assert (Atype /= Null_Node);

      --  A data type must be analyzed only once.
      --  Note: ATYPE must be own-ed by the parent node.
      pragma Assert (Get_Expr_Type (Atype) = Null_Node);

      case Get_Kind (Atype) is
         when N_Predefined_Typedef =>
            raise Program_Error;
         when N_Logic_Type
           | N_Bit_Type
           | N_String_Type
           | N_Real_Type
           | N_Shortreal_Type =>
            raise Internal_Error;
         when N_Packed_Array =>
            Sem_Packed_Array_Type (Atype);
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Array_Cst =>
            raise Internal_Error;
         when N_Enum_Type =>
            Sem_Enum_Type (Atype);
         when N_Array =>
            Sem_Array_Type (Atype);
         when N_Dynamic_Array =>
            Sem_Dynamic_Array_Type (Atype);
         when N_Associative_Array =>
            Sem_Associative_Array (Atype);
         when N_Queue =>
            Sem_Queue_Type (Atype);
         when N_Packed_Struct_Type =>
            Sem_Packed_Struct_Type (Atype);
         when N_Struct_Type =>
            Sem_Struct_Type (Atype);
         when N_Typedef =>
            raise Program_Error;
         when N_Name =>
            Sem_Type_Name (Sem_Name (Atype));
         when N_Scoped_Name
           | N_Dotted_Name =>
            Sem_Type_Name (Atype);
         when N_Class_Instance =>
            Res := Sem_Name (Atype);
            pragma Assert (Res = Null_Node or Res = Atype);
         when N_Class =>
            null;
         when N_Virtual_Interface =>
            declare
               Itf : Node;
               Decl : Node;
            begin
               Itf := Sem_Name (Get_Interface (Atype));
               Decl := Get_Declaration (Itf);
               if Decl = Null_Node then
                  return;
               end if;
               Set_Interface (Atype, Decl);
               case Get_Kind (Decl) is
                  when N_Interface_Declaration =>
                     if Get_Parameter_Port_Chain (Decl) = Null_Node then
                        --  FIXME: make it uniq ?
                        return;
                     else
                        --  TODO: auto-instantiate.
                        Error_Msg_Sem
                          (+Atype, "interface must be instantiated");
                        return;
                     end if;
                  when others =>
                     Error_Kind ("sem_type(virtual_interface)", Decl);
               end case;
            end;
         when others =>
            Error_Kind ("sem_type", Atype);
      end case;
   end Sem_Data_Type;

   function Are_Matching_Types (L, R : Node) return Boolean is
   begin
      --  GHDL: As types are internalized, matching types are unique.

      --  1800-2017 6.22.1 Matching types.
      --  a) Any built-in type matches every other occurrence of itself, in
      --     every scope.
      --  GHDL: they are built once, in Create_Basetypes.
      --
      --  b) A simple typedef or type parameter override that renames a
      --     built-in or user-defined type matches that built-in or
      --     user-defined type within the scope of the type identifier.
      --  GHDL: a typedef doesn't create a type, it just gives a name.
      --  FIXME: what is the scope of an identifier ?  It is defined
      --   somewhere ?
      --
      --  c) An anonymous enum, struct, or union type matches itself among
      --     data objects declared within the same declaration statement and
      --     no other data types.
      --  GHDL: the objects share the same type.
      --
      --  d) A typedef for an enum, struct, union, or class matches itself and
      --     the type of data objects declared using that data type within the
      --     scope of the data type identifier.
      --  GHDL: like b: a typedef just gives a name to a type.
      --
      --  e) A simple bit vector type that does not have a predefined width
      --     and one that does have a predefined width match if both are
      --     2-state or both are 4-state, both are signed or both are unsigned,
      --     both have the same width, and the range of the simple bit vector
      --     type without a predefined width is [width-1:0].
      --  GHDL: This is how predefined bit vector types are defined in
      --    Create_Basetypes.
      --
      --  f) Two array types match if they are both packed or both unpacked,
      --     are the same kind of array (fixed-size, dynamic, associative, or
      --     queue), have matching index types (for associative arrays), and
      --     have matching element types.  Fixed-size arrays shall also have
      --     the same left and right bounds.  Note that the element type of
      --     a multidimensional array is itself an array type.
      --  GHDL: That's how array types are uniquified.
      --
      --  g) Explicitly adding signed or unsigned modifiers to a type that does
      --     not change its default signing creates a type that matches the
      --     type without the explicit signing specification.
      --  GHDL: Signed/unsigned are always considered during unification
      --     whether they are implicit or explicit.
      --
      --  h) A typedef for an enum, struct, union, or class type declared in a
      --     package always matches itself, regardless of the scope into which
      --     the type is imported.
      --  GHDL: Like b) or d).

      return L = R;
   end Are_Matching_Types;

   --  Return N_Logic_Type or N_Bit_Type for integral type T.
   function Get_Bit_Kind (T : Node) return Nkind is
   begin
      case Get_Kind (T) is
         when N_Logic_Type =>
            return N_Logic_Type;
         when N_Bit_Type =>
            return N_Bit_Type;
         when N_Log_Packed_Array_Cst =>
            return N_Logic_Type;
         when N_Bit_Packed_Array_Cst =>
            return N_Bit_Type;
         when N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            return Get_Bit_Kind (Get_Packed_Base_Type (T));
         when others =>
            --  Not an integral type.
            raise Internal_Error;
      end case;
   end Get_Bit_Kind;

   function Are_Equivalent_Types (L, R : Node) return Boolean is
   begin
      --  1800-2017 6.22.2 Equivalent types.
      --  a) If two types match, they are equivalent.
      if Are_Matching_Types (L, R) then
         return True;
      end if;

      case Nkinds_Types (Get_Kind (L)) is
         --  b) An anonynous enum, unpacked struct, or unpacked union type is
         --     equivalent to itself among data objects declared within the
         --     same declaration statement and no other data types.
         --  GHDL: same as matching type.
         when N_Enum_Type
           | N_Struct_Type
           | N_Union_Type =>
            return L = R;

         --  c) Packed arrays, packed structures, packed unions, and
         --     built-in integral types are equivalent if they contain the
         --     same number of total bits, are either all 2-state or all
         --     4-state, and are either all signed or all unsigned.
         when N_Logic_Type
           | N_Bit_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            case Get_Kind (R) is
               when N_Logic_Type
                 | N_Log_Packed_Array_Cst
                 | N_Bit_Type
                 | N_Bit_Packed_Array_Cst
                 | N_Packed_Struct_Type
                 | N_Packed_Union_Type =>
                  null;
               when others =>
                  return False;
            end case;
            return Get_Bit_Kind (L) = Get_Bit_Kind (R)
              and then Get_Type_Width (L) = Get_Type_Width (R)
              and then Get_Signed_Flag (L) = Get_Signed_Flag (R);

         --  d) Unpacked fixed-size array types are equivalent if they have
         --     equivalent element types and equal size; the actual range
         --     bounds may differ.  Note that the element type of a
         --     multidimensional array is itself an array type.
         when N_Array_Cst =>
            return Get_Kind (R) = N_Array_Cst
              and then Compute_Length (L) = Compute_Length (R)
              and then Are_Equivalent_Types (Get_Type_Element_Type (L),
                                             Get_Type_Element_Type (R));

         --  e) Dynamic array, associative array and queue types are
         --     equivalent if they are the same kind of array (dynamic,
         --     associative, or queue), have equivalent index types (for
         --     associative arrays), and have equivalent element types.
         when N_Dynamic_Array_Cst =>
            return Get_Kind (R) = N_Dynamic_Array_Cst
              and then Are_Equivalent_Types (Get_Type_Element_Type (L),
                                             Get_Type_Element_Type (R));
         when N_Associative_Array_Cst =>
            return Get_Kind (R) = N_Associative_Array_Cst
              and then Are_Equivalent_Types (Get_Type_Index_Type (L),
                                             Get_Type_Index_Type (R))
              and then Are_Equivalent_Types (Get_Type_Element_Type (L),
                                             Get_Type_Element_Type (R));
         when N_Queue_Cst =>
            --  GHDL: maximum size doesn't matter.
            return Get_Kind (R) = N_Queue_Cst
              and then Are_Equivalent_Types (Get_Type_Element_Type (L),
                                             Get_Type_Element_Type (R));

         when N_Real_Type
           | N_Shortreal_Type
           | N_String_Type
           | N_Chandle_Type
           | N_Event_Type
           | N_Void_Type
           | N_Nature
           | N_Class
           | N_Instantiated_Class
           | N_Virtual_Interface =>
            --  They are equivalent only if they are matching types.
            return False;

         when N_Error_Type =>
            return True;

         when N_Null_Type =>
            return False;

         when N_Packed_Array
           | N_Array
           | N_Class_Instance
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Queue =>
            --  Not internalized.
            raise Internal_Error;
      end case;
   end Are_Equivalent_Types;

   function Is_Subclass_Of (Sub_Class : Node; Parent_Class : Node)
                           return Boolean
   is
      Parent_Depth : constant Int32 := Get_Inheritance_Depth (Parent_Class);
      Sub_Depth : constant Int32 := Get_Inheritance_Depth (Sub_Class);
      N : Node;
      Depth : Int32;
   begin
      if Sub_Depth < Parent_Depth then
         return False;
      end if;
      N := Sub_Class;
      Depth := Sub_Depth;
      while Depth > Parent_Depth loop
         N := Get_Type_Base_Class_Type (N);
         pragma Assert (Get_Kind (N) in Nkinds_Class);
         Depth := Depth - 1;
      end loop;
      return N = Parent_Class;
   end Is_Subclass_Of;

   function Insert_Assignment_Compatible
     (Dst_Type : Node; Src : Node; Loc : Node) return Node
   is
      Src_Type : constant Node := Get_Expr_Type (Src);
   begin
      if Src_Type = Null_Node then
         --  Propagate error.
         return Src;
      end if;

      --  1800-2017 6.22.3 Assignment compatible
      --  All equivalent types, [...] are assignment-compatible types.
      --  GHDL: without conversion.
      if Are_Equivalent_Types (Dst_Type, Src_Type) then
         return Implicit_Conversion (Src, Dst_Type);
      end if;

      --  1800-2017 6.22.3 Assignment compatible
      --  [...] and all nonequivalent types that have implicit cast rules
      --  defined between them, are assignment-compatible types.
      --
      --  Unpacked arrays are assignment compatible with certain other arrays
      --  that are not of equivalent type.  Assignment compatibility of
      --  unpacked arrays is discussed in detail in 7.6.
      --
      --  Implicit casting rules are defined in 6.24
      --  GHDL: Unfortunately, there is almost no information in 6.24.  FIXME.
      --   But:

      --  1800-2017 5.7.2 Real literal constants
      --  Implicit conversion shall take place when a real number is assigned
      --  to an integer.

      --  1800-2017 6.11.2 2-state and 4-state data types
      --  Automatic type conversions from a smaller number of bits to a
      --  larger number of bits involve zero-extensions if unsigned or sign
      --  extensions if signed.  Automatic type conversion from a larger
      --  number of bits to a smaller number of bits involve truncation of the
      --  most significant bits.  When a 4-state value is automatically
      --  converted to a 2-state value, any unknown or high-impedance bits
      --  shall be converted to zeros.

      --  1800-2017 6.12.2 Conversion
      --  Implicit conversion shall take place when a real number is assigned
      --  to an integer.
      --  Implicit conversion shall also take place when an expression is
      --  assigned to a real.

      case Nkinds_Types (Get_Kind (Src_Type)) is
         when N_Logic_Type
           | N_Bit_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Packed_Struct_Type
           | N_Packed_Union_Type =>
            case Nkinds_Types (Get_Kind (Dst_Type)) is
               when N_Error_Type =>
                  --  Always equivalent.
                  raise Internal_Error;
               when N_Logic_Type
                 | N_Bit_Type
                 | N_Real_Type
                 | N_Shortreal_Type
                 | N_Nature
                 | N_Log_Packed_Array_Cst
                 | N_Bit_Packed_Array_Cst
                 | N_Packed_Struct_Type
                 | N_Packed_Union_Type =>
                  --  1800-2017 6.22.3 Assignment compatible
                  --  For example, all integral types are assignment
                  --  compatible.
                  --  GHDL: FIXME: need more ref.
                  return Implicit_Conversion (Src, Dst_Type);
               when N_Enum_Type =>
                  Error_Msg_Sem
                    (+Loc,
                     "no implicit conversion from integral type to enum");
                  return Src;
               when N_String_Type
                 | N_Chandle_Type
                 | N_Event_Type
                 | N_Void_Type
                 | N_Union_Type
                 | N_Struct_Type
                 | N_Array_Cst
                 | N_Queue_Cst
                 | N_Dynamic_Array_Cst
                 | N_Associative_Array_Cst
                 | N_Virtual_Interface
                 | N_Class
                 | N_Instantiated_Class
                 | N_Null_Type =>
                  Error_Msg_Sem (+Loc, "incompatible type for assignment "
                                   & "to an non-integral object");
                  return Src;
               when N_Array
                 | N_Packed_Array
                 | N_Class_Instance
                 | N_Dynamic_Array
                 | N_Associative_Array
                 | N_Queue =>
                  --  Type must have been internalized.
                  raise Internal_Error;
            end case;
         when N_Enum_Type =>
            if not Is_Integral_Type (Dst_Type) then
               Error_Msg_Sem (+Loc, "incompatible types for assignment");
               return Src;
            elsif Get_Kind (Dst_Type) = N_Enum_Type then
               Error_Msg_Sem (+Loc, "incompatible enum types for assignment");
               return Src;
            else
               return Implicit_Conversion (Src, Dst_Type);
            end if;
         when N_Array_Cst
           | N_Dynamic_Array_Cst
           | N_Queue_Cst =>
            --  1800-2017 7.6 Array assignments
            --  A fixed-size unpacked array, dynamic array, or queue, or a
            --  slice of such an array, shall be assignment compatible with
            --  any other such array or slice if all the following conditions
            --  are satisfied:
            --  -  The element types of source and target shall be equivalent.
            --  -  If the target is a fixed-size array or a slice, the
            --     source array shall have the same number of elements as the
            --     target.
            case Get_Kind (Dst_Type) is
               when N_Array_Cst
                 | N_Dynamic_Array_Cst
                 | N_Queue_Cst =>
                  if not Are_Equivalent_Types
                    (Get_Type_Element_Type (Dst_Type),
                     Get_Type_Element_Type (Src_Type))
                  then
                     Error_Msg_Sem (+Loc, "element types are not equivalent");
                     return Src;
                  end if;
                  if Get_Kind (Dst_Type) = N_Array_Cst
                    and then Get_Kind (Src_Type) = N_Array_Cst
                  then
                     --  TODO: check size.
                     Error_Msg_Sem
                       (+Loc, "incompatible array size for assignment");
                  end if;
                  return Src;
               when others =>
                  Error_Msg_Sem (+Loc, "incompatible types for assignment");
                  return Src;
            end case;
         when N_Associative_Array_Cst =>
            --  1800-2017 7.6 Array assignments
            --  Associative arrays are assignment compatible only with
            --  associative arrays, as described in 7.9.9.
            --  TODO.
            raise Internal_Error;
         when N_Class
           | N_Instantiated_Class =>
            if Get_Kind (Src_Type) not in Nkinds_Class then
               Error_Msg_Sem (+Loc, "only a class can be assigned to a class");
               return Src;
            end if;
            if not Is_Subclass_Of (Src_Type, Dst_Type) then
               --  TODO: improve error message ?
               Error_Msg_Sem
                 (+Loc, "source class is not a parent of target class");
            end if;
            return Src;
         when N_String_Type
           | N_Chandle_Type
           | N_Event_Type
           | N_Void_Type
           | N_Nature
           | N_Struct_Type
           | N_Virtual_Interface
           | N_Union_Type =>
            Error_Msg_Sem (+Loc, "incompatible types for assignment");
            return Src;
         when N_Null_Type =>
            --  TODO.
            raise Internal_Error;
         when N_Error_Type =>
            return Src;
         when N_Packed_Array
           | N_Array
           | N_Class_Instance
           | N_Dynamic_Array
           | N_Associative_Array
           | N_Queue =>
            --  Only internalized types are allowed.
            raise Internal_Error;
      end case;
   end Insert_Assignment_Compatible;

   procedure Create_Basetypes
   is
      use Std_Names;
      use Name_Table;
      N : Node;
      Arg, Args : Node;
   begin
      Packed_Array_Interning.Init;
      Array_Interning.Init;
      Dynamic_Array_Interning.Init;
      Assoc_Array_Interning.Init;
      Queue_Interning.Init;
      Class_Interning.Init;

      N := Create_Node (N_Logic_Type);
      pragma Assert (N = Unsigned_Logic_Type);
      Set_Signed_Flag (N, False);
      Set_Type_Width (N, 1);

      N := Create_Node (N_Logic_Type);
      pragma Assert (N = Signed_Logic_Type);
      Set_Signed_Flag (N, True);
      Set_Type_Width (N, 1);

      N := Create_Node (N_Bit_Type);
      pragma Assert (N = Unsigned_Bit_Type);
      Set_Signed_Flag (N, False);
      Set_Type_Width (N, 1);

      N := Create_Node (N_Bit_Type);
      pragma Assert (N = Signed_Bit_Type);
      Set_Signed_Flag (N, True);
      Set_Type_Width (N, 1);

      N := Create_Node (N_Real_Type);
      pragma Assert (N = Real_Type);
      Set_Signed_Flag (N, True);
      Set_Type_Width (N, 64);

      N := Create_Node (N_Shortreal_Type);
      pragma Assert (N = Shortreal_Type);
      Set_Signed_Flag (N, True);
      Set_Type_Width (N, 32);

      --  Integer types
      N := Get_Packed_Array_Type (7, 0, Unsigned_Bit_Type, False);
      pragma Assert (N = Unsigned_Byte_Type);

      N := Get_Packed_Array_Type (7, 0, Unsigned_Bit_Type, True);
      pragma Assert (N = Signed_Byte_Type);

      N := Get_Packed_Array_Type (15, 0, Unsigned_Bit_Type, False);
      pragma Assert (N = Unsigned_Shortint_Type);

      N := Get_Packed_Array_Type (15, 0, Unsigned_Bit_Type, True);
      pragma Assert (N = Signed_Shortint_Type);

      N := Get_Packed_Array_Type (31, 0, Unsigned_Bit_Type, False);
      pragma Assert (N = Unsigned_Int_Type);

      N := Get_Packed_Array_Type (31, 0, Unsigned_Bit_Type, True);
      pragma Assert (N = Signed_Int_Type);

      N := Get_Packed_Array_Type (63, 0, Unsigned_Bit_Type, False);
      pragma Assert (N = Unsigned_Longint_Type);

      N := Get_Packed_Array_Type (63, 0, Unsigned_Bit_Type, True);
      pragma Assert (N = Signed_Longint_Type);

      N := Get_Packed_Array_Type (31, 0, Unsigned_Logic_Type, False);
      pragma Assert (N = Unsigned_Integer_Type);

      N := Get_Packed_Array_Type (31, 0, Unsigned_Logic_Type, True);
      pragma Assert (N = Signed_Integer_Type);

      N := Get_Packed_Array_Type (63, 0, Unsigned_Logic_Type, False);
      pragma Assert (N = Unsigned_Time_Type);

      N := Get_Packed_Array_Type (63, 0, Unsigned_Logic_Type, True);
      pragma Assert (N = Signed_Time_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Logic_Typedef);
      Set_Identifier (N, Get_Identifier ("logic signed"));
      Set_Expr_Type (N, Signed_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Logic_Typedef);
      Set_Identifier (N, Get_Identifier ("logic unsigned"));
      Set_Expr_Type (N, Unsigned_Logic_Type);

      --  1800-2017 6.11 Integer data types
      --  INTEGER 4-state data type, 32-bit signed integer
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Integer_Typedef);
      Set_Identifier (N, Name_Integer);
      Set_Expr_Type (N, Signed_Integer_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Integer_Typedef);
      Set_Identifier (N, Get_Identifier ("integer signed"));
      Set_Expr_Type (N, Signed_Integer_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Integer_Typedef);
      Set_Identifier (N, Get_Identifier ("integer unsigned"));
      Set_Expr_Type (N, Unsigned_Integer_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Reg_Typedef);
      Set_Identifier (N, Std_Names.Name_Reg);
      Set_Expr_Type (N, Unsigned_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Reg_Typedef);
      Set_Identifier (N, Get_Identifier ("reg signed"));
      Set_Expr_Type (N, Signed_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Reg_Typedef);
      Set_Identifier (N, Get_Identifier ("reg unsigned"));
      Set_Expr_Type (N, Unsigned_Logic_Type);

      --  TIME 4-state data type, 64-bit unsigned integer
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Time_Typedef);
      Set_Identifier (N, Std_Names.Name_Time);
      Set_Expr_Type (N, Unsigned_Time_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Time_Typedef);
      Set_Identifier (N, Get_Identifier ("time signed"));
      Set_Expr_Type (N, Signed_Time_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Time_Typedef);
      Set_Identifier (N, Get_Identifier ("time unsigned"));
      Set_Expr_Type (N, Unsigned_Time_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Bit_Typedef);
      Set_Identifier (N, Get_Identifier ("bit signed"));
      Set_Expr_Type (N, Signed_Bit_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Bit_Typedef);
      Set_Identifier (N, Get_Identifier ("bit unsigned"));
      Set_Expr_Type (N, Unsigned_Bit_Type);

      --  BYTE 2-state data type, 8-bit signed integer or ASCII character
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Byte_Typedef);
      Set_Identifier (N, Std_Names.Name_Byte);
      Set_Expr_Type (N, Signed_Byte_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Byte_Typedef);
      Set_Identifier (N, Get_Identifier ("byte signed"));
      Set_Expr_Type (N, Signed_Byte_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Byte_Typedef);
      Set_Identifier (N, Get_Identifier ("byte unsigned"));
      Set_Expr_Type (N, Unsigned_Byte_Type);

      --  SHORTINT 2-state data type, 16-bit signed integer
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Shortint_Typedef);
      Set_Identifier (N, Std_Names.Name_Shortint);
      Set_Expr_Type (N, Signed_Shortint_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Shortint_Typedef);
      Set_Identifier (N, Get_Identifier ("shortint signed"));
      Set_Expr_Type (N, Signed_Shortint_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Shortint_Typedef);
      Set_Identifier (N, Get_Identifier ("shortint unsigned"));
      Set_Expr_Type (N, Unsigned_Shortint_Type);

      --  INT 2-state data type, 32-bit signed integer
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Int_Typedef);
      Set_Identifier (N, Std_Names.Name_Int);
      Set_Expr_Type (N, Signed_Int_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Int_Typedef);
      Set_Identifier (N, Get_Identifier ("int signed"));
      Set_Expr_Type (N, Signed_Int_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Int_Typedef);
      Set_Identifier (N, Get_Identifier ("int unsigned"));
      Set_Expr_Type (N, Unsigned_Int_Type);

      --  LONGINT 2-state data type, 64-bit signed integer
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Longint_Typedef);
      Set_Identifier (N, Std_Names.Name_Longint);
      Set_Expr_Type (N, Signed_Longint_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Signed_Longint_Typedef);
      Set_Identifier (N, Get_Identifier ("longint signed"));
      Set_Expr_Type (N, Signed_Longint_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Unsigned_Longint_Typedef);
      Set_Identifier (N, Get_Identifier ("longint unsigned"));
      Set_Expr_Type (N, Unsigned_Longint_Type);

      --  Implicit
      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Implicit_Typedef);
      Set_Identifier (N, No_Name_Id);
      Set_Expr_Type (N, Unsigned_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Implicit_Unsigned_Typedef);
      Set_Identifier (N, Name_Unsigned);
      Set_Expr_Type (N, Unsigned_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Implicit_Signed_Typedef);
      Set_Identifier (N, Name_Signed);
      Set_Expr_Type (N, Signed_Logic_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Real_Typedef);
      Set_Identifier (N, Name_Real);
      Set_Expr_Type (N, Real_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Shortreal_Typedef);
      Set_Identifier (N, Name_Shortreal);
      Set_Expr_Type (N, Shortreal_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Realtime_Typedef);
      Set_Identifier (N, Name_Realtime);
      Set_Expr_Type (N, Realtime_Type);

      N := Create_Node (N_String_Type);
      pragma Assert (N = String_Type);

      N := Create_Node (N_Chandle_Type);
      pragma Assert (N = Chandle_Type);

      N := Create_Node (N_Event_Type);
      pragma Assert (N = Event_Type);

      N := Create_Node (N_Void_Type);
      pragma Assert (N = Void_Type);

      N := Create_Node (N_Error_Type);
      Set_Type_Width (N, 1);
      pragma Assert (N = Error_Type);

      N := Create_Node (N_Null_Type);
      pragma Assert (N = Null_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = String_Typedef);
      Set_Identifier (N, Name_String);
      Set_Expr_Type (N, String_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Chandle_Typedef);
      Set_Identifier (N, Name_Chandle);
      Set_Expr_Type (N, Chandle_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Event_Typedef);
      Set_Identifier (N, Name_Event);
      Set_Expr_Type (N, Event_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Void_Typedef);
      Set_Identifier (N, Name_Void);
      Set_Expr_Type (N, Void_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Error_Typedef);
      Set_Identifier (N, Name_Error);
      Set_Expr_Type (N, Error_Type);

      --  Wildcard types.
      N := Create_Node (N_Wildcard_Type);
      pragma Assert (N = Wildcard_Type_Type);

      N := Create_Node (N_Wildcard_Type);
      pragma Assert (N = Wildcard_Element_Type);

      N := Create_Node (N_Wildcard_Type);
      pragma Assert (N = Wildcard_Index_Type);

      N := Create_Node (N_Wildcard_Type);
      pragma Assert (N = Wildcard_Element_Queue_Type);

      N := Create_Node (N_Wildcard_Type);
      pragma Assert (N = Wildcard_Index_Queue_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Wildcard_Type_Typedef);
      Set_Expr_Type (N, Wildcard_Type_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Wildcard_Element_Typedef);
      Set_Expr_Type (N, Wildcard_Element_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Wildcard_Index_Typedef);
      Set_Expr_Type (N, Wildcard_Index_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Wildcard_Element_Queue_Typedef);
      Set_Expr_Type (N, Wildcard_Element_Queue_Type);

      N := Create_Node (N_Predefined_Typedef);
      pragma Assert (N = Wildcard_Index_Queue_Typedef);
      Set_Expr_Type (N, Wildcard_Index_Queue_Type);

      --  String methods
      --  1800-2017 6.16.1 len()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Len);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Len_Method);

      --  1800-2017 6.16.8 Substr()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Substr);
      Set_Data_Type (N, String_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Substr_Method);

      --  1800-2017 6.16.9 Atoi(), atoihex(), atooct(), atobin()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Atoi);
      Set_Data_Type (N, Integer_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Atoi_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Atohex);
      Set_Data_Type (N, Integer_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Atohex_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Atooct);
      Set_Data_Type (N, Integer_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Atooct_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Atobin);
      Set_Data_Type (N, Integer_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Atobin_Method);

      --  1800-2017 6.16.11 Itoa()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Itoa);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Itoa_Method);

      --  1800-2017 6.16.4 Toupper()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Toupper);
      Set_Data_Type (N, String_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Toupper_Method);

      --  1800-2017 6.16.4 Tolower()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Tolower);
      Set_Data_Type (N, String_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = String_Tolower_Method);

      --  Enum methods
      --  First
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_First);
      Set_Data_Type (N, Wildcard_Type_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_First_Method);

      --  Last
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Last);
      Set_Data_Type (N, Wildcard_Type_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_Last_Method);

      --  Next
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Next);
      Set_Data_Type (N, Wildcard_Type_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_Next_Method);

      --  Prev
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Prev);
      Set_Data_Type (N, Wildcard_Type_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_Prev_Method);

      --  Num
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Num);
      Set_Data_Type (N, Signed_Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_Num_Method);

      --  Name
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Name);
      Set_Data_Type (N, String_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Enum_Name_Method);

      --  Queue methods
      --  1800-2017 7.10.2.1 Size()
      --  function int size();
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Size);
      Set_Data_Type (N, Signed_Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Size_Method);

      --  1800-2017 7.10.2.2 Insert()
      --  function void insert (input integer index, input element_t item);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Insert);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Insert_Method);

      --  1800-2017 7.10.2.3 Delete()
      --  functin void delete( [input integer index] );
      --  1) without argument
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Delete);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Delete_Method);

      --  2) with argument
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Delete);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Delete_Index_Method);

      --  Pop_front
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Pop_Front);
      Set_Data_Type (N, Wildcard_Element_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Pop_Front_Method);

      --  Pop_back
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Pop_Back);
      Set_Data_Type (N, Wildcard_Element_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Pop_Back_Method);

      --  Push_Front
      --  function void push_front (input element_t item);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Push_Front);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Push_Front_Method);

      --  Push_Back
      --  function void push_back (input element_t item);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Push_Back);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Queue_Push_Back_Method);

      --  Associative methods
      --  1800-2017 7.9.1 Num() and size()
      --  function int num();
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Num);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Num_Method);

      --  function int size();
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Size);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Size_Method);

      --  1800-2017 7.9.2 Delete()
      --  function void delete( [input index] );
      --  1) without argument
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Delete);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Delete_Method);

      --  2) with argument
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Delete);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Delete_Index_Method);

      --  1800-2017 7.9.3 Exists()
      --  function int exists (input index);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Exists);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Exists_Method);

      --  1800-2017 7.9.4 First()
      --  function int first (ref index);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_First);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_First_Method);

      --  1800-2017 7.9.5 First()
      --  function int last (ref index);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Last);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Last_Method);

      --  1800-2017 7.9.6 Next()
      --  function int next (ref index);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Next);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Next_Method);

      --  1800-2017 7.9.7 Prev()
      --  function int prev (ref index);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Prev);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Associative_Prev_Method);

      --  Dynamic methods
      --  1800-2017 7.5.2 Size()
      --  function int size()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Size);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Dynamic_Size_Method);

      --  1800-2017 7.5.3 Delete()
      --  function void delete()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Delete);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Dynamic_Delete_Method);

      --  Array manipulation methods
      --  1800-2017 7.12.1 Array locator methods
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Find);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Find_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Find_Index);
      Set_Data_Type (N, Wildcard_Index_Queue_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Find_Index_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Find_First_Index);
      Set_Data_Type (N, Wildcard_Index_Queue_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Find_First_Index_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Min);
      Set_Data_Type (N, Wildcard_Element_Queue_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Min_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Max);
      Set_Data_Type (N, Wildcard_Element_Queue_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Max_Method);

      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Unique);
      Set_Data_Type (N, Wildcard_Element_Queue_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Unique_Method);

      --  Array ordering methods
      --  function void ordering_method (array_type iterator = item);
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Sort);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Array_Sort_Method);

      --  Event triggered
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Triggered);
      Set_Data_Type (N, Unsigned_Bit_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Event_Triggered_Method);

      --  Random variables
      --  18.8 rand_mode()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Rand_Mode);
      Set_Data_Type (N, Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Rand_Mode_Func_Method);

      N := Create_Node (N_Task);
      Set_Identifier (N, Name_Rand_Mode);
      Set_Fully_Analyzed_Flag (N, True);
      pragma Assert (N = Rand_Mode_Task_Method);

      --  The base root class.
      --  Used to that all classes have common methods defined.
      N := Create_Node (N_Class);
      Set_Identifier (N, Get_Identifier ("$baseclass"));
      Set_Virtual_Flag (N, True);
      Set_Class_Visibility (N, Visibility_Public);
      pragma Assert (N = Base_Root_Class);

      --  18.6.1 randomize()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Randomize);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, Signed_Int_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Randomize_Method);
      Set_Class_Item_Chain (Base_Root_Class, N);

      --  18.6.2 pre_randomize()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Pre_Randomize);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Pre_Randomize_Method);
      Set_Chain (Base_Randomize_Method, N);

      --  18.6.4 post_randomize()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Post_Randomize);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Post_Randomize_Method);
      Set_Chain (Base_Pre_Randomize_Method, N);

      --  18.13.3 srandom()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Srandom);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Srandom_Method);
      Set_Chain (Base_Post_Randomize_Method, N);

      --  18.13.4 get_randstate()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Get_Randstate);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, String_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Get_Randstate_Method);
      Set_Chain (Base_Srandom_Method, N);

      --  18.13.5 set_randstate()
      N := Create_Node (N_Function);
      Set_Identifier (N, Name_Set_Randstate);
      Set_Ansi_Port_Flag (N, True);
      Set_Data_Type (N, Void_Typedef);
      Set_Fully_Analyzed_Flag (N, True);
      Set_Parent (N, Base_Root_Class);
      pragma Assert (N = Base_Set_Randstate_Method);
      Set_Chain (Base_Get_Randstate_Method, N);

      --  Arguments for string.substr
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_I);
      Set_Data_Type (Arg, Signed_Int_Typedef);
      Args := Arg;
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_J);
      Set_Data_Type (Arg, Signed_Int_Typedef);
      Set_Chain (Args, Arg);
      Set_Tf_Ports_Chain (String_Substr_Method, Args);

      --  Arguments for string.itoa()
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_I);
      Set_Data_Type (Arg, Signed_Integer_Typedef);
      Set_Tf_Ports_Chain (String_Itoa_Method, Arg);

      --  Argument for associative.exists, associative.delete
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_Index);
      Set_Data_Type (Arg, Wildcard_Index_Typedef);
      Set_Tf_Ports_Chain (Associative_Exists_Method, Arg);
      Set_Tf_Ports_Chain (Associative_Delete_Index_Method, Arg);

      --  Arguments for queue.insert.
      Arg := Create_Node (N_Tf_Input);
      Set_Data_Type (Arg, Signed_Integer_Typedef);
      Set_Identifier (Arg, Name_Index);
      Args := Arg;
      Arg := Create_Node (N_Tf_Input);
      Set_Data_Type (Arg, Wildcard_Element_Typedef);
      Set_Identifier (Arg, Name_Item);
      Set_Chain (Args, Arg);
      Set_Tf_Ports_Chain (Queue_Insert_Method, Args);

      --  Arguments for qeueue.delete with index.
      Arg := Create_Node (N_Tf_Input);
      Set_Data_Type (Arg, Signed_Integer_Typedef);
      Set_Identifier (Arg, Name_Index);
      Set_Tf_Ports_Chain (Queue_Delete_Index_Method, Arg);

      --  Argument for first/last.
      Arg := Create_Node (N_Tf_Ref);
      Set_Identifier (Arg, Name_Index);
      Set_Data_Type (Arg, Wildcard_Index_Typedef);
      Set_Tf_Ports_Chain (Associative_First_Method, Arg);
      Set_Tf_Ports_Chain (Associative_Last_Method, Arg);
      Set_Tf_Ports_Chain (Associative_Next_Method, Arg);
      Set_Tf_Ports_Chain (Associative_Prev_Method, Arg);

      --  Argument for rand_mode task
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Get_Identifier ("on_off"));
      Set_Data_Type (Arg, Unsigned_Bit_Typedef);
      Set_Tf_Ports_Chain (Rand_Mode_Task_Method, Arg);

      --  Argument for srandom()
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_Seed);
      Set_Data_Type (Arg, Int_Typedef);
      Set_Tf_Ports_Chain (Base_Srandom_Method, Arg);

      --  Argument for set_randstate()
      Arg := Create_Node (N_Tf_Input);
      Set_Identifier (Arg, Name_State);
      Set_Data_Type (Arg, String_Typedef);
      Set_Tf_Ports_Chain (Base_Set_Randstate_Method, Arg);

   end Create_Basetypes;

end Verilog.Sem_Types;
