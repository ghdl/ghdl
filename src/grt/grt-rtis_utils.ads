--  GHDL Run Time (GRT) - RTI utilities.
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
with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Stdio; use Grt.Stdio;

package Grt.Rtis_Utils is
   --  Action to perform after a node was handled by the user function:
   --  Traverse_Ok: continue to process.
   --  Traverse_Skip: do not traverse children.
   --  Traverse_Stop: end of walk.
   type Traverse_Result is (Traverse_Ok, Traverse_Skip, Traverse_Stop);

   --  An RTI object is a context and an RTI declaration.
   type Rti_Object is record
      Obj : Ghdl_Rti_Access;
      Ctxt : Rti_Context;
   end record;

   --  Traverse all blocks (package, entities, architectures, block, generate,
   --  processes).
   generic
      with function Process (Ctxt : Rti_Context;
                             Obj : Ghdl_Rti_Access)
                            return Traverse_Result;
   function Traverse_Blocks (Ctxt : Rti_Context) return Traverse_Result;

   generic
      type Param_Type is private;
      with procedure Process (Val_Addr : Address;
                              Val_Name : Vstring;
                              Val_Type : Ghdl_Rti_Access;
                              Param : Param_Type);
   procedure Foreach_Scalar (Ctxt : Rti_Context;
                             Obj_Type : Ghdl_Rti_Access;
                             Obj_Addr : Address;
                             Is_Sig : Boolean;
                             Param : Param_Type);

   --  Convert object address OBJ_LOC (got from RTIs) to ADDR and BOUNDS.
   --  Deals with complex types and fat pointers.
   procedure Object_To_Base_Bounds (Obj_Type : Ghdl_Rti_Access;
                                    Obj_Loc : Address;
                                    Addr : out Address;
                                    Bounds : out Address);

   --  Get address of element EL for record at OBJ.
   procedure Record_To_Element (Obj : Address;
                                El : Ghdl_Rtin_Element_Acc;
                                Is_Sig : Boolean;
                                Rec_Layout : Address;
                                El_Addr : out Address;
                                El_Bounds : out Address);

   --  Return True iff RTI is an unbounded type.
   function Is_Unbounded (Rti : Ghdl_Rti_Access) return Boolean;

   procedure Get_Value (Str : in out Vstring;
                        Value : Value_Union;
                        Type_Rti : Ghdl_Rti_Access);

   --  Get the name of a physical unit.
   function Get_Physical_Unit_Name (Unit : Ghdl_Rti_Access)
                                   return Ghdl_C_String;

   --  Get the value of a physical unit.
   function Get_Physical_Unit_Value (Unit : Ghdl_Rti_Access;
                                     Type_Rti : Ghdl_Rti_Access)
                                    return Ghdl_I64;

   --  Disp a value.
   procedure Disp_Value (Stream : FILEs;
                         Value : Value_Union;
                         Type_Rti : Ghdl_Rti_Access);

   --  Get context as a path name.
   --  If IS_INSTANCE is true, the architecture name of entities is added.
   procedure Get_Path_Name (Rstr : in out Rstring;
                            Last_Ctxt : Rti_Context;
                            Sep : Character;
                            Is_Instance : Boolean := True);

   --  Disp a context as a path.
   procedure Put (Stream : FILEs; Ctxt : Rti_Context);

   --  Extract line and column from a linecol.
   function Get_Linecol_Line (Linecol : Ghdl_Index_Type) return Ghdl_U32;
   function Get_Linecol_Col (Linecol : Ghdl_Index_Type) return Ghdl_U32;

   --  Return the filename in which CTXT is defined.  Used to report locations.
   function Get_Filename (Ctxt : Rti_Context) return Ghdl_C_String;
end Grt.Rtis_Utils;
