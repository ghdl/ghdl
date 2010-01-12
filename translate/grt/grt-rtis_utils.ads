--  GHDL Run Time (GRT) - RTI utilities.
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

   procedure Get_Value (Str : in out Vstring;
                        Value : Value_Union;
                        Type_Rti : Ghdl_Rti_Access);

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
end Grt.Rtis_Utils;
