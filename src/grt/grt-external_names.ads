--  GHDL Run Time (GRT) - External names.
--  Copyright (C) 2026 Tristan Gingold
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

--  Resolution of external names (VHDL-08 8.7) for the AOT backends (gcc and
--  llvm), using the RTIs.  The code generated for an external name calls
--  Ghdl_Extname_Start, then Ghdl_Extname_Up for each '^.', then
--  Ghdl_Extname_Elem for each pathname element and finally
--  Ghdl_Extname_Object for the object simple name.

with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;
with Grt.Processes; use Grt.Processes;

package Grt.External_Names is
   --  Kind of pathname, for Ghdl_Extname_Start.
   Extname_Absolute : constant Ghdl_Index_Type := 0;
   Extname_Relative : constant Ghdl_Index_Type := 1;
   Extname_Package  : constant Ghdl_Index_Type := 2;

   --  Class of the object, for Ghdl_Extname_Object.
   Extname_Constant : constant Ghdl_Index_Type := 0;
   Extname_Signal   : constant Ghdl_Index_Type := 1;
   Extname_Variable : constant Ghdl_Index_Type := 2;

   --  State during the resolution.  Must be kept in sync with
   --  translation.adb.
   type Ghdl_Extname_State is record
      --  Current declarative region (instance data and RTI).
      Base : Address;
      Block : Ghdl_Rti_Access;
      --  Name of the library (for a package pathname).
      Lib : Ghdl_C_String;
      --  Location of the external name, for error messages.
      Filename : Ghdl_C_String;
      Line : Ghdl_I32;
      --  What is expected next (see body).
      Step : Ghdl_I32;
      --  0: an object that is not yet elaborated is an error.
      --  1: a component instance that is not yet elaborated (as it is
      --     bound after the elaboration of the declarations of its parent)
      --     is not an error: the resolution is abandoned and
      --     Ghdl_Extname_Defer_Failed will defer it.
      --  2: likewise for anything not yet elaborated.
      Soft : Ghdl_I32;
   end record;
   pragma Convention (C, Ghdl_Extname_State);
   type Ghdl_Extname_State_Acc is access all Ghdl_Extname_State;

   --  Result.  Must be kept in sync with translation.adb.
   type Ghdl_Extname_Result is record
      --  For a signal: the signal (for a scalar signal) or the address of
      --  the signals (for a composite signal).  Null for other objects.
      Sig : Address;
      --  Address of the value.
      Val : Address;
      --  For an array: the bounds.  For a record: the layout.
      Bounds : Address;
   end record;
   pragma Convention (C, Ghdl_Extname_Result);
   type Ghdl_Extname_Result_Acc is access all Ghdl_Extname_Result;

   --  Start the resolution of an external name.  KIND is one of
   --  Extname_Absolute, Extname_Relative or Extname_Package.  For a relative
   --  pathname, CTXT_RTI and CTXT_BASE is the context in which the external
   --  name appears.  SOFT is copied in ST.
   procedure Ghdl_Extname_Start (St : Ghdl_Extname_State_Acc;
                                 Kind : Ghdl_Index_Type;
                                 Ctxt_Rti : Ghdl_Rti_Access;
                                 Ctxt_Base : Address;
                                 Filename : Ghdl_C_String;
                                 Line : Ghdl_I32;
                                 Soft : Ghdl_Index_Type);

   --  '^.' of a relative pathname.
   procedure Ghdl_Extname_Up (St : Ghdl_Extname_State_Acc);

   --  A pathname element.  If HAS_INDEX is not 0, INDEX is the value of
   --  the expression (the position for an enumerated type).
   procedure Ghdl_Extname_Elem (St : Ghdl_Extname_State_Acc;
                                Name : Ghdl_C_String;
                                Has_Index : Ghdl_Index_Type;
                                Index : Ghdl_I64);

   --  The object simple name.  CLASS is the class of the external name
   --  (Extname_Constant, Extname_Signal or Extname_Variable).  BASE_TYPE is
   --  the RTI of the base type of the external name (or null to not check
   --  the type).  If not null, EXP_BOUNDS are the bounds of the subtype of
   --  the external name (for arrays, the lengths are checked) or its range
   --  (for scalars, the bounds and the direction are checked).
   procedure Ghdl_Extname_Object (St : Ghdl_Extname_State_Acc;
                                  Name : Ghdl_C_String;
                                  Class : Ghdl_Index_Type;
                                  Base_Type : Ghdl_Rti_Access;
                                  Exp_Bounds : Address;
                                  Res : Ghdl_Extname_Result_Acc);

   --  Return 1 if the resolution of a (soft) external name has failed since
   --  the last call to Ghdl_Extname_Defer_Failed, 0 otherwise.
   function Ghdl_Extname_Failed return Ghdl_Index_Type;

   --  If the resolution of a (soft) external name has failed, defer the
   --  call of PROC with THIS until the end of the elaboration (see
   --  Grt.Processes.Ghdl_Elab_Defer).  Clear the failure.
   procedure Ghdl_Extname_Defer_Failed (Proc : Proc_Acc; This : Instance_Acc);

   pragma Export (C, Ghdl_Extname_Failed, "__ghdl_extname_failed");
   pragma Export (C, Ghdl_Extname_Defer_Failed,
                  "__ghdl_extname_defer_failed");
   pragma Export (C, Ghdl_Extname_Start, "__ghdl_extname_start");
   pragma Export (C, Ghdl_Extname_Up, "__ghdl_extname_up");
   pragma Export (C, Ghdl_Extname_Elem, "__ghdl_extname_elem");
   pragma Export (C, Ghdl_Extname_Object, "__ghdl_extname_object");
end Grt.External_Names;
