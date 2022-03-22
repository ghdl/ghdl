--  GHDL Run Time (GRT) - VCD generator.
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

with System;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Rtis;

package Grt.Vcd is
   --  Abstract type for IO.
   type Vcd_Put_Acc is access procedure (Str : String);
   type Vcd_Putc_Acc is access procedure (C : Character);
   type Vcd_Close_Acc is access procedure;

   Vcd_Put : Vcd_Put_Acc;
   Vcd_Putc : Vcd_Putc_Acc;
   Vcd_Close : Vcd_Close_Acc;

   --  VCD type of an object
   type Vcd_Var_Type is
     (
      --  Incompatible vcd type
      Vcd_Bad,

      --  A user-defined enumerated type (other than bit or boolean)
      Vcd_Enum8,

      --  Boolean
      Vcd_Bool,

      --  32bit integer
      Vcd_Integer32,

      --  64bit float
      Vcd_Float64,

      --  A bit type
      Vcd_Bit, Vcd_Stdlogic,

      --  A bit vector type
      Vcd_Bitvector, Vcd_Stdlogic_Vector,

      --  Any array (that is not a vector)
      Vcd_Array,

      --  Any record
      Vcd_Struct
     );

   subtype Vcd_Var_Vectors is Vcd_Var_Type
     range Vcd_Bitvector .. Vcd_Stdlogic_Vector;

   --  Which value to be displayed: effective or driving (for out signals).
   type Vcd_Value_Kind is
     (Vcd_Effective, Vcd_Driving, Vcd_Variable, Vcd_Value_Bad);

   subtype Vcd_Value_Valid is
     Vcd_Value_Kind range Vcd_Effective .. Vcd_Variable;

   --  For signals.
   subtype Vcd_Value_Signals is Vcd_Value_Kind
     range Vcd_Effective .. Vcd_Driving;

   type Verilog_Wire_Info (Vtype : Vcd_Var_Type := Vcd_Bad) is record
      Val : Vcd_Value_Kind;

      --  Access to an array of signals or access to the value.
      Ptr : System.Address;

      case Vtype is
         when Vcd_Var_Vectors =>
            --  Vector bounds.
            Vec_Range : Ghdl_Range_Ptr;
         when Vcd_Enum8 =>
            --  Base type.
            Rti : Rtis.Ghdl_Rti_Access;
         when Vcd_Array =>
            Arr_Rti : Rtis.Ghdl_Rti_Access;
            Arr_Bounds : System.Address;
         when others =>
            null;
      end case;
   end record;

   procedure Get_Verilog_Wire (Sig : VhpiHandleT;
                               Info : out Verilog_Wire_Info);

   --  Number of signals in INFO (at least one).
   function Get_Wire_Length (Info : Verilog_Wire_Info) return Ghdl_Index_Type;

   --  Return TRUE if last change time of the wire described by INFO is LAST.
   --  Used by vcd to know if a signal has changed and should be dumped.
   function Verilog_Wire_Changed (Info : Verilog_Wire_Info;
                                  Last : Std_Time)
                                 return Boolean;

   --  Return TRUE if there is an event on the wire, for the current cycle.
   function Verilog_Wire_Event (Info : Verilog_Wire_Info) return Boolean;

   --  Return a pointer to the value of a wire.
   function Verilog_Wire_Val (Info : Verilog_Wire_Info) return Ghdl_Value_Ptr;
   function Verilog_Wire_Val (Info : Verilog_Wire_Info; Idx : Ghdl_Index_Type)
                             return Ghdl_Value_Ptr;

   procedure Register;
end Grt.Vcd;
