--  GHDL Run Time (GRT) -  analog solver.
--  Copyright (C) 2022 Tristan Gingold
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

with Grt.Types; use Grt.Types;

package Grt.Analog_Solver is
   type F64_Array is array (Natural range <>) of Ghdl_F64;
   subtype F64_Fat_Array is F64_Array (Natural);

   --  A pointer to an F64 array.
   type F64_C_Arr_Ptr is access all F64_Fat_Array;
   pragma Convention (C, F64_C_Arr_Ptr);

   --  Initialize the analog solver, SIZE is the number of scalar quantities.
   procedure Init (Size : Ghdl_I32);

   --  Return the address of the initial values vector and derivative vector.
   function Get_Init_Val_Ptr return F64_C_Arr_Ptr;
   function Get_Init_Der_Ptr return F64_C_Arr_Ptr;

   --  Finish initialization.
   procedure Start;

   procedure Residues (T : Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr);
   pragma Import (C, Residues, "grt__analog_solver__residues");

   procedure Set_Root_Size (Size : Ghdl_I32);

   procedure Roots (T : Ghdl_F64;
                    Y : F64_C_Arr_Ptr;
                    Yp: F64_C_Arr_Ptr;
                    Res : F64_C_Arr_Ptr);
   pragma Import (C, Roots, "grt__analog_solver__roots");

   procedure Set_Values (Y : F64_C_Arr_Ptr;
                         Yp: F64_C_Arr_Ptr);
   pragma Import (C, Set_Values, "grt__analog_solver__set_values");

   --  Return status:
   --  0: Ok, Tn reached
   --  1: Stopped due to zero crossing
   --  2: failure
   procedure Solve (T : Ghdl_F64; Tn : in out Ghdl_F64; Res : out Integer);

   procedure Finish;
end Grt.Analog_Solver;
