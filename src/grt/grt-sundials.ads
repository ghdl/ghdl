--  GHDL Run Time (GRT) - Sundials IDA binding.
--  Copyright (C) 2023 Tristan Gingold
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

package Grt.Sundials is
   type F64_Array is array (Natural range <>) of Ghdl_F64;
   subtype F64_Fat_Array is F64_Array (Natural);

   --  A pointer to an F64 array.
   type F64_C_Arr_Ptr is access all F64_Fat_Array;
   pragma Convention (C, F64_C_Arr_Ptr);

   function Initialize return Ghdl_I32;
   pragma Import (C, Initialize, "grt__sundials__initialize");

   --  Initialize the analog solver, SIZE is the number of scalar quantities.
   function Create (Sz : Ghdl_I32; Nbr_Roots : Ghdl_I32) return Ghdl_I32;
   pragma Import (C, Create, "grt__sundials__create");

   --  Set maximum solver step.
   procedure Set_Max_Step (Step : Ghdl_F64);
   pragma Import (C, Set_Max_Step, "grt__sundials__set_max_step");

   --  Return the address of the initial values vector and derivative vector.
   function Get_Yy_Vec return F64_C_Arr_Ptr;
   pragma Import (C, Get_Yy_Vec, "grt__sundials__get_yy_vec");

   function Get_Yp_Vec return F64_C_Arr_Ptr;
   pragma Import (C, Get_Yp_Vec, "grt__sundials__get_yp_vec");

   procedure Residues (T : Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr);
   pragma Import (C, Residues, "grt__analog_solver__residues");

   procedure Roots (T : Ghdl_F64;
                    Y : F64_C_Arr_Ptr;
                    Yp: F64_C_Arr_Ptr;
                    Res : F64_C_Arr_Ptr);
   pragma Import (C, Roots, "grt__analog_solver__roots");

   procedure Set_Values (Y : F64_C_Arr_Ptr;
                         Yp: F64_C_Arr_Ptr);
   pragma Import (C, Set_Values, "grt__analog_solver__set_values");

   procedure Solver_Set_Algebric_Variable (Idx : Natural);
   pragma Import (C, Solver_Set_Algebric_Variable,
                  "grt__analog_solver__set_algebric_variable");
   procedure Solver_Set_Differential_Variable (Idx : Natural);
   pragma Import (C, Solver_Set_Differential_Variable,
                  "grt__analog_solver__set_differential_variable");

   --  Resolve initial conditions.
   function Solver_Initial_Conditions(Tout : Ghdl_F64) return Integer;
   pragma Import (C, Solver_Initial_Conditions,
                  "grt__analog_solver__initial_conditions");

   procedure Sundials_Solve
     (T : Ghdl_F64; Tn : in out Ghdl_F64; Res : out Integer);
   pragma Import (C, Sundials_Solve, "grt__sundials__solve");
end Grt.Sundials;
