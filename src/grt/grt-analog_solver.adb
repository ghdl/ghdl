--  GHDL Run Time (GRT) -  analog solver for sundials.
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

with Grt.Errors; use Grt.Errors;

package body Grt.Analog_Solver is
   function Sundials_Init (Sz : Ghdl_I32) return Ghdl_I32;
   pragma Import (C, Sundials_Init, "grt_sundials_init");

   function Sundials_Start return Ghdl_I32;
   pragma Import (C, Sundials_Start, "grt_sundials_start");

   procedure Sundials_Solve (T0 : Ghdl_F64; Tn : in out Ghdl_F64;
                                            Res : out Integer);
   pragma Import (C, Sundials_Solve, "grt_sundials_solve");

   procedure Init (Size : Ghdl_I32)
   is
      Res : Ghdl_I32;
   begin
      Res := Sundials_Init (Size);
      if Res < 0 then
         Internal_Error ("sundials initialization failure");
      end if;
   end Init;

   function Sundials_Get_Yy_Vec return F64_C_Arr_Ptr;
   pragma Import (C, Sundials_Get_Yy_Vec, "grt_sundials_get_yy_vec");

   function Get_Init_Val_Ptr return F64_C_Arr_Ptr is
   begin
      return Sundials_Get_Yy_Vec;
   end Get_Init_Val_Ptr;

   function Sundials_Get_Yp_Vec return F64_C_Arr_Ptr;
   pragma Import (C, Sundials_Get_Yp_Vec, "grt_sundials_get_yp_vec");

   function Get_Init_Der_Ptr return F64_C_Arr_Ptr is
   begin
      return Sundials_Get_Yp_Vec;
   end Get_Init_Der_Ptr;

   procedure Start is
   begin
      if Sundials_Start /= 0 then
         Internal_Error ("sundials start");
      end if;
   end Start;

   procedure Set_Root_Size (Size : Ghdl_I32) is
   begin
      Internal_Error ("sundials set_root_size");
   end Set_Root_Size;

   procedure Solve (T : Ghdl_F64; Tn : in out Ghdl_F64; Res : out Integer) is
   begin
      Sundials_Solve (T, Tn, Res);
   end Solve;

   procedure Finish is
   begin
      Internal_Error ("sundials finish");
   end Finish;
end Grt.Analog_Solver;
