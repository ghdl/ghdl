--  GHDL Run Time (GRT) -  Well known RTI types.
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
with Grt.Astdio;
with Grt.Avhpi; use Grt.Avhpi;

package body Grt.Rtis_Types is

   procedure Avhpi_Error (Err : AvhpiErrorT)
   is
      use Grt.Astdio;
      pragma Unreferenced (Err);
   begin
      Put_Line ("grt.rtis_utils.Avhpi_Error!");
   end Avhpi_Error;

   --  Extract std_ulogic type.
   procedure Search_Types (Pack : VhpiHandleT)
   is
      Decl_It : VhpiHandleT;
      Decl : VhpiHandleT;

      Error : AvhpiErrorT;
      Name : String (1 .. 16);
      Name_Len : Natural;
      Rti : Ghdl_Rti_Access;
   begin
      Vhpi_Get_Str (VhpiLibLogicalNameP, Pack, Name, Name_Len);
      if not (Name_Len = 4 and then Name (1 .. 4)= "ieee") then
         return;
      end if;

      Vhpi_Iterator (VhpiDecls, Pack, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      --  Extract packages.
      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         if Vhpi_Get_Kind (Decl) = VhpiEnumTypeDeclK then
            Vhpi_Get_Str (VhpiNameP, Decl, Name, Name_Len);
            Rti := Avhpi_Get_Rti (Decl);
            if Name_Len = 10 and then Name (1 .. 10) = "std_ulogic" then
               Ieee_Std_Logic_1164_Std_Ulogic_RTI_Ptr := Rti;
            end if;
         end if;
      end loop;
   end Search_Types;

   procedure Search_Packages
   is
      Pack : VhpiHandleT;
      Pack_It : VhpiHandleT;

      Error : AvhpiErrorT;
      Name : String (1 .. 16);
      Name_Len : Natural;
   begin
      Get_Package_Inst (Pack_It);

      --  Extract packages.
      loop
         Vhpi_Scan (Pack_It, Pack, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         Vhpi_Get_Str (VhpiNameP, Pack, Name, Name_Len);
         if Name_Len = 14 and then Name (1 .. 14) = "std_logic_1164" then
            Search_Types (Pack);
         end if;
      end loop;
   end Search_Packages;

   Search_Types_RTI_Done : Boolean := False;

   procedure Search_Types_RTI is
   begin
      if Search_Types_RTI_Done then
         return;
      else
         Search_Types_RTI_Done := True;
      end if;

      Search_Packages;
   end Search_Types_RTI;
end Grt.Rtis_Types;
