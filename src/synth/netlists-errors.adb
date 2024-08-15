--  Error handling for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

package body Netlists.Errors is
   function "+" (N : Instance) return Earg_Type is
   begin
      return Make_Earg_Synth_Instance (Uns32 (N));
   end "+";

   function "+" (N : Net) return Earg_Type is
   begin
      return Make_Earg_Synth_Net (Uns32 (N));
   end "+";

   function "+" (N : Sname) return Earg_Type is
   begin
      return Make_Earg_Synth_Name (Uns32 (N));
   end "+";

   procedure Output_Name_1 (N : Sname)
   is
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Output_Message ("*nil*");
         return;
      end if;

      Prefix := Get_Sname_Prefix (N);
      if Prefix /= No_Sname then
         Output_Name_1 (Prefix);
         Output_Message (".");
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User =>
            Output_Identifier (Get_Sname_Suffix (N));
         when Sname_Artificial =>
            Output_Identifier (Get_Sname_Suffix (N));
         when Sname_Version =>
            Output_Message ("n");
            Output_Uns32 (Get_Sname_Version (N));
      end case;
   end Output_Name_1;

   procedure Output_Name (N : Sname) is
   begin
      Output_Message ("""");
      Output_Name_1 (N);
      Output_Message ("""");
   end Output_Name;

   procedure Synth_Instance_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      pragma Unreferenced (Err);
      Inst : constant Instance := Instance (Val);
   begin
      case Format is
         when 'n' =>
            Output_Name (Get_Instance_Name (Inst));
         when 'i' =>
            Output_Message ("i");
            Output_Uns32 (Uns32 (Inst));
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Instance_Handler;

   procedure Synth_Net_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      pragma Unreferenced (Err);
      N : constant Net := Net (Val);
   begin
      case Format is
         when 'n' =>
            declare
               Inst : constant Instance := Get_Net_Parent (N);
               Idx : constant Port_Idx := Get_Port_Idx (N);
               Name : Sname;
            begin
               if Is_Self_Instance (Inst) then
                  Name := Get_Input_Desc (Get_Module (Inst), Idx).Name;
               else
                  Name := Get_Output_Desc (Get_Module (Inst), Idx).Name;
               end if;
               Output_Name (Name);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Synth_Net_Handler;

   procedure Synth_Name_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      pragma Unreferenced (Err);
      N : constant Sname := Sname (Val);
   begin
      if Format = 'n' then
         Output_Name (N);
      else
         raise Internal_Error;
      end if;
   end Synth_Name_Handler;

   procedure Initialize is
   begin
      Register_Earg_Handler
        (Earg_Synth_Instance, Synth_Instance_Handler'Access);
      Register_Earg_Handler
        (Earg_Synth_Net, Synth_Net_Handler'Access);
      Register_Earg_Handler
        (Earg_Synth_Name, Synth_Name_Handler'Access);
   end Initialize;
end Netlists.Errors;
