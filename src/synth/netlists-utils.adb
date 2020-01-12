--  Netlist utilities (composed of a few calls).
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Mutils;
with Types_Utils; use Types_Utils;

with Netlists.Gates; use Netlists.Gates;


package body Netlists.Utils is
   function Get_Nbr_Inputs (Inst : Instance) return Port_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      case Get_Id (M) is
         when Id_Concatn =>
            return Port_Nbr (Get_Param_Uns32 (Inst, 0));
         when others =>
            if Is_Self_Instance (Inst) then
               return Get_Nbr_Outputs (M);
            else
               return Get_Nbr_Inputs (M);
            end if;
      end case;
   end Get_Nbr_Inputs;

   function Get_Nbr_Outputs (Inst : Instance) return Port_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      if Is_Self_Instance (Inst) then
         return Get_Nbr_Inputs (M);
      else
         return Get_Nbr_Outputs (M);
      end if;
   end Get_Nbr_Outputs;

   function Get_Nbr_Params (Inst : Instance) return Param_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      case Get_Id (M) is
         when Id_Const_Bit =>
            return Param_Nbr ((Get_Width (Get_Output (Inst, 0)) + 31) / 32);
         when Id_Const_Log =>
            return Param_Nbr
              (2 * ((Get_Width (Get_Output (Inst, 0)) + 31) / 32));
         when others =>
            return Get_Nbr_Params (M);
      end case;
   end Get_Nbr_Params;

   function Get_Param_Desc
     (Inst : Instance; Param : Param_Idx) return Param_Desc is
   begin
      return Get_Param_Desc (Get_Module (Inst), Param);
   end Get_Param_Desc;

   function Get_Id (Inst : Instance) return Module_Id is
   begin
      return Get_Id (Get_Module (Inst));
   end Get_Id;

   function Get_Input_Name (M : Module; I : Port_Idx) return Sname is
   begin
      return Get_Input_Desc (M, I).Name;
   end Get_Input_Name;

   function Get_Output_Name (M : Module; I : Port_Idx) return Sname is
   begin
      return Get_Output_Desc (M, I).Name;
   end Get_Output_Name;

   function Get_Input_Width (M : Module; I : Port_Idx) return Width is
   begin
      return Get_Input_Desc (M, I).W;
   end Get_Input_Width;

   function Get_Output_Width (M : Module; I : Port_Idx) return Width is
   begin
      return Get_Output_Desc (M, I).W;
   end Get_Output_Width;

   function Get_Input_Net (Inst : Instance; Idx : Port_Idx) return Net is
   begin
      return Get_Driver (Get_Input (Inst, Idx));
   end Get_Input_Net;

   function Get_Input_Instance (Inst : Instance; Idx : Port_Idx)
                               return Instance is
   begin
      return Get_Net_Parent (Get_Input_Net (Inst, Idx));
   end Get_Input_Instance;

   function Is_Const_Module (Id : Module_Id) return Boolean is
   begin
      case Id is
         when Id_Const_UB32
           | Id_Const_SB32
           | Id_Const_UL32
           | Id_Const_X
           | Id_Const_Z
           | Id_Const_0
           | Id_Const_Bit
           | Id_Const_Log =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Const_Module;

   function Is_Const_Net (N : Net) return Boolean is
   begin
      return Is_Const_Module (Get_Id (Get_Net_Parent (N)));
   end Is_Const_Net;

   function Get_Net_Uns64 (N : Net) return Uns64
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Inst) is
         when Id_Const_UB32 =>
            declare
               Va : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               Wd : constant Width := Get_Width (N);
            begin
               --  There must not be any garbage.
               pragma Assert (Shift_Right (Va, Natural (Wd)) = 0);
               return Uns64 (Va);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net_Uns64;

   function Get_Net_Int64 (N : Net) return Int64 is
   begin
      return To_Int64 (Get_Net_Uns64 (N));
   end Get_Net_Int64;

   procedure Get_Net_Element
     (N : Net; Off : Uns32; Va : out Uns32; Zx : out Uns32)
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Inst) is
         when Id_Const_UB32 =>
            declare
               V : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               Wd : constant Width := Get_Width (N);
            begin
               pragma Assert (Off < 32);
               Zx := 0;
               Va := Shift_Right (V, Natural (Wd - Off)) and 1;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Net_Element;

   function Skip_Signal (N : Net) return Net
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Inst) is
         when Id_Signal =>
            return Get_Input_Net (Inst, 0);
         when others =>
            return N;
      end case;
   end Skip_Signal;

   function Is_Connected (O : Net) return Boolean is
   begin
      return Get_First_Sink (O) /= No_Input;
   end Is_Connected;

   function Has_One_Connection (O : Net) return Boolean
   is
      Inp : Input;
   begin
      Inp := Get_First_Sink (O);
      if Inp = No_Input then
         --  No connection.
         return False;
      end if;
      Inp := Get_Next_Sink (Inp);
      return Inp = No_Input;
   end Has_One_Connection;

   procedure Disconnect_And_Free (I : Input)
   is
      I_Net : constant Net := Get_Driver (I);
      Inst : constant Instance := Get_Net_Parent (I_Net);
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      --  First disconnect.
      Disconnect (I);

      --  Quick check: is output (of I) still used ?
      if Is_Connected (I_Net) then
         return;
      end if;

      --  Check that all outputs are unused.
      Nbr_Outputs := Get_Nbr_Outputs (Inst);
      if Nbr_Outputs > 1 then
         for K in 0 .. Nbr_Outputs - 1 loop
            if Is_Connected (Get_Output (Inst, K)) then
               return;
            end if;
         end loop;
      end if;

      --  First disconnect inputs.
      Nbr_Inputs := Get_Nbr_Inputs (Inst);
      if Nbr_Inputs > 0 then
         for K in 0 .. Nbr_Inputs - 1 loop
            Disconnect_And_Free (Get_Input (Inst, K));
         end loop;
      end if;

      --  Free Inst
      Free_Instance (Inst);
   end Disconnect_And_Free;

   function Same_Net (L, R : Net) return Boolean is
   begin
      if L = R then
         --  Obvious case.
         return True;
      end if;

      if Get_Width (L) /= Get_Width (R) then
         --  Must have the same width.
         return False;
      end if;

      declare
         Linst : constant Instance := Get_Net_Parent (L);
         Rinst : constant Instance := Get_Net_Parent (R);
      begin
         if Get_Id (Linst) /= Get_Id (Rinst) then
            return False;
         end if;
         case Get_Id (Linst) is
            when Id_Uextend =>
               --  When index is extended from a subtype.
               return Same_Net (Get_Input_Net (Linst, 0),
                                Get_Input_Net (Rinst, 0));
            when Id_Extract =>
               --  When index is extracted from a record.
               if Get_Param_Uns32 (Linst, 0) /= Get_Param_Uns32 (Rinst, 0) then
                  return False;
               end if;
               return Same_Net (Get_Input_Net (Linst, 0),
                                Get_Input_Net (Rinst, 0));
            when others =>
               return False;
         end case;
      end;
   end Same_Net;

   function Clog2 (W : Width) return Width is
   begin
      return Uns32 (Mutils.Clog2 (Uns64 (W)));
   end Clog2;
end Netlists.Utils;
