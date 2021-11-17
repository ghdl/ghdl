--  Netlist utilities (composed of a few calls).
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

with Mutils;
with Types_Utils; use Types_Utils;

with Netlists.Gates; use Netlists.Gates;


package body Netlists.Utils is
   function Get_Nbr_Inputs (Inst : Instance) return Port_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      case Get_Id (M) is
         when Id_Concatn
           | Id_Pmux =>
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

   function Get_Inout_Flag (M : Module; I : Port_Idx) return Boolean is
   begin
      return Get_Output_Desc (M, I).Dir = Port_Inout;
   end Get_Inout_Flag;

   function Get_Input_Net (Inst : Instance; Idx : Port_Idx) return Net is
   begin
      return Get_Driver (Get_Input (Inst, Idx));
   end Get_Input_Net;

   function Get_Input_Instance (Inst : Instance; Idx : Port_Idx)
                               return Instance is
   begin
      return Get_Net_Parent (Get_Input_Net (Inst, Idx));
   end Get_Input_Instance;

   function Get_Param_Name (M : Module; I : Param_Idx) return Sname is
   begin
      return Get_Param_Desc (M, I).Name;
   end Get_Param_Name;

   function Get_Param_Type (M : Module; I : Param_Idx) return Param_Type is
   begin
      return Get_Param_Desc (M, I).Typ;
   end Get_Param_Type;

   function Is_Const_Net (N : Net) return Boolean is
   begin
      if Get_Width (N) = 0 then
         return True;
      end if;
      return Get_Id (Get_Net_Parent (N)) in Constant_Module_Id;
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
         when Id_Const_SB32 =>
            declare
               Va : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               Wd : constant Natural := Natural (Get_Width (N));
               Res : Uns64;
            begin
               Res := Uns64 (Va);
               Res := Shift_Left (Res, 64 - Wd);
               Res := Shift_Right_Arithmetic (Res, 64 - Wd);
               return Res;
            end;
         when others =>
            if Get_Width (N) = 0 then
               return 0;
            end if;
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

   function Disconnect_And_Get (I : Input) return Net
   is
      N : Net;
   begin
      N := Get_Driver (I);
      Disconnect (I);
      return N;
   end Disconnect_And_Get;

   function Disconnect_And_Get (Inst : Instance; I : Port_Idx) return Net is
   begin
      return Disconnect_And_Get (Get_Input (Inst, I));
   end Disconnect_And_Get;

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

   function Same_Clock (L, R : Net) return Boolean
   is
      Linst : constant Instance := Get_Net_Parent (L);
      Rinst : constant Instance := Get_Net_Parent (R);
   begin
      if Get_Id (Linst) /= Get_Id (Rinst) then
         return False;
      end if;
      pragma Assert (Get_Id (Linst) in Edge_Module_Id);
      return Same_Net (Get_Input_Net (Linst, 0),
                       Get_Input_Net (Rinst, 0));
   end Same_Clock;

   procedure Copy_Instance_Attributes (Dest : Instance; Src : Instance)
   is
      Attr : Attribute;
   begin
      Attr := Get_Instance_First_Attribute (Src);
      while Attr /= No_Attribute loop
         Set_Instance_Attribute (Dest,
                                 Get_Attribute_Name (Attr),
                                 Get_Attribute_Type (Attr),
                                 Get_Attribute_Pval (Attr));
         Attr := Get_Attribute_Next (Attr);
      end loop;
   end Copy_Instance_Attributes;

   function Clog2 (W : Width) return Width is
   begin
      return Uns32 (Mutils.Clog2 (Uns64 (W)));
   end Clog2;
end Netlists.Utils;
