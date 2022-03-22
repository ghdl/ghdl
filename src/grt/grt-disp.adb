--  GHDL Run Time (GRT) - Common display subprograms.
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Stdio; use Grt.Stdio;

package body Grt.Disp is

--    procedure Put_Trim (Stream : FILEs; Str : String)
--    is
--       Start : Natural;
--    begin
--       Start := Str'First;
--       while Start <= Str'Last and then Str (Start) = ' ' loop
--          Start := Start + 1;
--       end loop;
--       Put (Stream, Str (Start .. Str'Last));
--    end Put_Trim;

--   procedure Put_E8 (Stream : FILEs; E8 : Ghdl_E8; Type_Desc : Ghdl_Desc_Ptr)
--    is
--    begin
--       Put_Str_Len (Stream, Type_Desc.E8.Values (Natural (E8)));
--    end Put_E8;

   --procedure Put_E32
   --  (Stream : FILEs; E32 : Ghdl_E32; Type_Desc : Ghdl_Desc_Ptr)
   --is
   --begin
   --   Put_Str_Len (Stream, Type_Desc.E32.Values (Natural (E32)));
   --end Put_E32;

   procedure Put_Sig_Index (Sig : Sig_Table_Index) is
   begin
      Put_I32 (stdout, Ghdl_I32 (Sig));
   end Put_Sig_Index;

   procedure Put_Sig_Range (Sig : Sig_Table_Range) is
   begin
      Put_Sig_Index (Sig.First);
      if Sig.Last /= Sig.First then
         Put ("-");
         Put_Sig_Index (Sig.Last);
      end if;
   end Put_Sig_Range;

   procedure Disp_Now is
   begin
      Put ("Now is ");
      Put_Time (stdout, Current_Time);
      Put (" +");
      Put_I32 (stdout, Ghdl_I32 (Current_Delta));
      New_Line;
   end Disp_Now;

   procedure Disp_Propagation_Kind (Kind : Propagation_Kind_Type) is
   begin
      case Kind is
         when Drv_One_Driver =>
            Put ("Drv (1 drv) ");
         when Eff_One_Driver =>
            Put ("Eff (1 drv) ");
         when Drv_One_Port =>
            Put ("Drv (1 prt) ");
         when Eff_One_Port =>
            Put ("Eff (1 prt) ");
         when Imp_Forward =>
            Put ("Forward ");
         when Imp_Forward_Build =>
            Put ("Forward_Build ");
         when Imp_Guard =>
            Put ("Guard ");
         when Imp_Stable =>
            Put ("Stable ");
         when Imp_Quiet =>
            Put ("Quiet ");
         when Imp_Transaction =>
            Put ("Transaction ");
         when Imp_Delayed =>
            Put ("Delayed ");
         when Eff_Actual =>
            Put ("Eff Actual ");
         when Eff_Multiple =>
            Put ("Eff multiple ");
         when Drv_One_Resolved =>
            Put ("Drv 1 resolved ");
         when Eff_One_Resolved =>
            Put ("Eff 1 resolved ");
         when In_Conversion =>
            Put ("In conv ");
         when Out_Conversion =>
            Put ("Out conv ");
         when Drv_Error =>
            Put ("Drv error ");
         when Drv_Multiple =>
            Put ("Drv multiple ");
         when Prop_End =>
            Put ("end ");
      end case;
   end Disp_Propagation_Kind;

   procedure Disp_Signals_Order is
   begin
      for I in Propagation.First .. Propagation.Last loop
         Put_I32 (stdout, Ghdl_I32 (I));
         Put (": ");
         Disp_Propagation_Kind (Propagation.Table (I).Kind);
         case Propagation.Table (I).Kind is
            when Drv_One_Driver
              | Eff_One_Driver
              | Drv_One_Port
              | Eff_One_Port
              | Drv_One_Resolved
              | Eff_One_Resolved
              | Imp_Guard
              | Imp_Stable
              | Imp_Quiet
              | Imp_Transaction
              | Imp_Delayed
              | Eff_Actual =>
               Put_Sig_Index (Signal_Ptr_To_Index (Propagation.Table (I).Sig));
               New_Line;
            when Imp_Forward =>
               Put_I32 (stdout, Ghdl_I32 (Propagation.Table (I).Sig.Net));
               New_Line;
            when Imp_Forward_Build =>
               declare
                  Forward : Forward_Build_Acc;
               begin
                  Forward := Propagation.Table (I).Forward;
                  Put_Sig_Index (Signal_Ptr_To_Index (Forward.Src));
                  Put (" -> ");
                  Put_Sig_Index (Signal_Ptr_To_Index (Forward.Targ));
                  New_Line;
               end;
            when Eff_Multiple
              | Drv_Multiple =>
               Put_Sig_Range (Propagation.Table (I).Resolv.Sig_Range);
               New_Line;
            when In_Conversion
              | Out_Conversion =>
               declare
                  Conv : Sig_Conversion_Acc;
               begin
                  Conv := Propagation.Table (I).Conv;
                  Put_Sig_Range (Conv.Src);
                  Put (" -> ");
                  Put_Sig_Range (Conv.Dest);
                  New_Line;
               end;
            when Prop_End =>
               New_Line;
            when Drv_Error =>
               null;
         end case;
      end loop;
   end Disp_Signals_Order;

   procedure Disp_Mode (Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_B1 =>
            Put (" b1");
         when Mode_E8 =>
            Put (" e8");
         when Mode_E32 =>
            Put ("e32");
         when Mode_I32 =>
            Put ("i32");
         when Mode_I64 =>
            Put ("i64");
         when Mode_F64 =>
            Put ("f64");
      end case;
   end Disp_Mode;

   procedure Disp_Value (Value : Value_Union; Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_B1 =>
            if Value.B1 then
               Put ("T");
            else
               Put ("F");
            end if;
         when Mode_E8 =>
            Put_I32 (stdout, Ghdl_I32 (Value.E8));
         when Mode_E32 =>
            Put_I32 (stdout, Ghdl_I32 (Value.E32));
         when Mode_I32 =>
            Put_I32 (stdout, Value.I32);
         when Mode_I64 =>
            Put_I64 (stdout, Value.I64);
         when Mode_F64 =>
            Put_F64 (stdout, Value.F64);
      end case;
   end Disp_Value;
end Grt.Disp;
