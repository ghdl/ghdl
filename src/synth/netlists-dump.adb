--  Routine to dump (for debugging purpose) a netlist.
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Name_Table;
with Files_Map;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Locations;

package body Netlists.Dump is
   procedure Put_Width (W : Width) is
   begin
      Put_Trim (Width'Image (W));
   end Put_Width;

   procedure Put_Id (N : Name_Id) is
   begin
      Put (Name_Table.Image (N));
   end Put_Id;

   procedure Disp_Binary_Digit (Va : Uns32; Zx : Uns32; I : Natural) is
   begin
      Put (Bchar (((Va / 2**I) and 1) + ((Zx / 2**I) and 1) * 2));
   end Disp_Binary_Digit;

   procedure Disp_Binary_Digits (Va : Uns32; Zx : Uns32; W : Natural) is
   begin
      for I in 1 .. W loop
         Disp_Binary_Digit (Va, Zx, W - I);
      end loop;
   end Disp_Binary_Digits;

   procedure Disp_Pval_Binary_Digits (Pv : Pval)
   is
      Len : constant Uns32 := Get_Pval_Length (Pv);
      V   : Logic_32;
      Off : Uns32;
   begin
      if Len = 0 then
         return;
      end if;

      V := Read_Pval (Pv, (Len - 1) / 32);
      for I in reverse 0 .. Len - 1 loop
         Off := I mod 32;
         if Off = 31 then
            V := Read_Pval (Pv, I / 32);
         end if;
         Disp_Binary_Digit (V.Val, V.Zx, Natural (Off));
      end loop;
   end Disp_Pval_Binary_Digits;

   procedure Disp_Pval_Binary (Pv : Pval) is
   begin
      Put ('"');
      Disp_Pval_Binary_Digits (Pv);
      Put ('"');
   end Disp_Pval_Binary;

   procedure Disp_Pval_String (Pv : Pval)
   is
      Len : constant Uns32 := Get_Pval_Length (Pv);
      pragma Assert (Len rem 8 = 0);
      V   : Logic_32;
      Off : Uns32;
      C   : Uns32;
   begin
      Put ('"');
      if Len > 0 then
         V := Read_Pval (Pv, (Len - 1) / 32);
         for I in reverse 0 .. (Len / 8) - 1 loop
            Off := I mod 4;
            if Off = 3 then
               V := Read_Pval (Pv, I / 4);
            end if;
            pragma Assert (V.Zx = 0);
            C := Shift_Right (V.Val, Natural (8 * Off)) and 16#ff#;
            Put (Character'Val (C));
         end loop;
      end if;
      Put ('"');
   end Disp_Pval_String;

   procedure Disp_Instance_Id (Inst : Instance) is
   begin
      if Flag_Disp_Id then
         Put ("{i");
         Put_Trim (Instance'Image (Inst));
         Put ('}');
      end if;
   end Disp_Instance_Id;

   procedure Dump_Name (N : Sname)
   is
      use Name_Table;
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      Prefix := Get_Sname_Prefix (N);
      if Prefix /= No_Sname then
         Dump_Name (Prefix);
         Put (".");
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User =>
            Put ("\");
            Put (Image (Get_Sname_Suffix (N)));
         when Sname_Artificial =>
            Put ("$");
            Put_Id (Get_Sname_Suffix (N));
         when Sname_Version =>
            Put ("%");
            Put_Uns32 (Get_Sname_Version (N));
      end case;
   end Dump_Name;

   procedure Dump_Input_Name (I : Input; With_Id : Boolean := False)
   is
      Inst : constant Instance := Get_Input_Parent (I);
      Idx : constant Port_Idx := Get_Port_Idx (I);
      M : constant Module := Get_Module (Inst);
   begin
      Dump_Name (Get_Instance_Name (Inst));
      Put ('.');
      if Is_Self_Instance (Inst) then
         Dump_Name (Get_Output_Desc (M, Idx).Name);
      else
         if Idx < Get_Nbr_Inputs (M) then
            Dump_Name (Get_Input_Desc (M, Idx).Name);
         else
            Put_Trim (Port_Nbr'Image (Idx));
         end if;
      end if;
      if With_Id then
         Put ("{p");
         Put_Trim (Input'Image (I));
         Put ('}');
      end if;
   end Dump_Input_Name;

   procedure Dump_Net_Name (N : Net; With_Id : Boolean := False)
   is
      Inst : constant Instance := Get_Net_Parent (N);
      Idx : constant Port_Idx := Get_Port_Idx (N);
   begin
      Dump_Name (Get_Instance_Name (Inst));
      Put ('.');
      if Is_Self_Instance (Inst) then
         Dump_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
      else
         Dump_Name (Get_Output_Desc (Get_Module (Inst), Idx).Name);
      end if;
      if With_Id then
         Put ("{n");
         Put_Trim (Net'Image (N));
         Put ('w');
         Put_Width (Get_Width (N));
         Put ('}');
      end if;
   end Dump_Net_Name;

   procedure Dump_Parameter (Inst : Instance; Idx : Param_Idx)
   is
      Desc : constant Param_Desc := Get_Param_Desc (Inst, Idx);
   begin
      if Desc.Name /= No_Sname then
         --  Const_Bit/Log gates have anonymous parameters.
         Dump_Name (Desc.Name);
         Put ('=');
      end if;

      case Desc.Typ is
         when Param_Invalid =>
            Put ("invalid");
         when Param_Uns32 =>
            Put_Uns32 (Get_Param_Uns32 (Inst, Idx));
         when Param_Pval_Vector
            | Param_Pval_String
            | Param_Pval_Integer
            | Param_Pval_Real
            | Param_Pval_Time_Ps
            | Param_Pval_Boolean =>
            Put ("generic");
      end case;
   end Dump_Parameter;

   procedure Dump_Attributes (Inst : Instance; Indent : Natural := 0)
   is
      Attrs : constant Attribute := Get_Instance_First_Attribute (Inst);
      Attr  : Attribute;
      Kind  : Param_Type;
      Val   : Pval;
   begin
      Attr := Attrs;
      while Attr /= No_Attribute loop
         pragma Assert (Has_Instance_Attribute (Inst));

         Put_Indent (Indent);
         Put ("attribute ");
         Put_Id (Get_Attribute_Name (Attr));
         Put (" of ");
         Dump_Name (Get_Instance_Name (Inst));
         Disp_Instance_Id (Inst);
         Put (" := ");
         Kind := Get_Attribute_Type (Attr);
         Val := Get_Attribute_Pval (Attr);
         case Kind is
            when Param_Invalid
              | Param_Uns32 =>
               Put ("??");
            when Param_Pval_String =>
               Disp_Pval_String (Val);
            when Param_Pval_Vector
              | Param_Pval_Integer
              | Param_Pval_Boolean
              | Param_Pval_Real
              | Param_Pval_Time_Ps =>
               Disp_Pval_Binary (Val);
         end case;
         Put_Line (";");
         Attr := Get_Attribute_Next (Attr);
      end loop;
   end Dump_Attributes;

   procedure Dump_Instance (Inst : Instance; Indent : Natural := 0)
   is
      Loc : constant Location_Type := Locations.Get_Location (Inst);
   begin
      if Loc /= No_Location then
         declare
            File : Name_Id;
            Line : Positive;
            Col : Natural;
         begin
            Put_Indent (Indent);
            Put ("# ");
            Files_Map.Location_To_Position (Loc, File, Line, Col);
            Put (Name_Table.Image (File));
            Put (':');
            Put_Uns32 (Uns32 (Line));
            Put (':');
            Put_Uns32 (Uns32 (Col));
            New_Line;
         end;
      end if;

      Dump_Attributes (Inst, Indent);

      Put_Indent (Indent);
      Put ("instance ");
      Dump_Name (Get_Instance_Name (Inst));
      Disp_Instance_Id (Inst);
      Put (": ");
      Dump_Name (Get_Module_Name (Get_Module (Inst)));
      New_Line;

      if Get_Nbr_Params (Inst) > 0 then
         Put_Indent (Indent + 1);
         Put ("parameters");
         for P in Params (Inst) loop
            pragma Warnings (Off, P);
            Put (' ');
            Dump_Parameter (Inst, Get_Param_Idx (P));
         end loop;
         New_Line;
      end if;

      if Get_Nbr_Inputs (Inst) > 0 then
         for I of Inputs (Inst) loop
            Put_Indent (Indent + 1);
            Put ("input ");
            Dump_Input_Name (I, True);
            Put (" <- ");
            declare
               N : constant Net := Get_Driver (I);
            begin
               if N /= No_Net then
                  Dump_Net_Name (N, True);
               end if;
            end;
            New_Line;
         end loop;
      end if;

      if Get_Nbr_Outputs (Inst) > 0 then
         Put_Indent (Indent + 1);
         Put ("outputs");
         for O of Outputs (Inst) loop
            Put (' ');
            Dump_Net_Name (O, True);
         end loop;
         New_Line;
      end if;
   end Dump_Instance;

   procedure Disp_Width (W : Width) is
   begin
      if W /= 1 then
         Put ('[');
         if W = 0 then
            Put ('?');
         else
            Put_Width (W - 1);
            Put (":0");
         end if;
         Put (']');
      end if;
   end Disp_Width;

   procedure Dump_Module_Port (Desc : Port_Desc; Dir : Port_Kind) is
   begin
      case Dir is
         when Port_In =>
            Put ("input");
         when Port_Out =>
            Put ("output");
         when Port_Inout =>
            raise Internal_Error;
      end case;
      Put (' ');
      Dump_Name (Desc.Name);
      Disp_Width (Desc.W);
      Put (';');
      New_Line;
   end Dump_Module_Port;

   procedure Dump_Module_Header (M : Module; Indent : Natural := 0) is
   begin
      --  Module id and name.
      Put_Indent (Indent);
      Put ("module ");
      if Flag_Disp_Id then
         Put ("{m");
         Put_Trim (Module'Image (M));
         Put ("} ");
      end if;
      Dump_Name (Get_Module_Name (M));
      New_Line;

      --  Parameters.
      for P of Params_Desc (M) loop
         Put_Indent (Indent + 1);
         Put ("parameter");
         Put (' ');
         Dump_Name (P.Name);
         Put (": ");
         case P.Typ is
            when Param_Invalid =>
               Put ("invalid");
            when Param_Uns32 =>
               Put ("uns32");
            when Param_Pval_Vector =>
               Put ("pval.vector");
            when Param_Pval_String =>
               Put ("pval.string");
            when Param_Pval_Integer =>
               Put ("pval.integer");
            when Param_Pval_Real =>
               Put ("pval.real");
            when Param_Pval_Time_Ps =>
               Put ("pval.time.ps");
            when Param_Pval_Boolean =>
               Put ("pval.boolean");
         end case;
         New_Line;
      end loop;

      --  Ports.
      for I in 1 .. Get_Nbr_Inputs (M) loop
         Put_Indent (Indent + 1);
         Dump_Module_Port (Get_Input_Desc (M, I - 1), Port_In);
      end loop;
      for I in 1 .. Get_Nbr_Outputs (M) loop
         Put_Indent (Indent + 1);
         Dump_Module_Port (Get_Output_Desc (M, I - 1), Port_Out);
      end loop;
   end Dump_Module_Header;

   procedure Dump_Module (M : Module; Indent : Natural := 0) is
   begin
      Dump_Module_Header (M, Indent);

      for S of Sub_Modules (M) loop
         Dump_Module (S, Indent + 1);
      end loop;

      declare
         Self : constant Instance := Get_Self_Instance (M);
      begin
         if Self /= No_Instance then
            Dump_Instance (Self, Indent + 1);
         end if;
      end;

      for Inst of Instances (M) loop
         Dump_Instance (Inst, Indent + 1);
      end loop;

      for N of Nets (M) loop
         Put_Indent (Indent + 1);
         Put ("connect ");
         Dump_Net_Name (N, True);

         declare
            First : Boolean;
         begin
            First := True;
            for S of Sinks (N) loop
               if First then
                  Put (" -> ");
                  First := False;
               else
                  Put (", ");
               end if;
               Dump_Input_Name (S, True);
            end loop;
         end;
         New_Line;
      end loop;
   end Dump_Module;

   procedure Disp_Net_Name (N : Net) is
   begin
      if N = No_Net then
         Put ("?");
      else
         declare
            Inst : constant Instance := Get_Net_Parent (N);
            Idx : constant Port_Idx := Get_Port_Idx (N);
         begin
            if Is_Self_Instance (Inst) then
               Dump_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
            else
               Dump_Name (Get_Instance_Name (Inst));
               Put (':');
               Dump_Name (Get_Output_Desc (Get_Module (Inst), Idx).Name);
            end if;
         end;
      end if;
   end Disp_Net_Name;

   procedure Put_Net_Width (N : Net) is
   begin
      Put ("{n");
      Put_Trim (Net'Image (N));
      Put ('w');
      Put_Uns32 (Get_Width (N));
      Put ('}');
   end Put_Net_Width;

   procedure Dump_Net_Name_And_Width (N : Net)
   is
      W : Width;
   begin
      if N = No_Net then
         Put ("?");
      else
         Disp_Net_Name (N);

         W := Get_Width (N);
         if Flag_Disp_Id then
            Put_Net_Width (N);
         else
            if W /= 1 then
               Put ('[');
               Put_Uns32 (W);
               Put (']');
            end if;
         end if;

      end if;
   end Dump_Net_Name_And_Width;

   procedure Disp_Instance_Assign (Inst : Instance; Indent : Natural := 0);

   function Can_Inline (Inst : Instance) return Boolean
   is
      O : Net;
      Inp : Input;
   begin
      case Get_Id (Inst) is
         when Id_Signal
           | Id_Output =>
            --  Cut loops.
            return False;
         when others =>
            null;
      end case;
      if Is_Self_Instance (Inst) then
         return False;
      end if;
      if Get_Nbr_Outputs (Inst) /= 1 then
         return False;
      end if;
      if Has_Instance_Attribute (Inst) then
         return False;
      end if;
      O := Get_Output (Inst, 0);
      Inp := Get_First_Sink (O);
      if Inp = No_Input or else Get_Next_Sink (Inp) /= No_Input then
         return False;
      end if;
      if Is_Self_Instance (Get_Input_Parent (Inp)) then
         return False;
      end if;

      return True;
   end Can_Inline;

   procedure Disp_Driver (Drv : Net; Indent : Natural)
   is
      Drv_Inst : Instance;
   begin
      if Drv = No_Net then
         Put ('?');
      else
         Drv_Inst := Get_Net_Parent (Drv);
         if Flag_Disp_Inline and then Can_Inline (Drv_Inst) then
            Disp_Instance_Assign (Drv_Inst, Indent);
         else
            Disp_Net_Name (Drv);
            if Flag_Disp_Id then
               Put_Net_Width (Drv);
            end if;
         end if;
      end if;
   end Disp_Driver;

   --  Debug routine: disp net driver
   procedure Debug_Net (N : Net) is
   begin
      if N = No_Net then
         Put ('?');
      else
         Disp_Instance (Get_Net_Parent (N), False, 0);
      end if;
      New_Line;
   end Debug_Net;

   pragma Unreferenced (Debug_Net);

   Xdigits : constant array (Uns32 range 0 ..15) of Character :=
     "0123456789abcdef";

   procedure Disp_Instance
     (Inst : Instance; With_Name : Boolean; Indent : Natural)
   is
      M : constant Module := Get_Module (Inst);
   begin
      if True then
         --  Pretty-print for some gates
         case Get_Id (M) is
            when Id_Const_UB32 =>
               declare
                  W : constant Width := Get_Width (Get_Output (Inst, 0));
                  V : Uns32;
                  I : Natural;
               begin
                  Put_Width (W);
                  Put ("'uh");
                  V := Get_Param_Uns32 (Inst, 0);
                  if W >= 32 then
                     I := 8;
                  else
                     I := (Natural (W) + 3) / 4;
                  end if;
                  while I > 0 loop
                     I := I - 1;
                     Put (Xdigits (Shift_Right (V, I * 4) and 15));
                  end loop;
               end;
               return;

            when Id_Extract =>
               declare
                  W : constant Width := Get_Width (Get_Output (Inst, 0));
                  Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
               begin
                  Disp_Driver (Get_Input_Net (Inst, 0), Indent);
                  Put ('[');
                  if W > 1 then
                     Put_Uns32 (Off + W - 1);
                     Put (':');
                  end if;
                  Put_Uns32 (Off);
                  Put (']');
                  return;
               end;

            when others =>
               null;
         end case;
      end if;

      Dump_Name (Get_Module_Name (M));

      Disp_Instance_Id (Inst);

      if Has_Instance_Attribute (Inst) then
         declare
            Attr  : Attribute;
            Kind  : Param_Type;
            Val   : Pval;
         begin
            Attr := Get_Instance_First_Attribute (Inst);
            Put ("(* ");
            loop
               Put_Id (Get_Attribute_Name (Attr));
               Put ("=");
               Kind := Get_Attribute_Type (Attr);
               Val := Get_Attribute_Pval (Attr);
               case Kind is
                  when Param_Invalid
                    | Param_Uns32 =>
                     Put ("??");
                  when Param_Pval_String =>
                     Disp_Pval_String (Val);
                  when Param_Pval_Vector
                    | Param_Pval_Integer
                    | Param_Pval_Boolean
                    | Param_Pval_Real
                    | Param_Pval_Time_Ps =>
                     Disp_Pval_Binary (Val);
               end case;
               Attr := Get_Attribute_Next (Attr);
               exit when Attr = No_Attribute;
               Put (", ");
            end loop;
            Put (" *)");
         end;
      end if;

      if Get_Nbr_Params (Inst) > 0 then
         declare
            First : Boolean;
         begin
            First := True;
            Put (" #(");
            for P in Params (Inst) loop
               pragma Warnings (Off, P);
               if not First then
                  Put (", ");
               end if;
               First := False;
               Dump_Parameter (Inst, Get_Param_Idx (P));
            end loop;
            Put (")");
         end;
      end if;

      if With_Name then
         Put (' ');
         Dump_Name (Get_Instance_Name (Inst));
      end if;

      declare
         Nbr_Inputs : constant Port_Nbr := Get_Nbr_Inputs (Inst);
         M : constant Module := Get_Module (Inst);
         Nbr_Fixed_Inputs : constant Port_Nbr := Get_Nbr_Inputs (M);
         Drv : Net;
         I : Input;
         Desc : Port_Desc;
      begin
         if Nbr_Inputs > 0 then
            Put (" (");
            for Idx in 0 .. Nbr_Inputs - 1 loop
               I := Get_Input (Inst, Idx);
               if Idx > 0 then
                  Put (",");
               end if;
               New_Line;
               Put_Indent (Indent);

               --  Input name.
               if Idx < Nbr_Fixed_Inputs then
                  Desc := Get_Input_Desc (M, Idx);
                  if Desc.Name /= No_Sname then
                     Put ('.');
                     Dump_Name (Desc.Name);
                     if Flag_Disp_Id then
                        Put ("{p");
                        Put_Trim (Input'Image (I));
                        Put ('}');
                     end if;
                     Put (": ");
                  end if;
               end if;

               --  Input value.
               Drv := Get_Driver (I);

               if Drv = No_Net then
                  Put ('?');
               else
                  Disp_Driver (Drv, Indent + 1);
               end if;
            end loop;
            Put (')');
         end if;
      end;
   end Disp_Instance;

   procedure Disp_Instance_Assign (Inst : Instance; Indent : Natural := 0) is
   begin
      case Get_Nbr_Outputs (Inst) is
         when 0 =>
            null;
         when 1 =>
            Dump_Net_Name_And_Width (Get_Output (Inst, 0));
            Put (" := ");
         when others =>
            declare
               First : Boolean;
            begin
               First := True;
               Put ('(');
               for O of Outputs (Inst) loop
                  if not First then
                     Put (", ");
                  end if;
                  First := False;
                  Dump_Net_Name_And_Width (O);
               end loop;
               Put (") := ");
            end;
      end case;

      Disp_Instance (Inst, False, Indent + 1);
   end Disp_Instance_Assign;

   procedure Disp_Module (M : Module; Indent : Natural := 0) is
   begin
      --  Name and ports.
      Dump_Module_Header (M, Indent);

      --  Submodules.
      for S of Sub_Modules (M) loop
         if Get_Id (S) >= Id_User_None then
            Disp_Module (S, Indent + 1);
         end if;
      end loop;

      for Inst of Instances (M) loop
         if not (Flag_Disp_Inline and then Can_Inline (Inst)) then
            Put_Indent (Indent + 1);
            Disp_Instance_Assign (Inst, Indent + 1);
            New_Line;
         end if;
      end loop;

      --  Assignments to outputs.
      declare
         Self : constant Instance := Get_Self_Instance (M);
         Drv : Net;
      begin
         if Self /= No_Instance then
            for I of Inputs (Self) loop
               Put_Indent (Indent + 1);
               Dump_Name (Get_Output_Desc (M, Get_Port_Idx (I)).Name);
               Put (" := ");
               Drv := Get_Driver (I);
               if False then
                  Disp_Driver (Drv, 0);
               else
                  Disp_Net_Name (Drv);
                  if Flag_Disp_Id and Drv /= No_Net then
                     Put_Net_Width (Drv);
                  end if;
               end if;
               New_Line;
            end loop;
         end if;
      end;
   end Disp_Module;
end Netlists.Dump;
