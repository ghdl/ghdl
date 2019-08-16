--  Disp a netlist in vhdl using the original entity.
--  Copyright (C) 2019 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Types; use Types;
with Name_Table;

with Vhdl.Prints;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Ieee.Numeric;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;

with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Disp_Vhdl; use Netlists.Disp_Vhdl;

package body Synth.Disp_Vhdl is
   procedure Disp_Signal (Desc : Port_Desc) is
   begin
      Put ("  signal ");
      Put_Name (Desc.Name);
      Put (": ");
      Put_Type (Desc.W);
      Put_Line (";");
   end Disp_Signal;

   procedure Disp_Ports_As_Signals (M : Module) is
   begin
      for I in 1 .. Get_Nbr_Inputs (M) loop
         Disp_Signal (Get_Input_Desc (M, I - 1));
      end loop;
      for I in 1 .. Get_Nbr_Outputs (M) loop
         Disp_Signal (Get_Output_Desc (M, I - 1));
      end loop;
   end Disp_Ports_As_Signals;

   procedure Disp_In_Converter
     (M : Module; Idx : in out Port_Idx; Pfx : String; Ptype : Node)
   is
      Btype : constant Node := Get_Base_Type (Ptype);
      Desc : constant Port_Desc := Get_Input_Desc (M, Idx);
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Btype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
              or else Btype = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
            then
               --  Nothing to do.
               Put_Line ("  wrap_" & Pfx & " <= " & Pfx & ";");
               Idx := Idx + 1;
            else
               --  Any other enum.
               --  TODO: width = 1
               Put ("  wrap_" & Pfx & " <= std_logic_vector(to_unsigned(");
               Put (Name_Table.Image (Get_Identifier
                                        (Get_Type_Declarator (Ptype))));
               Put ("'pos (" & Pfx & ")," & Width'Image (Desc.W) & "));");
               New_Line;
               Idx := Idx + 1;
            end if;
         when Iir_Kind_Integer_Type_Definition =>
            --  FIXME: signed or unsigned ?
            Put ("  wrap_" & Pfx & " <= ");
            if Desc.W > 1 then
               Put ("std_logic_vector(");
            end if;
            Put ("to_unsigned(");
            Put (Pfx & "," & Width'Image (Desc.W) & ")");
            if Desc.W > 1 then
               Put (")");
            elsif Desc.W = 1 then
               Put ("(0)");
            end if;
            Put_Line (";");
            Idx := Idx + 1;
         when Iir_Kind_Array_Type_Definition =>
            if Btype = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
               --  Nothing to do.
               Put ("  wrap_" & Pfx & " <= " & Pfx);
               if Desc.W = 1 then
                  --  This is an array of length 1.  A scalar is used in the
                  --  netlist.
                  Put (" (" & Pfx & "'left)");
               end if;
               Put_Line (";");
               Idx := Idx + 1;
            elsif Btype = Vhdl.Ieee.Numeric.Numeric_Std_Unsigned_Type
               or Btype = Vhdl.Ieee.Numeric.Numeric_Std_Signed_Type then
               Put ("  wrap_" & Pfx & " <= std_logic_vector(" & Pfx);
               if Desc.W = 1 then
                  --  This is an array of length 1.  A scalar is used in the
                  --  netlist.
                  Put (" (" & Pfx & "'left)");
               end if;
               Put_Line (");");
               Idx := Idx + 1;
            else
               Error_Kind ("disp_in_converter(arr)", Ptype);
            end if;
         when others =>
            Error_Kind ("disp_in_converter", Ptype);
      end case;
   end Disp_In_Converter;

   procedure Disp_Out_Converter
     (M : Module; Idx : in out Port_Idx; Pfx : String; Ptype : Node)
   is
      Btype : constant Node := Get_Base_Type (Ptype);
      Desc : constant Port_Desc := Get_Output_Desc (M, Idx);
   begin
      case Get_Kind (Btype) is
         when Iir_Kind_Enumeration_Type_Definition =>
            if Btype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type
              or else Btype = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Type
            then
               --  Nothing to do.
               Put_Line ("  " & Pfx & " <= wrap_" & Pfx & ";");
               Idx := Idx + 1;
            else
               --  Any other enum.
               Put ("  " & Pfx & " <= ");
               Put (Name_Table.Image (Get_Identifier
                                        (Get_Type_Declarator (Ptype))));
               Put ("'val (to_integer(unsigned(wrap_" & Pfx & ")));");
               New_Line;
               Idx := Idx + 1;
            end if;
         when Iir_Kind_Integer_Type_Definition =>
            --  FIXME: signed or unsigned ?
            Put ("  " & Pfx & " <= to_integer (unsigned");
            if Desc.W = 1 then
               Put ("'(0 => wrap_" & Pfx & ')');
            else
               Put (" (wrap_" & Pfx & ')');
            end if;
            Put_Line (");");
            Idx := Idx + 1;
         when Iir_Kind_Array_Type_Definition =>
            if Btype = Vhdl.Ieee.Std_Logic_1164.Std_Logic_Vector_Type then
               --  Nothing to do.
               Put ("  " & Pfx);
               if Desc.W = 1 then
                  --  This is an array of length 1.  A scalar is used in the
                  --  netlist.
                  Put (" (" & Pfx & "'left)");
               end if;
               Put_Line (" <= wrap_" & Pfx & ";");
               Idx := Idx + 1;
            elsif Btype = Vhdl.Std_Package.Bit_Vector_Type_Definition then
               --  Nothing to do.
               Put ("  " & Pfx);
               if Desc.W = 1 then
                  --  This is an array of length 1.  A scalar is used in the
                  --  netlist.
                  Put (" (" & Pfx & "'left) <= to_bit (wrap_" & Pfx & ");");
               else
                  Put (" <= to_bit_vector (wrap_" & Pfx & ");");
               end if;
               New_Line;
               Idx := Idx + 1;
            elsif Is_One_Dimensional_Array_Type (Btype)
              and then (Get_Base_Type (Get_Element_Subtype (Btype))
                          = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type)
            then
               --  unsigned, signed or a compatible array.
               Put ("  " & Pfx);
               if Desc.W = 1 then
                  --  This is an array of length 1.  A scalar is used in the
                  --  netlist.
                  Put (" (" & Pfx & "'left)");
               end if;
               Put (" <= ");
               Put (Name_Table.Image (Get_Identifier
                                        (Get_Type_Declarator (Btype))));
               Put_Line (" (wrap_" & Pfx & ");");
               Idx := Idx + 1;
            else
               Error_Kind ("disp_out_converter(arr)", Ptype);
            end if;
         when others =>
            Error_Kind ("disp_out_converter", Ptype);
      end case;
   end Disp_Out_Converter;

   procedure Disp_Vhdl_Wrapper (Ent : Node; Top : Module)
   is
      Unit : constant Node := Get_Design_Unit (Ent);
      Main : Module;
      Name_Wrap : Name_Id;
   begin
      --  Extract the first user submodule.
      Main := Get_First_Sub_Module (Top);
      while Get_Id (Main) < Id_User_None loop
         Main := Get_Next_Sub_Module (Main);
      end loop;

      --  Disp the original design unit.
      Vhdl.Prints.Disp_Vhdl (Unit);

      --  Disp sub-units (in reverse order).
      declare
         M : Module;
         Num : Natural;
      begin
         Num := 0;
         M := Get_Next_Sub_Module (Main);
         while M /= No_Module loop
            if Get_Id (M) >= Id_User_None then
               Num := Num + 1;
            end if;
            M := Get_Next_Sub_Module (M);
         end loop;

         declare
            type Module_Array is array (1 .. Num) of Module;
            Modules : Module_Array;
         begin
            Num := 0;
            M := Get_Next_Sub_Module (Main);
            while M /= No_Module loop
               if Get_Id (M) >= Id_User_None then
                  Num := Num + 1;
                  Modules (Num) := M;
               end if;
               M := Get_Next_Sub_Module (M);
            end loop;

            for I in reverse Modules'Range loop
               Netlists.Disp_Vhdl.Disp_Vhdl (Modules (I), False);
            end loop;
         end;
      end;
      New_Line;

      --  Rename ports.
      Name_Wrap := Name_Table.Get_Identifier ("wrap");
      for P of Ports_Desc (Main) loop
         pragma Assert (Get_Sname_Prefix (P.Name) = No_Sname);
         Set_Sname_Prefix (P.Name, New_Sname_User (Name_Wrap));
      end loop;

      Put_Line ("library ieee;");
      Put_Line ("use ieee.numeric_std.all;");
      New_Line;
      Put ("architecture rtl of ");
      Put (Name_Table.Image (Get_Identifier (Ent)));
      Put_Line (" is");
      Disp_Ports_As_Signals (Main);
      Disp_Architecture_Declarations (Main);

      Put_Line ("begin");
      --  TODO: add assert for the value of the generics.

      declare
         Idx : Port_Idx;
         Port : Node;
      begin
         Port := Get_Port_Chain (Ent);
         Idx := 0;
         while Port /= Null_Node loop
            if Get_Mode (Port) = Iir_In_Mode then
               Disp_In_Converter
                 (Main, Idx,
                  Name_Table.Image (Get_Identifier (Port)), Get_Type (Port));
            end if;
            Port := Get_Chain (Port);
         end loop;
         pragma Assert (Idx = Get_Nbr_Inputs (Main));

         Port := Get_Port_Chain (Ent);
         Idx := 0;
         while Port /= Null_Node loop
            if Get_Mode (Port) = Iir_Out_Mode then
               Disp_Out_Converter
                 (Main, Idx,
                  Name_Table.Image (Get_Identifier (Port)), Get_Type (Port));
            end if;
            Port := Get_Chain (Port);
         end loop;
         pragma Assert (Idx = Get_Nbr_Outputs (Main));
      end;

      Disp_Architecture_Statements (Main);
      Put_Line ("end rtl;");
   end Disp_Vhdl_Wrapper;
end Synth.Disp_Vhdl;
