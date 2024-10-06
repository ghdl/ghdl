--  Verilog to vhdl conversion
--  Copyright (C) 2023 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
with Types; use Types;
with Name_Table;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Types; use Vhdl.Types;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Scanner;

with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Errors; use Verilog.Errors;

package body Verilog.Vhdl_Export is

   function Build_Foreign_Vector_Type (Loc : Vlg_Node) return Vhdl_Node
   is
      Res : Vhdl_Node;
   begin
      Res := Create_Iir (Iir_Kind_Foreign_Vector_Type_Definition);
      Set_Location (Res, Get_Location (Loc));
      return Res;
   end Build_Foreign_Vector_Type;

   function Convert_Type (N : Vlg_Node) return Vhdl_Node is
   begin
      case Get_Kind (N) is
         when N_Typedef =>
            return Convert_Type (Get_Data_Type (N));
         when N_Predefined_Typedef =>
            return Convert_Type (Get_Expr_Type (N));
         when N_Logic_Type
           | N_Packed_Array =>
            --  One foreign vector per object, as it is a subtype indication.
            return Build_Foreign_Vector_Type (N);
         when others =>
            Error_Kind ("convert_type", N);
      end case;
   end Convert_Type;

   procedure Convert_Decl (N : Vlg_Node;
                           Kind : Iir_Kind;
                           Mode : Iir_Mode;
                           First, Last : in out Vhdl_Node;
                           Parent : Vhdl_Node)
   is
      Id : constant Name_Id := Get_Identifier (N);
      Id_Len : constant Natural := Name_Table.Get_Name_Length (Id);
      New_Id : Name_Id;
      N_Typ : constant Vlg_Node := Get_Data_Type (N);
      Typ : Vhdl_Node;
      Res : Vhdl_Node;
   begin
      --  Convert to lower-case.
      --  TODO
      --  * escaped identifiers -> extended identifier
      --  * identifier with dollar or double underscores -> extended identifier
      --  * normal identifier -> convert to lower case
      --  * check for duplicate identifier due to conversion
      declare
         Name : String (1 .. Id_Len);
         Err : Boolean;
      begin
         Name := Name_Table.Image (Id);
         if Name (1) = '\' then
            --  Verilog escaped identifier
            --  Replace ending space with '\'.
            Name (Id_Len) := '\';
            Err := False;
         else
            Vhdl.Scanner.Convert_Identifier (Name, Err);
         end if;

         if not Err then
            New_Id := Name_Table.Get_Identifier (Name);
         else
            --  TODO: extended identifier ?
            raise Internal_Error;
         end if;
      end;

      --  Convert type
      if N_Typ = Null_Vlg_Node then
         --  Humm, a parameter can have no type.
         Typ := Build_Foreign_Vector_Type (N);
      else
         Typ := Convert_Type (N_Typ);
      end if;

      Res := Create_Iir (Kind);
      Set_Location (Res, Get_Location (N));
      Set_Identifier (Res, New_Id);
      Set_Type (Res, Typ);
      Set_Subtype_Indication (Res, Typ);
      Set_Mode (Res, Mode);
      Set_Parent (Res, Parent);
      Set_Visible_Flag (Res, True);

      Chain_Append (First, Last, Res);
   end Convert_Decl;

   procedure Convert_Decl_Chain (Head : Vlg_Node;
                                 Parent : Vhdl_Node;
                                 First_Gen, Last_Gen : in out Vhdl_Node;
                                 First_Port, Last_Port : in out Vhdl_Node)
   is
      N : Vlg_Node;
   begin
      N := Head;
      while N /= Null_Vlg_Node loop
         case Get_Kind (N) is
            when N_Parameter =>
               Convert_Decl (N, Iir_Kind_Interface_Constant_Declaration,
                             Iir_In_Mode,
                             First_Gen, Last_Gen, Parent);
            when N_Input =>
               Convert_Decl (N, Iir_Kind_Interface_Signal_Declaration,
                             Iir_In_Mode,
                             First_Port, Last_Port, Parent);
            when N_Output =>
               Convert_Decl (N, Iir_Kind_Interface_Signal_Declaration,
                             Iir_Out_Mode,
                             First_Port, Last_Port, Parent);
            when N_Inout =>
               Convert_Decl (N, Iir_Kind_Interface_Signal_Declaration,
                             Iir_Inout_Mode,
                             First_Port, Last_Port, Parent);
            when N_Port =>
               --  Ignore.
               --  TODO: should check the expression is a single name, with
               --   the identifier of the port.
               null;
            when N_Localparam
              | N_Var
              | N_Genvar =>
               null;
            when Nkinds_Nets
              | N_Assign
              | N_Module_Instance
              | N_Generate_Region
              | N_Loop_Generate
              | N_Always
              | N_Initial =>
               null;
            when others =>
               Error_Kind ("convert_decl_chain", N);
         end case;
         N := Get_Chain (N);
      end loop;
   end Convert_Decl_Chain;

   function Convert_Unit_To_Vhdl (N : Vhdl.Types.Vhdl_Node) return Boolean
   is
      Orig : constant Vlg_Node := Vlg_Node (Get_Foreign_Node (N));
      First_Gen, Last_Gen : Vhdl_Node;
      First_Port, Last_Port : Vhdl_Node;
   begin
      pragma Assert (Orig /= Null_Vlg_Node);

      Chain_Init (First_Gen, Last_Gen);
      Chain_Init (First_Port, Last_Port);

      Convert_Decl_Chain (Get_Parameter_Port_Chain (Orig), N,
                          First_Gen, Last_Gen, First_Port, Last_Port);
      Convert_Decl_Chain (Get_Ports_Chain (Orig), N,
                          First_Gen, Last_Gen, First_Port, Last_Port);
      Convert_Decl_Chain (Get_Items_Chain (Orig), N,
                          First_Gen, Last_Gen, First_Port, Last_Port);

      Set_Generic_Chain (N, First_Gen);
      Set_Port_Chain (N, First_Port);
      return True;
   end Convert_Unit_To_Vhdl;
end Verilog.Vhdl_Export;
