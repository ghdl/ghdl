--  Renaming to avoid use of keywords.
--  Copyright (C) 2022 Tristan Gingold
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

with Name_Table;
with Std_Names;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;

package body Netlists.Rename is
   function Rename_Sname (Name : Sname; Lang : Language_Type) return Sname
   is
      use Name_Table;
      use Std_Names;
      Id : Name_Id;
      Res : String (1 .. 12);
      Len : Positive;
   begin
      if Get_Sname_Kind (Name) /= Sname_User then
         return Name;
      end if;
      if Get_Sname_Prefix (Name) /= No_Sname then
         return Name;
      end if;

      Id := Get_Sname_Suffix (Name);

      pragma Assert (Lang = Language_Verilog);

      case Id is
         when Name_First_Verilog .. Name_Last_V2001 =>
            null;
         when Name_Xnor
           | Name_Nor
           | Name_Nand
           | Name_Xor
           | Name_Or
           | Name_And
           | Name_Begin
           | Name_Case
           | Name_Else
           | Name_End
           | Name_For
           | Name_Function
           | Name_If
           | Name_Inout
           | Name_Not
           | Name_While
           | Name_Wait =>
            null;
         when others =>
            --  Not a keyword
            return Name;
      end case;

      Len := Get_Name_Length (Id);
      Res (2 .. Len + 1) := Image (Id);
      Res (1) := '\';
      Res (Len + 2) := ' ';
      Id := Get_Identifier (Res (1 .. Len + 2));
      return New_Sname_User (Id, No_Sname);
   end Rename_Sname;

   procedure Rename_User_Module (M : Module; Lang : Language_Type)
   is
      Port : Port_Desc;
      Inst : Instance;
   begin
      --  Rename inputs and outputs.
      for I in 1 .. Get_Nbr_Inputs (M) loop
         Port := Get_Input_Desc (M, I - 1);
         Port.Name := Rename_Sname (Port.Name, Lang);
         Set_Input_Desc (M, I - 1, Port);
      end loop;
      for I in 1 .. Get_Nbr_Outputs (M) loop
         Port := Get_Output_Desc (M, I - 1);
         Port.Name := Rename_Sname (Port.Name, Lang);
         Set_Output_Desc (M, I - 1, Port);
      end loop;

      --  Rename some instances.
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         case Get_Id (Inst) is
            when Id_Signal
              | Id_Isignal =>
               Set_Instance_Name
                 (Inst, Rename_Sname (Get_Instance_Name (Inst), Lang));
            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;

      --  rename module name ?
      --  rename parameters ?
   end Rename_User_Module;

   procedure Rename_Module (M : Module; Lang : Language_Type)
   is
      Sm : Module;
   begin
      Sm := Get_First_Sub_Module (M);
      while Sm /= No_Module loop
         if Get_Id (Sm) >= Id_User_None then
            Rename_User_Module (Sm, Lang);
         end if;
         Sm := Get_Next_Sub_Module (Sm);
      end loop;
   end Rename_Module;
end Netlists.Rename;
