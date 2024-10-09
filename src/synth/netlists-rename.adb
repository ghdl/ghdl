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

package body Netlists.Rename is
   function Is_Simple_Sname (Name : Sname) return Boolean is
   begin
      --  Only user names can clash.
      return Get_Sname_Kind (Name) = Sname_User
        and then Get_Sname_Prefix (Name) = No_Sname;
   end Is_Simple_Sname;

   function Is_Escaped (Name : Sname; Lang : Language_Type) return Boolean
   is
      pragma Unreferenced (Lang);
      use Name_Table;
      Id : Name_Id;
   begin
      if not Is_Simple_Sname (Name) then
         return False;
      end if;

      Id := Get_Sname_Suffix (Name);
      return Get_Name_Ptr (Id)(1) = '\';
   end Is_Escaped;

   function Is_Verilog_Keyword (Id : Name_Id) return Boolean
   is
      use Std_Names;
   begin
      case Id is
         when Name_First_Verilog .. Name_Last_V2001 =>
            return True;
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
            return True;
         when others =>
            --  Not a keyword
            return False;
      end case;
   end Is_Verilog_Keyword;

   function Escape_Verilog (Id : Name_Id) return Sname
   is
      use Name_Table;
      Len : constant Positive := Get_Name_Length (Id);
      Res : String (1 .. Len + 2);
      New_Id : Name_Id;
   begin
      Res (2 .. Len + 1) := Image (Id);
      Res (1) := '\';
      Res (Len + 2) := ' ';
      New_Id := Get_Identifier (Res);
      return New_Sname_User (New_Id, No_Sname);
   end Escape_Verilog;

   function Rename_Verilog (Name : Sname) return Sname
   is
      Id : Name_Id;
   begin
      Id := Get_Sname_Suffix (Name);

      if Is_Verilog_Keyword (Id) then
         return Escape_Verilog (Id);
      end if;

      --  TODO: for escaped identifiers, check there is no spaces

      return Name;
   end Rename_Verilog;

   function Escape_Vhdl (Id : Name_Id) return Sname
   is
      use Name_Table;
      Len : constant Positive := Get_Name_Length (Id);
      Res : String (1 .. Len + 2);
      New_Id : Name_Id;
   begin
      Res (2 .. Len + 1) := Image (Id);
      Res (1) := '\';
      Res (Len + 2) := '\';
      New_Id := Get_Identifier (Res);
      return New_Sname_User (New_Id, No_Sname);
   end Escape_Vhdl;

   --  Return True if ID can be a vhdl identifier.
   function Is_Vhdl_Identifier (Id : Name_Id) return Boolean
   is
      S : constant String := Name_Table.Image (Id);
      pragma Assert (S'First = 1);
   begin
      for I in 1 .. S'Last loop
         case S (I) is
            when 'a' .. 'z'
              | 'A' .. 'Z' =>
               null;
            when '0' .. '9' =>
               if I = 1 then
                  return False;
               end if;
            when '_' =>
               if I = 1 or else I = S'Last or else S (I - 1) = '_' then
                  return False;
               end if;
            when others =>
               return False;
         end case;
      end loop;
      return True;
   end Is_Vhdl_Identifier;

   function Rename_Vhdl (Name : Sname) return Sname
   is
      Id : Name_Id;
   begin
      Id := Get_Sname_Suffix (Name);

      if Id in Std_Names.Name_Id_Vhdl19_Reserved_Words then
         return Escape_Vhdl (Id);
      end if;

      if not Is_Vhdl_Identifier (Id) then
         return Escape_Vhdl (Id);
      end if;

      --  TODO: escaped identifiers

      return Name;
   end Rename_Vhdl;

   function Rename_Sname (Name : Sname; Lang : Language_Type) return Sname is
   begin
      if not Is_Simple_Sname (Name) then
         return Name;
      end if;

      case Lang is
         when Language_Verilog =>
            return Rename_Verilog (Name);
         when Language_Vhdl =>
            return Rename_Vhdl (Name);
         when others =>
            raise Internal_Error;
      end case;
   end Rename_Sname;

   procedure Rename_User_Module (M : Module; Lang : Language_Type)
   is
      Port : Port_Desc;
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

      --  rename module name ?
      --  rename parameters ?
   end Rename_User_Module;

   procedure Rename_User_Instance
     (Ctxt : Context_Acc; M : Module; Lang : Language_Type)
   is
      Port : Port_Desc;
      Inst : Instance;
      Inst_Mod : Module;
   begin
      --  Rename some instances.
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         Inst_Mod := Get_Module (Inst);
         case Get_Id (Inst_Mod) is
            when Id_Signal
              | Id_Isignal =>
               Set_Instance_Name
                 (Inst, Rename_Sname (Get_Instance_Name (Inst), Lang));
            when Id_User_None .. Module_Id'Last =>
               declare
                  So : Net;
                  Sm : Module;
               begin
                  Set_Parent (Ctxt, M);
                  Sm := Get_Module (Inst);
                  for I in 1 .. Get_Nbr_Outputs (Sm) loop
                     --  If an output is a keyword, it cannot be used to
                     --  declare a net.  Add a Nop to rename it.
                     Port := Get_Output_Desc (Sm, I - 1);
                     if Is_Escaped (Port.Name, Lang) then
                        So := Get_Output (Inst, I - 1);
                        Insert_Nop (Ctxt, So);
                     end if;
                  end loop;
               end;
            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;
   end Rename_User_Instance;

   procedure Rename_Module
     (Ctxt : Context_Acc; M : Module; Lang : Language_Type)
   is
      Sm : Module;
   begin
      --  First the module declarations.
      Sm := Get_First_Sub_Module (M);
      while Sm /= No_Module loop
         if Get_Id (Sm) >= Id_User_None then
            Rename_User_Module (Sm, Lang);
         end if;
         Sm := Get_Next_Sub_Module (Sm);
      end loop;

      --  Then the body.
      Sm := Get_First_Sub_Module (M);
      while Sm /= No_Module loop
         if Get_Id (Sm) >= Id_User_None then
            Rename_User_Instance (Ctxt, Sm, Lang);
         end if;
         Sm := Get_Next_Sub_Module (Sm);
      end loop;
   end Rename_Module;
end Netlists.Rename;
