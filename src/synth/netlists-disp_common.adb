--  Routine to disp net names.
--  Copyright (C) 2025 Tristan Gingold
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
with Outputs; use Outputs;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils;

package body Netlists.Disp_Common is
   procedure Put_Id (N : Name_Id) is
   begin
      Wr (Name_Table.Image (N));
   end Put_Id;

   procedure Put_Name_Version (N : Sname) is
   begin
      Wr_Uns32 (Get_Sname_Version (N));
   end Put_Name_Version;

   --  Return True IFF N is an extended identifier.
   function Is_Extended_Sname (N : Sname) return Boolean is
   begin
      if N = No_Sname then
         return False;
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User
           | Sname_Version =>
            return Is_Extended_Sname (Get_Sname_Prefix (N));
         when Sname_System =>
            return False;
         when Sname_Field =>
            return True;
         when Sname_Unique =>
            return False;
      end case;
   end Is_Extended_Sname;

   procedure Put_Name_Inner (N : Sname)
   is
      Kind : constant Sname_Kind := Get_Sname_Kind (N);
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Wr ("*nil*");
         return;
      end if;

      if Kind in Sname_Kind_Prefix then
         Prefix := Get_Sname_Prefix (N);
         if Prefix /= No_Sname then
            Put_Name_Inner (Prefix);
            Wr ("_");
         end if;
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User =>
            Put_Id (Get_Sname_Suffix (N));
         when Sname_System =>
            Put_Id (Get_Sname_Suffix (N));
         when Sname_Field =>
            Put_Name_Inner (Get_Sname_Prefix (N));
            Wr ("[");
            Put_Id (Get_Sname_Suffix (N));
            Wr ("]");
         when Sname_Version
           | Sname_Unique =>
            Wr ("n");
            Put_Name_Version (N);
      end case;
   end Put_Name_Inner;

   procedure Put_Extended_End (Lang : Language_Range) is
   begin
      case Lang is
         when Language_Vhdl =>
            Wr ("\");
         when Language_Verilog =>
            Wr (" ");
      end case;
   end Put_Extended_End;

   procedure Put_Name (N : Sname; Lang : Language_Range)
   is
      Is_Extended : constant Boolean := Is_Extended_Sname (N);
   begin
      if Is_Extended then
         Wr ("\");
      end if;

      Put_Name_Inner (N);

      if Is_Extended then
         Put_Extended_End (Lang);
      end if;
   end Put_Name;

   procedure Put_Interface_Name (N : Sname; Lang : Language_Range) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Wr ("*nil*");
         return;
      end if;

      --  Interface names are not versionned.
      if Get_Sname_Kind (N) in Sname_System .. Sname_Field then
         Put_Name (N, Lang);
      else
         Wr ("*err*");
      end if;
   end Put_Interface_Name;

   procedure Disp_Net_Name (N : Net; Lang : Language_Range) is
   begin
      if N = No_Net then
         Wr ("<unassigned>");
         return;
      end if;

      declare
         Inst : constant Instance := Get_Net_Parent (N);
         Idx : constant Port_Idx := Get_Port_Idx (N);
         M : Module;
         Id : Module_Id;
         Inst_Name : Sname;
      begin
         if Is_Self_Instance (Inst) then
            --  For ports of the current module, simply use the port name.
            Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name, Lang);
         else
            Inst_Name := Get_Instance_Name (Inst);
            M := Get_Module (Inst);
            Id := Get_Id (M);
            case Id is
               when Id_Inout
                 | Id_Iinout
                 | Id_User_None .. Module_Id'Last =>
                  --  Gates with multiple outputs.
                  Wr ("\");
                  Put_Name_Inner (Inst_Name);
                  Wr (".");
                  Put_Name_Inner (Get_Output_Desc (M, Idx).Name);
                  Put_Extended_End (Lang);
               when others =>
                  Put_Name (Inst_Name, Lang);
            end case;
         end if;
      end;
   end Disp_Net_Name;

   procedure Put_Instance_Name (Name : Sname; Lang : Language_Range) is
   begin
      if Get_Sname_Kind (Name) = Sname_Version then
         Wr ("inst_");
         Put_Name_Version (Name);
      else
         Put_Name (Name, Lang);
      end if;
   end Put_Instance_Name;

   function Need_Name (Inst : Instance) return Boolean
   is
      use Netlists.Utils;
      Id : constant Module_Id := Get_Id (Inst);
   begin
      case Id is
         when Id_Extract
           | Id_Dyn_Extract
           | Id_Dyn_Insert
           | Id_Bmux
           | Id_Utrunc
           | Id_Strunc =>
            return True;
         when Id_User_None .. Module_Id'Last =>
            return True;
         when others =>
            return False;
      end case;
   end Need_Name;

   function Need_Edge (Inst : Instance) return Boolean
   is
      use Netlists.Utils;
      I : Input;
      Parent : Instance;
   begin
      I := Get_First_Sink (Get_Output (Inst, 0));
      while I /= No_Input loop
         Parent := Get_Input_Parent (I);
         case Get_Id (Parent) is
            when Id_Dff
              | Id_Adff
              | Id_Idff
              | Id_Iadff =>
               if I /= Get_Input (Parent, 0) then
                  return True;
               end if;
            when Id_Mem_Rd_Sync
              | Id_Mem_Wr_Sync =>
               if I /= Get_Input (Parent, 2) then
                  return True;
               end if;
            when others =>
               return True;
         end case;
         I := Get_Next_Sink (I);
      end loop;
      return False;
   end Need_Edge;
end Netlists.Disp_Common;
