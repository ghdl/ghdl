--  Find top module(s) for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Vhdl.Types; use Vhdl.Types;
with Vhdl.Nodes;
with Vhdl.Sem_Scopes;

with Verilog.Nodes; use Verilog.Nodes;

package body Verilog.Find_Top is
   procedure Find_Top_In_Modules_Chain (Items : Node)
   is
      Stmt : Node;
   begin
      Stmt := Items;

      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when N_Module_Instance =>
               declare
                  use Vhdl.Nodes;
                  use Vhdl.Sem_Scopes;
                  Name : constant Vlg_Node := Get_Module (Stmt);
                  Id : constant Name_Id := Get_Identifier (Name);
                  Interp : Name_Interpretation_Type;
                  Decl : Vhdl_Node;
               begin
                  Interp := Get_Interpretation (Id);
                  if Valid_Interpretation (Interp) then
                     Decl := Get_Declaration (Interp);
                     case Get_Kind (Decl) is
                        when Iir_Kind_Entity_Declaration
                           | Iir_Kind_Foreign_Module =>
                           Set_Elab_Flag (Get_Design_Unit (Decl), True);
                        when others =>
                           raise Internal_Error;
                     end case;
                  else
                     --  If there is no corresponding entity name for the
                     --  component name, assume it belongs to a different
                     --  library (or will be set by a configuration unit).
                     null;
                  end if;
               end;
            when N_Generate_Region
              | N_Generate_Block =>
               Find_Top_In_Modules_Chain (Get_Generate_Item_Chain (Stmt));
            when N_If_Generate =>
               Find_Top_In_Modules_Chain (Get_True_Block (Stmt));
               Find_Top_In_Modules_Chain (Get_False_Block (Stmt));
            when N_Loop_Generate =>
               Find_Top_In_Modules_Chain (Get_Generate_Block (Stmt));
            when N_Case_Generate =>
               --  TODO
               raise Internal_Error;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Find_Top_In_Modules_Chain;

   procedure Mark_Module (N : Int32)
   is
      Module : constant Node := Node (N);
   begin
      pragma Assert (Get_Kind (Module) = N_Module);
      Find_Top_In_Modules_Chain (Get_Items_Chain (Module));
   end Mark_Module;
end Verilog.Find_Top;
