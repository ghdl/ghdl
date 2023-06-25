--  Verilog elaboration
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

with Name_Table;
with Files_Map;
with Std_Names;
with Errorout;
with Simple_IO; use Simple_IO;

with Verilog.Sem;
with Verilog.Sem_Scopes; use Verilog.Sem_Scopes;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Instances;

package body Verilog.Elaborate is
   procedure Disp_All_Modules (Chain : Node)
   is
      El : Node;
   begin
      Put_Line ("modules are:");
      El := Chain;
      while El /= Null_Node loop
         Put (' ');
         Put_Line (Name_Table.Image (Get_Identifier (El)));
         El := Get_Chain (El);
      end loop;
   end Disp_All_Modules;

   pragma Unreferenced (Disp_All_Modules);

   procedure Resolve_Modules_Chain (Items : Node)
   is
      Stmt : Node;
      Name : Node;
      Module : Node;
   begin
      Stmt := Items;

      while Stmt /= Null_Node loop
         case Get_Kind (Stmt) is
            when N_Module_Instance =>
               Name := Get_Module (Stmt);
               Module := Get_Decl (Get_Identifier (Name));
               if Module = Null_Node then
                  Error_Msg_Sem (+Name, "unknown module %i", +Name);
                  --  TODO: handle cell libraries, foreign modules
               else
                  Set_Declaration (Name, Module);
                  --  Mutate the instance node.
                  case Get_Kind (Module) is
                     when N_Module
                       | N_Foreign_Module =>
                        Set_Instantiated_Flag (Module, True);
                     when N_Interface_Declaration =>
                        Mutate_Instance (Stmt, N_Interface_Instance);
                        Set_Interface_Name (Stmt, Name);
                        Set_Instance_Ref (Stmt, Module);
                     when N_Program_Declaration =>
                        Mutate_Instance (Stmt, N_Program_Instance);
                        Set_Instantiated_Flag (Module, True);
                     when N_Primitive =>
                        Mutate_Instance (Stmt, N_Primitive_Instance);
                     when others =>
                        raise Program_Error;
                  end case;
               end if;
            when N_Generate_Region
              | N_Generate_Block =>
               Resolve_Modules_Chain (Get_Generate_Item_Chain (Stmt));
            when N_If_Generate =>
               Resolve_Modules_Chain (Get_True_Block (Stmt));
               Resolve_Modules_Chain (Get_False_Block (Stmt));
            when N_Loop_Generate =>
               Resolve_Modules_Chain (Get_Generate_Block (Stmt));
            when N_Case_Generate =>
               --  TODO
               raise Internal_Error;
            when others =>
               null;
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Resolve_Modules_Chain;

   procedure Resolve_Modules (Sources : Node)
   is
      El : Node;
   begin
      --  2.  Resolve instances.
      El := Sources;
      while El /= Null_Node loop
         case Get_Kind (El) is
            when N_Module =>
               Resolve_Modules_Chain (Get_Items_Chain (El));
            when others =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;
   end Resolve_Modules;

   --  Resolve all instantiations, mark instantiated modules and programs.
   procedure Resolve_Instantiations (Units : Node)
   is
      Src, El : Node;
   begin
      --  1.  Put modules into name space.

      --  1800-2017 3.13 Name spaces
      --  a) The definitions name space unifies all the non-nested module,
      --     primitive, program, and interface identifiers defined outside all
      --     other declarations.
      Open_Name_Space;
      Src := Units;
      while Src /= Null_Node loop
         El := Get_Descriptions (Src);
         while El /= Null_Node loop
            case Get_Kind (El) is
               when N_Module
                 | N_Foreign_Module
                 | N_Primitive
                 | N_Program_Declaration
                 | N_Interface_Declaration =>
                  Add_Decl (El);
               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
         Src := Get_Chain (Src);
      end loop;

      --  TODO: add also foreign modules.

      --  Mark instantiated modules and programs, and resolve instantiation
      Src := Units;
      while Src /= Null_Node loop
         Resolve_Modules (Get_Descriptions (Src));
         Src := Get_Chain (Src);
      end loop;

      Close_Name_Space;

      if False then
         Put_Line ("top modules are:");
         Src := Units;
         while Src /= Null_Node loop
            El := Get_Descriptions (Src);
            while El /= Null_Node loop
               if not Get_Instantiated_Flag (El) then
                  Put (' ');
                  Put_Line (Name_Table.Image (Get_Identifier (El)));
               end if;
               El := Get_Chain (El);
            end loop;
            Src := Get_Chain (Src);
         end loop;
      end if;
   end Resolve_Instantiations;

   function Create_Root_Module return Node
   is
      Root_Filename : Name_Id;
      Root_Location : Location_Type;
      Root : Node;
   begin
      Root_Filename := Name_Table.Get_Identifier ("*root*");
      Root_Location := Files_Map.File_To_Location
        (Files_Map.Create_Virtual_Source_File (Root_Filename));
      Root := Create_Node (N_Module);
      Set_Location (Root, Root_Location);
      Set_Identifier (Root, Std_Names.Name_D_Root);
      return Root;
   end Create_Root_Module;

   --  Create an instantiation for Decl (a module or a program);
   function Create_Root_Instance (Decl : Node; Loc : Location_Type)
                                 return Node
   is
      Inst : Node;
      Name : Node;
      K : Nkind;
   begin
      --  Instance name.
      Name := Create_Node (N_Name);
      Set_Location (Name, Loc);
      Set_Identifier (Name, Get_Identifier (Decl));
      Set_Declaration (Name, Decl);

      --  The instance.
      case Get_Kind (Decl) is
         when N_Module =>
            K := N_Module_Instance;
         when N_Program_Declaration =>
            K := N_Program_Instance;
         when others =>
            raise Internal_Error;
      end case;
      Inst := Create_Node (K);
      Set_Location (Inst, Loc);
      Set_Identifier (Inst, Get_Identifier (Decl));
      Set_Module (Inst, Name);
      Set_Instance (Inst, Decl);

      return Inst;
   end Create_Root_Instance;

   --  Add module instantiations for uninstantiated modules in SOURCES.
   procedure Populate_Root_Module (Root : Node; Sources : Node)
   is
      Root_Location : constant Location_Type := Get_Location (Root);
      CU : Node;
      El : Node;
      Constr : Items_Constr;
      Inst : Node;
   begin
      --  Add an instance in root module for all uninstantiated modules.
      Init_Constr (Constr, Root);
      CU := Sources;
      while CU /= Null_Node loop
         El := Get_Descriptions (CU);
         while El /= Null_Node loop
            case Get_Kind (El) is
               when N_Module =>
                  if not Get_Instantiated_Flag (El) then
                     Inst := Create_Root_Instance (El, Root_Location);
                     Append_Node (Constr, Inst);
                  end if;
               when N_Program_Declaration =>
                  if not Get_Instantiated_Flag (El) then
                     Inst := Create_Root_Instance (El, Root_Location);
                     Append_Node (Constr, Inst);
                  end if;
               when others =>
                  null;
            end case;
            El := Get_Chain (El);
         end loop;
         CU := Get_Chain (CU);
      end loop;
      Set_Items_Chain (Root, Get_Constr_Chain (Constr));
   end Populate_Root_Module;

   function Elab_Design (Top : Node := Null_Node) return Node
   is
      --  The root module ($root) that simply instantiate top-level modules.
      Root_Module : Node;
   begin
      Resolve_Instantiations (Units_Chain);

      if Errorout.Nbr_Errors /= 0 then
         return Null_Node;
      end if;

      --  Create dummy root module.
      Root_Module := Create_Root_Module;

      if Top = Null_Node then
         --  Extract top-level modules: the uninstantiated ones.
         Populate_Root_Module (Root_Module, Units_Chain);
      else
         --  Single top-level module: TOP
         declare
            Inst : Node;
         begin
            Inst := Create_Root_Instance (Top, Get_Location (Root_Module));
            Set_Items_Chain (Root_Module, Inst);
         end;
      end if;

      Verilog.Sem_Instances.Instantiate_Design (Get_Items_Chain (Root_Module));

      --  Elaborate the design.
      Verilog.Sem.Sem_Design (Root_Module);

      return Root_Module;
   end Elab_Design;
end Verilog.Elaborate;
