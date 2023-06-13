--  Verilog semantic analyzer (scopes)
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

with Grt.Types;
with Grt.Stdio;
with Grt.Astdio;
with Tables;
with Name_Table; --use Name_Table;
with Name_Maps;
with Errorout; use Errorout;
with Verilog.Errors; use Verilog.Errors;

package body Verilog.Sem_Scopes is
   type Cell_Index is new Natural;

   type Name_Cell is record
      Potentially : Boolean;
      Prev : Cell_Index;
      Decl : Node;
   end record;
   pragma Pack (Name_Cell);
   for Name_Cell'Size use 64;

   package Names is new Tables
     (Table_Component_Type => Name_Cell,
      Table_Index_Type => Cell_Index,
      Table_Low_Bound => 2,
      Table_Initial => 128);

   Null_Index : constant Cell_Index := 0;

   --  A declaration belongs to the current scope iff Index > Current_Scope.
   --  (In fact Current_Scope is the index of the saved previous current_scope,
   --   so it could be Index >= Current_Scope).
   Current_Scope : Cell_Index := Null_Index;

   procedure Open_Name_Space is
   begin
      --  Save the previous Current_Scope.
      Names.Append (Name_Cell'(Potentially => False,
                               Prev => Current_Scope,
                               Decl => Null_Node));
      Current_Scope := Names.Last;
   end Open_Name_Space;

   procedure Set_Decl (Id : Name_Id; Cell : Cell_Index) is
   begin
      Name_Table.Set_Name_Info (Id, Int32 (Cell));
   end Set_Decl;

   procedure Close_Name_Space
   is
      Old : Cell_Index;
   begin
      for N in reverse Current_Scope + 1 .. Names.Last loop
         Set_Decl (Get_Identifier (Names.Table (N).Decl),
                   Names.Table (N).Prev);
      end loop;
      Old := Current_Scope;
      Current_Scope := Names.Table (Current_Scope).Prev;
      Names.Set_Last (Old - 1);
   end Close_Name_Space;

   function Peek_Scope_Decl (Id : Name_Id) return Node
   is
      Index : Cell_Index;
   begin
      Index := Cell_Index (Name_Table.Get_Name_Info (Id));
      if Index = Null_Index or else Index < Current_Scope then
         --  No declaration or not in the current scope.
         return Null_Node;
      else
         return Names.Table (Index).Decl;
      end if;
   end Peek_Scope_Decl;

   function Get_Decl (Id : Name_Id) return Node
   is
      Index : Cell_Index;
   begin
      Index := Cell_Index (Name_Table.Get_Name_Info (Id));
      if Index = Null_Index then
         return Null_Node;
      end if;
      declare
         Cell : Name_Cell renames Names.Table (Index);
         Res : constant Node := Cell.Decl;
      begin
         if Cell.Potentially then
            if Index > Current_Scope then
               --  1800-2017 26.3 Referencing data in packages
               --  A potentially locally visible identifier from a wildcard
               --  import may become locally visible if the resolution of a
               --  reference to an identifier finds no other matching locally
               --  visible identifiers.
               Cell.Potentially := False;
            else
               --  1800-2017 26.3 Referencing data in packages
               --  If a match is found among the potentially locally visible
               --  identifiers, that identifier from the package shall be
               --  imported into the outer scope, becoming a locally visible
               --  identifier within the outer scope.
               Names.Append (Name_Cell'(Potentially => False,
                                        Prev => Index,
                                        Decl => Cell.Decl));
               Set_Decl (Id, Names.Last);
               --  CELL may not be valid anymore!
            end if;
         end if;
         return Res;
      end;
   end Get_Decl;

   function Get_Decl_No_Import (Id : Name_Id) return Node
   is
      Index : Cell_Index;
   begin
      Index := Cell_Index (Name_Table.Get_Name_Info (Id));
      if Index = Null_Index then
         return Null_Node;
      end if;
      declare
         Cell : Name_Cell renames Names.Table (Index);
         Res : constant Node := Cell.Decl;
      begin
         if Cell.Potentially then
            return Null_Node;
         end if;
         return Res;
      end;
   end Get_Decl_No_Import;

   procedure Add_Decl (N : Node; Potentially : Boolean := False)
   is
      Id : constant Name_Id := Get_Identifier (N);
      Index : Cell_Index;
      Parent : Node;
      Prev : Node;
      Prev_Parent : Node;
   begin
      Index := Cell_Index (Name_Table.Get_Name_Info (Id));
      if Index > Current_Scope then
         --  Previous declaration in the current scope.
         Parent := Get_Parent (N);
         Prev := Names.Table (Index).Decl;

         if Get_Kind (Parent) in Nkinds_Any_Class
           and then Get_Parent (Prev) /= Parent
         then
            --  Previous declaration was inherited.
            Prev_Parent := Get_Parent (Prev);
            pragma Assert (Get_Kind (Prev_Parent) in Nkinds_Any_Class);

            --  FIXME: Warnings for redeclaration, error for different
            --  virtual method redeclaration.
            --  (only if Parent is being analyzed).

            --  Replace previous declaration with the new one.
            Names.Table (Index).Decl := N;
         elsif Names.Table (Index).Potentially then
            --  Identifier is potentially locally visible.
            if Potentially then
               --  1800-2017 26.3 Referencing data in packages
               --  if shall be illegal if the wildcard import of more than one
               --  package within the same scope defines the same potentially
               --  locally visible identifier and a search for a reference
               --  matches that identifier.

               --  GHDL: importing is OK, use isn't.  Mark it.
               --  TODO: cannot directly change DECL as it is used by
               --  Close_Name_Space.
               --  Names.Table (Index).Decl := Null_Node;
               null;
            else
               --  1800-2017 26.3 Referencing data in packages
               --  A potentially locally visible identifier from a wildcard
               --  import may become locally visible if the resolution of a
               --  reference to an identifier finds no other matching locally
               --  visbile identifier.

               --  GHDL: override the current potentially locally visible
               --  declaration.
               Names.Table (Index) := (Potentially => False,
                                       Prev => Names.Table (Index).Prev,
                                       Decl => N);
            end if;
         else
            --  Identifier is locally visible.
            if Potentially then
               --  1800-2017 26.3 Referencing data in packages
               --  A potentially locally visible identifier from a wildcard
               --  import may become locally visible if the resolution of a
               --  reference to an identifier finds no other matching locally
               --  visbile identifier.

               --  GHDL: there is already a locally visible identifier.
               null;
            else
               Error_Msg_Sem (+N, "redeclaration of %i", +Id);
               Error_Msg_Sem (+Prev, " (previous declaration)");
            end if;
         end if;
         return;
      end if;

      --  1800-2017 26.3 Referencing data in packages
      --  If no locally visible identifiers match, then the potentially locally
      --  visible identifiers defined prior to the point of the reference in
      --  the current scope shall be searched.

      --  GHDL: so a wildcard import in a scope has priority over declarations
      --  in inner scopes.

      Names.Append (Name_Cell'(Potentially => Potentially,
                               Prev => Index,
                               Decl => N));
      Set_Decl (Id, Names.Last);
   end Add_Decl;

   procedure Add_Decl_Chain (Chain : Node; Potentially : Boolean := False)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         Add_Decl (N, Potentially);
         N := Get_Chain (N);
      end loop;
   end Add_Decl_Chain;

   procedure Add_Tf_Decls (Chain : Node)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         case Get_Kind (N) is
            when N_Task
              | N_Function =>
               Add_Decl (N);
            when N_Extern_Task
              | N_Extern_Function =>
               Add_Decl (N);
            when N_Import_DPI_Function =>
               Add_Decl (N);
            when others =>
               null;
         end case;
         N := Get_Chain (N);
      end loop;
   end Add_Tf_Decls;

   procedure Add_Item_Type_Decl (Atype : Node) is
   begin
      case Get_Kind (Atype) is
         when N_Enum_Type =>
            declare
               El : Node;
            begin
               El := Get_Enum_Names (Atype);
               while El /= Null_Node loop
                  pragma Assert (Get_Kind (El) = N_Enum_Name);
                  Add_Decl (El);
                  El := Get_Chain (El);
               end loop;
            end;
         when others =>
            null;
      end case;
   end Add_Item_Type_Decl;

   procedure Add_Class_Decls (Klass : Node)
   is
      Item : Node;
   begin
      --  1800-2017 8.18 Data hiding and encapsulation
      --  Class parameters and class local parameters are also public.
      Add_Decl_Chain (Get_Parameter_Port_Chain (Klass));

      Item := Get_Class_Item_Chain (Klass);
      while Item /= Null_Node loop
         --  Visibility ?
         case Get_Kind (Item) is
            when N_Function
              | N_Extern_Function
              | N_Task
              | N_Extern_Task =>
               Add_Decl (Item);
            when N_Var
              | N_Typedef =>
               Add_Decl (Item);
               if Get_Type_Owner (Item) then
                  Add_Item_Type_Decl (Get_Data_Type (Item));
               end if;
            when others =>
               Error_Kind ("add_class_decls", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Add_Class_Decls;

   procedure Replace_Decl (N : Node)
   is
      Id : constant Name_Id := Get_Identifier (N);
      Index : Cell_Index;
   begin
      Index := Cell_Index (Name_Table.Get_Name_Info (Id));
      if Index < Current_Scope then
         --  Can only replace a declaration in the current scope.
         --  Not in the current scope.
         raise Internal_Error;
      end if;
      Names.Table (Index).Decl := N;
   end Replace_Decl;

   procedure Wildcard_Import_Enum_Type (Etype : Node) is
   begin
      Add_Decl_Chain (Get_Enum_Names (Etype), True);
   end Wildcard_Import_Enum_Type;

   procedure Wildcard_Import (Pkg : Node)
   is
      Item : Node;
   begin
      --  1800-2017 26.3 Referencing data in packages.
      Item := Get_Package_Item_Chain (Pkg);
      while Item /= Null_Node loop
         Add_Decl (Item, True);

         --  Also import enum names, which can be in any declaration.
         case Get_Kind (Item) is
            when N_Typedef =>
               --  TODO: handle other declarations...
               if Get_Type_Owner (Item) then
                  declare
                     Decl_Type : constant Node := Get_Data_Type (Item);
                  begin
                     if Get_Kind (Decl_Type) = N_Enum_Type then
                        Wildcard_Import_Enum_Type (Decl_Type);
                     end if;
                  end;
               end if;
            when others =>
               null;
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Wildcard_Import;

   procedure Debug_Scopes
   is
      use Grt.Astdio;
      use Grt.Types;
      S : constant Grt.Stdio.FILEs := Grt.Stdio.stdout;
   begin
      for I in reverse Names.First .. Names.Last loop
         declare
            Cell : Name_Cell renames Names.Table (I);
         begin
            Put_I32 (S, Ghdl_I32 (I));
            Put (S, ": ");
            if Cell.Decl = Null_Node then
               Put (S, "******** prev scope: ");
            else
               Put (S, Name_Table.Image (Get_Identifier (Cell.Decl)));
               Put (S, ", prev: ");
            end if;
            Put_I32 (S, Ghdl_I32 (Cell.Prev));
            New_Line (S);
         end;
      end loop;
   end Debug_Scopes;

   pragma Unreferenced (Debug_Scopes);

   package Node_Maps is
      new Name_Maps (T => Node, No_Element => Null_Node);

   use Node_Maps;

   Package_Name_Space : Node_Maps.Map_Type;
   Definition_Name_Space : Node_Maps.Map_Type;

   function Get_Definition (Id : Name_Id) return Node is
   begin
      return Get_Element (Definition_Name_Space, Id);
   end Get_Definition;

   procedure Add_Definition (Decl : Node)
   is
      Id : constant Name_Id := Get_Identifier (Decl);
      Prev : Node;
   begin
      Prev := Get_Definition (Id);
      if Prev /= Null_Node then
         Error_Msg_Sem (+Decl, "%i already declared at %l", (+Decl, +Prev));
      else
         Set_Element (Definition_Name_Space, Id, Decl);
      end if;
   end Add_Definition;

   function Get_Package (Id : Name_Id) return Node is
   begin
      return Get_Element (Package_Name_Space, Id);
   end Get_Package;

   procedure Add_Package (Decl : Node)
   is
      Id : constant Name_Id := Get_Identifier (Decl);
      Prev : Node;
   begin
      Prev := Get_Package (Id);
      if Prev /= Null_Node then
         Error_Msg_Sem
           (+Decl, "package %i already declared at %l", (+Decl, +Prev));
      else
         Set_Element (Package_Name_Space, Id, Decl);
      end if;
   end Add_Package;

   procedure Init is
   begin
      Init (Package_Name_Space);
      Init (Definition_Name_Space);
   end Init;
end Verilog.Sem_Scopes;
