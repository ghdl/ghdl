--  Node garbage collector (for debugging).
--  Copyright (C) 2014 Tristan Gingold
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

with Ada.Unchecked_Deallocation;
with Types; use Types;
with Logging; use Logging;
with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;
with Vhdl.Errors; use Vhdl.Errors;
with Libraries;
with Vhdl.Disp_Tree;
with Vhdl.Std_Package;
with PSL.Types; use PSL.Types;

package body Vhdl.Nodes_GC is

   type Marker_Array is array (Iir range <>) of Boolean;
   type Marker_Array_Acc is access Marker_Array;

   Has_Error : Boolean := False;

   Markers : Marker_Array_Acc;

   procedure Free is new Ada.Unchecked_Deallocation
     (Marker_Array, Marker_Array_Acc);

   procedure Report_Early_Reference (N : Iir; F : Nodes_Meta.Fields_Enum) is
   begin
      Log ("early reference to ");
      Log (Nodes_Meta.Get_Field_Image (F));
      Log (" in ");
      Vhdl.Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Early_Reference;

   procedure Report_Already_Marked (N : Iir) is
   begin
      Log ("Already marked ");
      Vhdl.Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Already_Marked;

   procedure Mark_Iir (N : Iir);

   procedure Mark_Iir_List (N : Iir_List)
   is
      It : List_Iterator;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All =>
            null;
         when others =>
            It := List_Iterate (N);
            while Is_Valid (It) loop
               Mark_Iir (Get_Element (It));
               Next (It);
            end loop;
      end case;
   end Mark_Iir_List;

   procedure Mark_Iir_List_Ref (N : Iir_List; F : Fields_Enum)
   is
      El : Iir;
      It : List_Iterator;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All =>
            null;
         when others =>
            It := List_Iterate (N);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if not Markers (El) then
                  Report_Early_Reference (El, F);
               end if;
               Next (It);
            end loop;
      end case;
   end Mark_Iir_List_Ref;

   procedure Mark_Iir_Flist (N : Iir_Flist)
   is
      El : Iir;
   begin
      case N is
         when Null_Iir_Flist
           | Iir_Flist_All
           | Iir_Flist_Others =>
            null;
         when others =>
            for I in Flist_First .. Flist_Last (N) loop
               El := Get_Nth_Element (N, I);
               Mark_Iir (El);
            end loop;
      end case;
   end Mark_Iir_Flist;

   procedure Mark_Iir_Flist_Ref (N : Iir_Flist; F : Fields_Enum)
   is
      El : Iir;
   begin
      case N is
         when Null_Iir_Flist
           | Iir_Flist_All
           | Iir_Flist_Others =>
            null;
         when others =>
            for I in Flist_First .. Flist_Last (N) loop
               El := Get_Nth_Element (N, I);
               if not Markers (El) then
                  Report_Early_Reference (El, F);
               end if;
            end loop;
      end case;
   end Mark_Iir_Flist_Ref;

   procedure Mark_PSL_Node (N : PSL_Node) is
   begin
      null;
   end Mark_PSL_Node;

   procedure Mark_PSL_NFA (N : PSL_NFA) is
   begin
      null;
   end Mark_PSL_NFA;

   procedure Already_Marked (N : Iir) is
   begin
      --  An unused node mustn't be referenced.
      if Get_Kind (N) = Iir_Kind_Unused then
         raise Internal_Error;
      end if;

      if not Flag_Disp_Multiref then
         return;
      end if;

      case Get_Kind (N) is
         when Iir_Kind_Interface_Constant_Declaration =>
            if Get_Identifier (N) = Null_Identifier then
               --  Anonymous interfaces are shared by predefined functions.
               return;
            end if;
         when others =>
            null;
      end case;

      Report_Already_Marked (N);
   end Already_Marked;

   procedure Mark_Chain (Head : Iir)
   is
      El : Iir;
   begin
      El := Head;
      while El /= Null_Iir loop
         Mark_Iir (El);
         El := Get_Chain (El);
      end loop;
   end Mark_Chain;

   procedure Report_Unreferenced_Node (N : Iir) is
   begin
      Vhdl.Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Unreferenced_Node;

   procedure Mark_Iir_Ref_Field (N : Iir; F : Fields_Enum)
   is
      Nf : constant Iir := Get_Iir (N, F);
   begin
      if Is_Valid (Nf) and then not Markers (Nf) then
         Report_Early_Reference (N, F);
      end if;
   end Mark_Iir_Ref_Field;

   procedure Mark_Iir (N : Iir) is
   begin
      if N = Null_Iir then
         return;
      elsif Markers (N) then
         Already_Marked (N);
         return;
      else
         Markers (N) := True;
      end if;

      declare
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Mark_Iir (Get_Iir (N, F));
                     when Attr_Ref =>
                        Mark_Iir_Ref_Field (N, F);
                     when Attr_Forward_Ref
                       | Attr_Chain_Next =>
                        null;
                     when Attr_Maybe_Forward_Ref =>
                        --  Only used for Named_Entity
                        pragma Assert (F = Field_Named_Entity);

                        --  Overload_List has to be handled specially, as it
                        --  that case the Ref applies to the elements of the
                        --  list.
                        declare
                           Nf : constant Iir := Get_Iir (N, F);
                        begin
                           if Nf /= Null_Iir then
                              if Get_Is_Forward_Ref (N) then
                                 pragma Assert
                                   (Get_Kind (Nf) /= Iir_Kind_Overload_List);
                                 null;
                              else
                                 if Get_Kind (Nf) = Iir_Kind_Overload_List then
                                    Mark_Iir (Nf);
                                 else
                                    Mark_Iir_Ref_Field (N, F);
                                 end if;
                              end if;
                           end if;
                        end;
                     when Attr_Maybe_Ref =>
                        if Get_Is_Ref (N) then
                           Mark_Iir_Ref_Field (N, F);
                        else
                           Mark_Iir (Get_Iir (N, F));
                        end if;
                     when Attr_Chain =>
                        Mark_Chain (Get_Iir (N, F));
                     when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_List =>
                  declare
                     Ref : Boolean;
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Ref := False;
                        when Attr_Of_Ref =>
                           Ref := True;
                        when Attr_Of_Maybe_Ref =>
                           Ref := Get_Is_Ref (N);
                        when Attr_Ref =>
                           Ref := True;
                        when others =>
                           raise Internal_Error;
                     end case;
                     if Ref then
                        Mark_Iir_List_Ref (Get_Iir_List (N, F), F);
                     else
                        Mark_Iir_List (Get_Iir_List (N, F));
                     end if;
                  end;
               when Type_Iir_Flist =>
                  declare
                     Ref : Boolean;
                  begin
                     case Get_Field_Attribute (F) is
                        when Attr_None =>
                           Ref := False;
                        when Attr_Of_Ref =>
                           Ref := True;
                        when Attr_Of_Maybe_Ref =>
                           Ref := Get_Is_Ref (N);
                        when Attr_Ref =>
                           Ref := True;
                        when others =>
                           raise Internal_Error;
                     end case;
                     if Ref then
                        Mark_Iir_Flist_Ref (Get_Iir_Flist (N, F), F);
                     else
                        Mark_Iir_Flist (Get_Iir_Flist (N, F));
                     end if;
                  end;
               when Type_PSL_Node =>
                  Mark_PSL_Node (Get_PSL_Node (N, F));
               when Type_PSL_NFA =>
                  Mark_PSL_NFA (Get_PSL_NFA (N, F));
               when others =>
                  null;
            end case;
         end loop;
      end;
   end Mark_Iir;

   procedure Mark_Unit (Unit : Iir)
   is
      List : Iir_List;
      It : List_Iterator;
      El : Iir;
   begin
      pragma Assert (Get_Kind (Unit) = Iir_Kind_Design_Unit);
      if Markers (Unit) then
         return;
      end if;

      --  Mark parents of UNIT.
      declare
         File : constant Iir := Get_Design_File (Unit);
         Lib : constant Iir := Get_Library (File);
      begin
         Markers (File) := True;
         Markers (Lib) := True;
      end;

      --  First mark dependences
      List := Get_Dependence_List (Unit);
      if List /= Null_Iir_List then
         It := List_Iterate (List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            case Get_Kind (El) is
               when Iir_Kind_Design_Unit =>
                  Mark_Unit (El);
               when Iir_Kind_Entity_Aspect_Entity =>
                  declare
                     Ent : constant Iir := Get_Entity_Name (El);
                     Arch_Name : constant Iir := Get_Architecture (El);
                     Arch : Iir;
                  begin
                     Mark_Unit (Get_Design_Unit (Get_Named_Entity (Ent)));

                     --  Architecture is optional.
                     if Is_Valid (Arch_Name) then
                        Arch := Get_Named_Entity (Arch_Name);
                        --  There are many possibilities for the architecture.
                        if Is_Valid (Arch) then
                           case Get_Kind (Arch) is
                              when Iir_Kind_Design_Unit =>
                                 null;
                              when Iir_Kind_Architecture_Body =>
                                 Arch := Get_Design_Unit (Arch);
                              when others =>
                                 Error_Kind ("mark_unit", Arch);
                           end case;
                           Mark_Unit (Arch);
                        end if;
                     end if;
                  end;
               when others =>
                  Error_Kind ("mark_unit", El);
            end case;
            Next (It);
         end loop;
      end if;

      Mark_Iir (Unit);
   end Mark_Unit;

   --  Initialize the mark process.  Create the array and mark some unrooted
   --  but referenced nodes in std_package.
   procedure Mark_Init
   is
      use Vhdl.Std_Package;
   begin
      Markers := new Marker_Array'(Null_Iir .. Nodes.Get_Last_Node => False);

      Has_Error := False;

      --  Node not owned, but used for "/" (time, time).
      Markers (Convertible_Integer_Type_Definition) := True;
      Markers (Convertible_Real_Type_Definition) := True;
   end Mark_Init;

   --  Marks known nodes that aren't owned.
   procedure Mark_Not_Owned
   is
      use Vhdl.Std_Package;
   begin
      --  These nodes are owned by type/subtype declarations, so unmark them
      --  before marking their owner.
      Markers (Convertible_Integer_Type_Definition) := False;
      Markers (Convertible_Real_Type_Definition) := False;

      --  These nodes are not rooted.
      Mark_Iir (Convertible_Integer_Type_Declaration);
      Mark_Iir (Convertible_Integer_Subtype_Declaration);
      Mark_Iir (Convertible_Real_Type_Declaration);
      Mark_Iir (Universal_Integer_One);
      Mark_Chain (Wildcard_Type_Declaration_Chain);
      Mark_Iir (Error_Mark);
   end Mark_Not_Owned;

   procedure Mark_Units_Of_All_Libraries is
   begin
      --  The user nodes.
      declare
         Lib : Iir;
         File : Iir;
         Unit : Iir;
      begin
         --  First mark all known libraries and file.
         Lib := Libraries.Get_Libraries_Chain;
         while Is_Valid (Lib) loop
            pragma Assert (Get_Kind (Lib) = Iir_Kind_Library_Declaration);
            pragma Assert (not Markers (Lib));
            Markers (Lib) := True;
            File := Get_Design_File_Chain (Lib);
            while Is_Valid (File) loop
               pragma Assert (Get_Kind (File) = Iir_Kind_Design_File);
               pragma Assert (not Markers (File));
               Markers (File) := True;
               File := Get_Chain (File);
            end loop;
            Lib := Get_Chain (Lib);
         end loop;

         --  Then mark all design units.  This has to consider first the
         --  dependencies.
         Lib := Libraries.Get_Libraries_Chain;
         while Is_Valid (Lib) loop
            pragma Assert (Get_Kind (Lib) = Iir_Kind_Library_Declaration);
            File := Get_Design_File_Chain (Lib);
            while Is_Valid (File) loop
               pragma Assert (Get_Kind (File) = Iir_Kind_Design_File);
               Unit := Get_First_Design_Unit (File);
               while Is_Valid (Unit) loop
                  Mark_Unit (Unit);
                  Unit := Get_Chain (Unit);
               end loop;
               File := Get_Chain (File);
            end loop;
            Lib := Get_Chain (Lib);
         end loop;
      end;

      --  Obsoleted units.
      declare
         Unit : Iir;
      begin
         Unit := Libraries.Obsoleted_Design_Units;
         while Is_Valid (Unit) loop
            pragma Assert (Get_Kind (Unit) = Iir_Kind_Design_Unit);
            --  FIXME: obsoleted units may be in various state:
            --  - unit created by the .cf file and replaced by the loaded one
            --    (should have been free)
            --  - unit directly obsoleted by a new unit in the same file
            --  - unit indirectly obsoleted.
            if Get_Date_State (Unit) <= Date_Disk then
               --  Never loaded unit, so not referenced and removed from its
               --  design file.
               --  FIXME: free it early.
               pragma Assert (Get_Dependence_List (Unit) = Null_Iir_List);
               Mark_Iir (Unit);
            else
               if not Markers (Unit) then
                  Mark_Iir (Unit);
               end if;
            end if;
            Unit := Get_Chain (Unit);
         end loop;
      end;
   end Mark_Units_Of_All_Libraries;

   procedure Report_Unreferenced
   is
      use Vhdl.Std_Package;
      El : Iir;
      Nbr_Unreferenced : Natural;
   begin
      Mark_Init;
      Mark_Units_Of_All_Libraries;
      Mark_Not_Owned;

      --  Iterate on all nodes, and report nodes not marked.
      El := Error_Mark;
      Nbr_Unreferenced := 0;
      while El in Markers'Range loop
         if not Markers (El) and then Get_Kind (El) /= Iir_Kind_Unused then
            if Nbr_Unreferenced = 0 then
               Log_Line ("** unreferenced nodes:");
            end if;
            Nbr_Unreferenced := Nbr_Unreferenced + 1;
            Report_Unreferenced_Node (El);
         end if;
         El := Next_Node (El);
      end loop;

      Free (Markers);

      if Has_Error then
         raise Internal_Error;
      end if;
   end Report_Unreferenced;

   procedure Check_Tree (Unit : Iir) is
   begin
      Mark_Init;
      Mark_Unit (Unit);
      Free (Markers);
      if Has_Error then
         raise Internal_Error;
      end if;
   end Check_Tree;
end Vhdl.Nodes_GC;
