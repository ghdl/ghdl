--  Node garbage collector (for debugging).
--  Copyright (C) 2014 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Text_IO;
with Types; use Types;
with Nodes;
with Nodes_Meta; use Nodes_Meta;
with Errorout; use Errorout;
with Iirs; use Iirs;
with Libraries;
with Disp_Tree;
with Std_Package;

package body Nodes_GC is

   type Marker_Array is array (Iir range <>) of Boolean;
   type Marker_Array_Acc is access Marker_Array;

   Has_Error : Boolean := False;

   Markers : Marker_Array_Acc;

   procedure Mark_Iir (N : Iir);

   procedure Mark_Iir_List (N : Iir_List)
   is
      El : Iir;
   begin
      case N is
         when Null_Iir_List
           | Iir_List_All
           | Iir_List_Others =>
            null;
         when others =>
            for I in Natural loop
               El := Get_Nth_Element (N, I);
               exit when El = Null_Iir;
               Mark_Iir (El);
            end loop;
      end case;
   end Mark_Iir_List;

   procedure Mark_PSL_Node (N : PSL_Node) is
   begin
      null;
   end Mark_PSL_Node;

   procedure Mark_PSL_NFA (N : PSL_NFA) is
   begin
      null;
   end Mark_PSL_NFA;

   procedure Report_Already_Marked (N : Iir)
   is
      use Ada.Text_IO;
   begin
      Put ("Already marked ");
      Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Already_Marked;

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

   procedure Report_Early_Reference (N : Iir; F : Nodes_Meta.Fields_Enum)
   is
      use Ada.Text_IO;
   begin
      Put ("early reference to ");
      Put (Nodes_Meta.Get_Field_Image (F));
      Put (" in ");
      Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Early_Reference;

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
      Disp_Tree.Disp_Tree (N, True);
      Has_Error := True;
   end Report_Unreferenced_Node;

   procedure Mark_Iir_Ref_Field (N : Iir; F : Fields_Enum) is
   begin
      case Get_Field_Type (F) is
         when Type_Iir =>
            declare
               Nf : constant Iir := Get_Iir (N, F);
            begin
               if Is_Valid (Nf) and then not Markers (Nf) then
                  Report_Early_Reference (N, F);
               end if;
            end;
         when Type_Iir_List =>
            declare
               Nl : constant Iir_List := Get_Iir_List (N, F);
               El : Iir;
            begin
               if Is_Null_List (Nl) or else Nl in Iir_Lists_All_Others then
                  return;
               end if;
               for I in Natural loop
                  El := Get_Nth_Element (Nl, I);
                  exit when El = Null_Iir;
                  if not Markers (El) then
                     Report_Early_Reference (El, F);
                  end if;
               end loop;
            end;
         when others =>
            raise Internal_Error;
      end case;
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
            case Get_Field_Attribute (F) is
               when Attr_Ref =>
                  Mark_Iir_Ref_Field (N, F);
               when Attr_Forward_Ref
                 | Attr_Chain_Next =>
                  null;
               when Attr_Maybe_Forward_Ref =>
                  --  Only used for Named_Entity
                  pragma Assert (F = Field_Named_Entity);

                  --  Overload_List has to be handled specially, as it that
                  --  case the Ref applies to the elements of the list.
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
               when Attr_None =>
                  case Get_Field_Type (F) is
                     when Type_Iir =>
                        Mark_Iir (Get_Iir (N, F));
                     when Type_Iir_List =>
                        Mark_Iir_List (Get_Iir_List (N, F));
                     when Type_PSL_Node =>
                        Mark_PSL_Node (Get_PSL_Node (N, F));
                     when Type_PSL_NFA =>
                        Mark_PSL_NFA (Get_PSL_NFA (N, F));
                     when others =>
                        null;
                  end case;
               when Attr_Of_Ref =>
                  Mark_Iir_Ref_Field (N, F);
            end case;
         end loop;
      end;
   end Mark_Iir;

   procedure Mark_Unit (Unit : Iir)
   is
      List : Iir_List;
      El : Iir;
   begin
      pragma Assert (Get_Kind (Unit) = Iir_Kind_Design_Unit);
      if Markers (Unit) then
         return;
      end if;

      --  First mark dependences
      List := Get_Dependence_List (Unit);
      if List /= Null_Iir_List then
         for I in Natural loop
            El := Get_Nth_Element (List, I);
            exit when El = Null_Iir;

            case Get_Kind (El) is
               when Iir_Kind_Design_Unit =>
                  Mark_Unit (El);
               when Iir_Kind_Entity_Aspect_Entity =>
                  declare
                     Ent : constant Iir := Get_Entity_Name (El);
                     Arch : constant Iir := Get_Architecture (El);
                  begin
                     Mark_Unit (Get_Design_Unit (Get_Named_Entity (Ent)));
                     if Is_Valid (Arch)
                       and then Is_Valid (Get_Named_Entity (Arch))
                     then
                        Mark_Unit (Get_Named_Entity (Arch));
                     end if;
                  end;
               when others =>
                  Error_Kind ("mark_unit", El);
            end case;
         end loop;
      end if;

      Mark_Iir (Unit);
   end Mark_Unit;

   procedure Report_Unreferenced
   is
      use Ada.Text_IO;
      use Std_Package;
      El : Iir;
      Nbr_Unreferenced : Natural;
   begin
      Markers := new Marker_Array'(Null_Iir .. Iirs.Get_Last_Node => False);

      Has_Error := False;

      --  Node not owned, but used for "/" (time, time).
      Markers (Convertible_Integer_Type_Definition) := True;
      Markers (Convertible_Real_Type_Definition) := True;

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

      El := Error_Mark;
      Nbr_Unreferenced := 0;
      while El in Markers'Range loop
         if not Markers (El) and then Get_Kind (El) /= Iir_Kind_Unused then
            if Nbr_Unreferenced = 0 then
               Put_Line ("** unreferenced nodes:");
            end if;
            Nbr_Unreferenced := Nbr_Unreferenced + 1;
            Report_Unreferenced_Node (El);
         end if;
         El := Iir (Nodes.Next_Node (Nodes.Node_Type (El)));
      end loop;

      if Has_Error then
         raise Internal_Error;
      end if;
   end Report_Unreferenced;
end Nodes_GC;
