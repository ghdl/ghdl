--  Cross references.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Tables;
with GNAT.Heap_Sort_A;
with Flags;
with Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Nodes_Priv;

package body Vhdl.Xrefs is
   type Xref_Type is record
      --  Where the cross-reference (or the name) appears.
      Loc : Location_Type;

      --  What the name refer to.
      Ref : Iir;

      --  Kind of reference (See package specification).
      Kind : Xref_Kind;
   end record;

   package Xref_Table is new Tables
     (Table_Index_Type => Natural,
      Table_Component_Type => Xref_Type,
      Table_Low_Bound => 0,
      Table_Initial => 128);

   function Get_Xref_Location (N : Xref) return Location_Type is
   begin
      return Xref_Table.Table (N).Loc;
   end Get_Xref_Location;

   function Get_Xref_Kind (N : Xref) return Xref_Kind is
   begin
      return Xref_Table.Table (N).Kind;
   end Get_Xref_Kind;

   function Get_Xref_Node (N : Xref) return Iir is
   begin
      return Xref_Table.Table (N).Ref;
   end Get_Xref_Node;

   function Get_Last_Xref return Xref is
   begin
      return Xref_Table.Last;
   end Get_Last_Xref;

   procedure Init is
   begin
      Xref_Table.Set_Last (Bad_Xref);
   end Init;

   procedure Add_Xref (Loc : Location_Type; Ref : Iir; Kind : Xref_Kind) is
   begin
      --  Check there is no xref for the same location to the same reference.
      --  (Note that a designatore may reference several declarations, this
      --   is possible in attribute specification for an overloadable name).
      --  This is a simple heuristic as this catch only two referenced in the
      --  row but efficient and should be enough to catch errors.
      pragma Assert
        (Xref_Table.Last < Xref_Table.First
           or else Xref_Table.Table (Xref_Table.Last).Loc /= Loc
           or else Xref_Table.Table (Xref_Table.Last).Ref /= Ref);

      Xref_Table.Append (Xref_Type'(Loc => Loc,
                                    Ref => Ref,
                                    Kind => Kind));
   end Add_Xref;

   procedure Xref_Decl (Decl : Iir) is
   begin
      if Flags.Flag_Xref then
         Add_Xref (Get_Location (Decl), Decl, Xref_Decl);
      end if;
   end Xref_Decl;

   procedure Xref_Ref (Name : Iir; Decl : Iir) is
   begin
      if Flags.Flag_Xref then
         Add_Xref (Get_Location (Name), Decl, Xref_Ref);
      end if;
   end Xref_Ref;

   procedure Xref_Body (Bod : Iir; Spec : Iir) is
   begin
      if Flags.Flag_Xref then
         Add_Xref (Get_Location (Bod), Spec, Xref_Body);
      end if;
   end Xref_Body;

   procedure Xref_End (Loc : Location_Type; Decl : Iir) is
   begin
      if Flags.Flag_Xref then
         Add_Xref (Loc, Decl, Xref_End);
      end if;
   end Xref_End;

   procedure Xref_Keyword (Loc : Location_Type) is
   begin
      if Flags.Flag_Xref then
         Add_Xref (Loc, Null_Iir, Xref_Keyword);
      end if;
   end Xref_Keyword;

   procedure Xref_Name_1 (Name : Iir) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Character_Literal =>
            declare
               Res : constant Iir := Get_Named_Entity (Name);
            begin
               if Res = Vhdl.Std_Package.Error_Mark then
                  return;
               end if;
               Add_Xref (Get_Location (Name), Res, Xref_Ref);
            end;
         when Iir_Kind_Selected_Element =>
            Add_Xref (Get_Location (Name),
                      Get_Named_Entity (Name), Xref_Ref);
         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Function_Call =>
            null;
         when Iir_Kinds_Attribute =>
            null;
         when Iir_Kind_Attribute_Name =>
            --  FIXME: user defined attributes.
            null;
         when Iir_Kind_Type_Conversion =>
            return;
         when others =>
            Error_Kind ("xref_name_1", Name);
      end case;
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Character_Literal =>
            null;
         when Iir_Kind_Selected_Name
           | Iir_Kind_Selected_Element
           | Iir_Kind_Attribute_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kinds_Attribute
           | Iir_Kind_Function_Call =>
            Xref_Name_1 (Get_Prefix (Name));
         when others =>
            Error_Kind ("xref_name_1", Name);
      end case;
   end Xref_Name_1;

   procedure Xref_Name (Name : Iir) is
   begin
      if Flags.Flag_Xref and Name /= Null_Iir then
         Xref_Name_1 (Name);
      end if;
   end Xref_Name;

   procedure Move (From : Natural; To : Natural)
   is
      Tmp : Xref_Type;
   begin
      Tmp := Xref_Table.Table (To);
      Xref_Table.Table (To) := Xref_Table.Table (From);
      Xref_Table.Table (From) := Tmp;
   end Move;

   function Loc_Lt (Op1, Op2 : Natural) return Boolean
   is
      L1 : constant Location_Type := Xref_Table.Table (Op1).Loc;
      L2 : constant Location_Type := Xref_Table.Table (Op2).Loc;
   begin
      return L1 < L2;
   end Loc_Lt;

   procedure Sort_By_Location is
   begin
      GNAT.Heap_Sort_A.Sort (Xref_Table.Last, Move'Access, Loc_Lt'Access);
   end Sort_By_Location;

   --  Sorting function by ref field.
   --  If ref fields are the same, then compare by location.
   function Node_Lt (Op1, Op2 : Natural) return Boolean
   is
      L1, L2 : Location_Type;
      N1, N2 : Iir;
      K1, K2 : Xref_Kind;
   begin
      L1 := Get_Location (Get_Xref_Node (Op1));
      L2 := Get_Location (Get_Xref_Node (Op2));

      if L1 /= L2 then
         return L1 < L2;
      end if;

      --  L1 = L2.
      --  Note: nodes of std_standard have the same location.  FIXME ?
      N1 := Get_Xref_Node (Op1);
      N2 := Get_Xref_Node (Op2);
      if Vhdl.Nodes."/=" (N1, N2) then
         return Vhdl.Nodes_Priv."<" (N1, N2);
      end if;

      --  Try to get declaration first.
      K1 := Get_Xref_Kind (Op1);
      K2 := Get_Xref_Kind (Op2);
      if K1 /= K2 then
         return K1 < K2;
      end if;
      L1 := Get_Xref_Location (Op1);
      L2 := Get_Xref_Location (Op2);
      return L1 < L2;
   end Node_Lt;

   procedure Sort_By_Node_Location is
   begin
      GNAT.Heap_Sort_A.Sort (Xref_Table.Last, Move'Access, Node_Lt'Access);
   end Sort_By_Node_Location;

   function Find (Loc : Location_Type) return Xref
   is
      Low : Xref;
      High : Xref;
      Mid : Xref;
      Mid_Loc : Location_Type;
   begin
      Low := First_Xref;
      High := Xref_Table.Last;
      loop
         Mid := (Low + High + 1) / 2;
         Mid_Loc := Xref_Table.Table (Mid).Loc;
         if Loc = Mid_Loc then
            return Mid;
         end if;
         if Mid = Low then
            return Bad_Xref;
         end if;
         if Loc > Mid_Loc then
            Low := Mid + 1;
         else
            High := Mid - 1;
         end if;
      end loop;
   end Find;

   procedure Fix_End_Xrefs
   is
      N : Iir;
   begin
      for I in First_Xref .. Get_Last_Xref loop
         if Get_Xref_Kind (I) = Xref_End then
            N := Get_Xref_Node (I);
            case Get_Kind (N) is
               when Iir_Kind_Function_Body
                 | Iir_Kind_Procedure_Body =>
                  Xref_Table.Table (I).Ref := Get_Subprogram_Specification (N);
               when others =>
                  null;
            end case;
         end if;
      end loop;
   end Fix_End_Xrefs;
end Vhdl.Xrefs;
