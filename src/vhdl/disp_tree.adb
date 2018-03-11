--  Node displaying (for debugging).
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

--  Display trees in raw form.  Mainly used for debugging.

with Ada.Text_IO; use Ada.Text_IO;
with Name_Table;
with Str_Table;
with Files_Map;
with PSL.Dump_Tree;
with Nodes_Meta;

--  Do not add a use clause for iirs_utils, as it may crash for ill-formed
--  trees, which is annoying while debugging.

package body Disp_Tree is
   --  Max depth for Disp_Iir.  Can be modified from a debugger.
   pragma Warnings (Off);
   Max_Depth : Natural := 10;
   pragma Warnings (On);

   procedure Disp_Header (N : Iir);

   procedure Disp_Tree_List_Flat (Tree_List: Iir_List; Tab: Natural);
   pragma Unreferenced (Disp_Tree_List_Flat);

   procedure Put_Indent (Tab: Natural) is
      Blanks : constant String (1 .. 2 * Tab) := (others => ' ');
   begin
      Put (Blanks);
   end Put_Indent;

   procedure Disp_Iir_Number (Node: Iir)
   is
      Res : String (1 .. 10) := "         ]";
      N : Int32 := Int32 (Node);
   begin
      for I in reverse 2 .. 9 loop
         Res (I) := Character'Val (Character'Pos ('0') + (N mod 10));
         N := N / 10;
         if N = 0 then
            Res (I - 1) := '[';
            Put (Res (I - 1 .. Res'Last));
            return;
         end if;
      end loop;
      Put (Res);
   end Disp_Iir_Number;

   -- For iir.

   procedure Disp_Iir_List
     (Tree_List : Iir_List; Tab : Natural; Depth : Natural)
   is
      It : List_Iterator;
   begin
      case Tree_List is
         when Null_Iir_List =>
            Put_Line ("null-list");
         when Iir_List_All =>
            Put_Line ("list-all");
         when others =>
            New_Line;
            It := List_Iterate (Tree_List);
            while Is_Valid (It) loop
               Put_Indent (Tab);
               Disp_Iir (Get_Element (It), Tab + 1, Depth);
               Next (It);
            end loop;
      end case;
   end Disp_Iir_List;

   procedure Disp_Iir_Flist
     (Tree_Flist : Iir_Flist; Tab : Natural; Depth : Natural)
   is
      El: Iir;
   begin
      if Tree_Flist = Null_Iir_Flist then
         Put_Line ("null-flist");
      elsif Tree_Flist = Iir_Flist_All then
         Put_Line ("flist-all");
      elsif Tree_Flist = Iir_Flist_Others then
         Put_Line ("flist-others");
      else
         New_Line;
         for I in Flist_First .. Flist_Last (Tree_Flist) loop
            El := Get_Nth_Element (Tree_Flist, I);
            Put_Indent (Tab);
            Disp_Iir (El, Tab + 1, Depth);
         end loop;
      end if;
   end Disp_Iir_Flist;

   procedure Disp_Chain (Tree_Chain: Iir; Indent: Natural; Depth : Natural)
   is
      El: Iir;
   begin
      New_Line;
      El := Tree_Chain;
      while El /= Null_Iir loop
         Put_Indent (Indent);
         Disp_Iir (El, Indent + 1, Depth);
         El := Get_Chain (El);
      end loop;
   end Disp_Chain;

   procedure Disp_Tree_Flat_Chain (Tree_Chain: Iir; Tab: Natural)
   is
      El: Iir;
   begin
      El := Tree_Chain;
      while El /= Null_Iir loop
         Disp_Iir (El, Tab, 0);
         El := Get_Chain (El);
      end loop;
   end Disp_Tree_Flat_Chain;
   pragma Unreferenced (Disp_Tree_Flat_Chain);

   procedure Disp_Tree_List_Flat (Tree_List : Iir_List; Tab : Natural)
   is
      It : List_Iterator;
   begin
      case Tree_List is
         when Null_Iir_List =>
            Put_Indent (Tab);
            Put_Line (" null-list");
         when Iir_List_All =>
            Put_Indent (Tab);
            Put_Line (" list-all");
         when others =>
            It := List_Iterate (Tree_List);
            while Is_Valid (It) loop
               Disp_Iir (Get_Element (It), Tab, 0);
               Next (It);
            end loop;
      end case;
   end Disp_Tree_List_Flat;

   function Image_Name_Id (Ident: Name_Id) return String
   is
      use Name_Table;
   begin
      if Ident = Null_Identifier then
         return "<anonymous>";
      elsif Is_Character (Ident) then
         return Image (Ident);
      else
         return '"' & Image (Ident) & '"';
      end if;
   end Image_Name_Id;

   function Image_Iir_Staticness (Static: Iir_Staticness) return String is
   begin
      case Static is
         when Unknown =>
            return "???";
         when None =>
            return "none";
         when Globally =>
            return "global";
         when Locally =>
            return "local";
      end case;
   end Image_Iir_Staticness;

   function Image_Boolean (Bool : Boolean) return String is
   begin
      if Bool then
         return "true";
      else
         return "false";
      end if;
   end Image_Boolean;

   function Image_Iir_Delay_Mechanism (Mech : Iir_Delay_Mechanism)
                                      return String is
   begin
      case Mech is
         when Iir_Inertial_Delay =>
            return "inertial";
         when Iir_Transport_Delay =>
            return "transport";
      end case;
   end Image_Iir_Delay_Mechanism;

   function Image_Iir_Mode (Mode : Iir_Mode) return String is
   begin
      case Mode is
         when Iir_Unknown_Mode =>
            return "???";
         when Iir_Linkage_Mode =>
            return "linkage";
         when Iir_Buffer_Mode =>
            return "buffer";
         when Iir_Out_Mode =>
            return "out";
         when Iir_Inout_Mode =>
            return "inout";
         when Iir_In_Mode =>
            return "in";
      end case;
   end Image_Iir_Mode;

   function Image_Iir_Signal_Kind (Kind : Iir_Signal_Kind) return String is
   begin
      case Kind is
         when Iir_Register_Kind =>
            return "register";
         when Iir_Bus_Kind =>
            return "bus";
      end case;
   end Image_Iir_Signal_Kind;

   function Image_Iir_Pure_State (State : Iir_Pure_State) return String is
   begin
      case State is
         when Pure =>
            return "pure";
         when Impure =>
            return "impure";
         when Maybe_Impure =>
            return "maybe_impure";
         when Unknown =>
            return "unknown";
      end case;
   end Image_Iir_Pure_State;

   function Image_Iir_All_Sensitized (Sig : Iir_All_Sensitized)
                                     return String is
   begin
      case Sig is
         when Unknown =>
            return "???";
         when No_Signal =>
            return "no_signal";
         when Read_Signal =>
            return "read_signal";
         when Invalid_Signal =>
            return "invalid_signal";
      end case;
   end Image_Iir_All_Sensitized;

   function Image_Iir_Constraint (Const : Iir_Constraint) return String is
   begin
      case Const is
         when Unconstrained =>
            return "unconstrained";
         when Partially_Constrained =>
            return "partially constrained";
         when Fully_Constrained =>
            return "fully constrained";
      end case;
   end Image_Iir_Constraint;

   function Image_Date_State_Type (State : Date_State_Type) return String is
   begin
      case State is
         when Date_Extern =>
            return "extern";
         when Date_Disk =>
            return "disk";
         when Date_Parse =>
            return "parse";
         when Date_Analyze =>
            return "analyze";
      end case;
   end Image_Date_State_Type;

   function Image_Tri_State_Type (State : Tri_State_Type) return String is
   begin
      case State is
         when True =>
            return "true";
         when False =>
            return "false";
         when Unknown =>
            return "unknown";
      end case;
   end Image_Tri_State_Type;

   function Image_Time_Stamp_Id (Id : Time_Stamp_Id) return String
     renames Files_Map.Get_Time_Stamp_String;

   function Image_File_Checksum_Id (Id : File_Checksum_Id) return String
     renames Files_Map.Get_File_Checksum_String;

   function Image_Iir_Predefined_Functions (F : Iir_Predefined_Functions)
                                           return String is
   begin
      return Iir_Predefined_Functions'Image (F);
   end Image_Iir_Predefined_Functions;

   procedure Disp_PSL_NFA (N : PSL_NFA; Indent : Natural)
   is
      pragma Unreferenced (Indent);
   begin
      if N = 0 then
         Put_Line ("*null*");
      else
         Put_Line ("*??*");
      end if;
   end Disp_PSL_NFA;

   function Image_Location_Type (Loc : Location_Type) return String is
   begin
      return Files_Map.Image (Loc);
   end Image_Location_Type;

   function Image_Iir_Direction (Dir : Iir_Direction) return String is
   begin
      case Dir is
         when Iir_To =>
            return "to";
         when Iir_Downto =>
            return "downto";
      end case;
   end Image_Iir_Direction;

   function Image_Token_Type (Tok : Tokens.Token_Type) return String
     renames Tokens.Image;

   function Image_String8 (N : Iir) return String
   is
      use Str_Table;
      T : constant Iir := Get_Type (N);
      Str : constant String8_Id := Get_String8_Id (N);
      Len : constant Int32 := Get_String_Length (N);
   begin
      if Is_Null (T) then
         --  Not yet analyzed, the string is the ASCII image.
         return Str_Table.String_String8 (Str, Len);
      else
         declare
            El : constant Iir := Get_Base_Type (Get_Element_Subtype (T));
            Lits : constant Iir_Flist := Get_Enumeration_Literal_List (El);
            Res : String (1 .. Natural (Len));
            C : Natural;
         begin
            for I in 1 .. Len loop
               C := Natural (Element_String8 (Str, I));
               Res (Natural (I)) := Name_Table.Get_Character
                 (Get_Identifier (Get_Nth_Element (Lits, C)));
            end loop;
            return Res;
         end;
      end if;
   end Image_String8;

   procedure Header (Str : String; Indent : Natural) is
   begin
      Put_Indent (Indent);
      Put (Str);
      Put (": ");
   end Header;

   procedure Disp_Header (N : Iir)
   is
      use Nodes_Meta;
      K : Iir_Kind;
   begin
      if N = Null_Iir then
         Put_Line ("*null*");
         return;
      end if;

      K := Get_Kind (N);
      Put (Get_Iir_Image (K));
      if Has_Identifier (K) then
         Put (' ');
         Put (Image_Name_Id (Get_Identifier (N)));
      end if;

      Put (' ');
      Disp_Iir_Number (N);

      --  Be nice: print type name for a type definition.
      if K in Iir_Kinds_Type_And_Subtype_Definition
        or K = Iir_Kind_Wildcard_Type_Definition
      then
         declare
            Decl : constant Iir := Get_Type_Declarator (N);
         begin
            if Decl /= Null_Iir
              and then Get_Identifier (Decl) /= Null_Identifier
            then
               Put (' ');
               Put (Image_Name_Id (Get_Identifier (Decl)));
            end if;
         end;
      end if;

      New_Line;
   end Disp_Header;

   procedure Disp_Iir (N : Iir; Indent : Natural; Depth : Natural)
   is
      Sub_Indent : constant Natural := Indent + 1;
      Ndepth : Natural;
   begin
      Disp_Header (N);

      if Depth = 0 or else N = Null_Iir then
         return;
      end if;

      Header ("location", Indent);
      declare
         L : Location_Type;
      begin
         L := Get_Location (N);
         loop
            Put (Image_Location_Type (L));
            L := Files_Map.Location_Instance_To_Location (L);
            exit when L = No_Location;
            Put (" instantiated at ");
         end loop;
         New_Line;
      end;

      declare
         use Nodes_Meta;
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            Header (Get_Field_Image (F), Indent);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Disp_Iir (Get_Iir (N, F), Sub_Indent, Depth - 1);
                     when Attr_Ref
                       | Attr_Forward_Ref
                       | Attr_Maybe_Forward_Ref =>
                        Disp_Iir (Get_Iir (N, F), Sub_Indent, 0);
                     when Attr_Maybe_Ref =>
                        if Get_Is_Ref (N) then
                           Ndepth := 0;
                        else
                           Ndepth := Depth - 1;
                        end if;
                        Disp_Iir (Get_Iir (N, F), Sub_Indent, Ndepth);
                     when Attr_Chain =>
                        Disp_Chain (Get_Iir (N, F), Sub_Indent, Depth - 1);
                     when Attr_Chain_Next =>
                        Disp_Iir_Number (Get_Iir (N, F));
                        New_Line;
                     when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_List =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Ndepth := Depth - 1;
                     when Attr_Of_Ref =>
                        Ndepth := 0;
                     when Attr_Ref =>
                        Ndepth := 0;
                     when Attr_Of_Maybe_Ref =>
                        if Get_Is_Ref (N) then
                           Ndepth := 0;
                        else
                           Ndepth := Depth - 1;
                        end if;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Disp_Iir_List (Get_Iir_List (N, F), Sub_Indent, Ndepth);
               when Type_Iir_Flist =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Ndepth := Depth - 1;
                     when Attr_Of_Ref =>
                        Ndepth := 0;
                     when Attr_Ref =>
                        Ndepth := 0;
                     when Attr_Of_Maybe_Ref =>
                        if Get_Is_Ref (N) then
                           Ndepth := 0;
                        else
                           Ndepth := Depth - 1;
                        end if;
                     when others =>
                        raise Internal_Error;
                  end case;
                  Disp_Iir_Flist (Get_Iir_Flist (N, F), Sub_Indent, Ndepth);
               when Type_PSL_NFA =>
                  Disp_PSL_NFA (Get_PSL_NFA (N, F), Sub_Indent);
               when Type_String8_Id =>
                  Put_Line ("<string8>");
               when Type_PSL_Node =>
                  PSL.Dump_Tree.Disp_Tree
                    (Get_PSL_Node (N, F), Sub_Indent, Depth - 1);
               when Type_Source_Ptr =>
                  Put_Line (Source_Ptr'Image (Get_Source_Ptr (N, F)));
               when Type_Source_File_Entry =>
                  Put_Line (Source_File_Entry'Image
                              (Get_Source_File_Entry (N, F)));
               when Type_Date_Type =>
                  Put_Line (Date_Type'Image (Get_Date_Type (N, F)));
               when Type_Number_Base_Type =>
                  Put_Line (Number_Base_Type'Image
                              (Get_Number_Base_Type (N, F)));
               when Type_Iir_Constraint =>
                  Put_Line (Image_Iir_Constraint
                              (Get_Iir_Constraint (N, F)));
               when Type_Iir_Mode =>
                  Put_Line (Image_Iir_Mode (Get_Iir_Mode (N, F)));
               when Type_Iir_Index32 =>
                  Put_Line (Iir_Index32'Image (Get_Iir_Index32 (N, F)));
               when Type_Iir_Int64 =>
                  Put_Line (Iir_Int64'Image (Get_Iir_Int64 (N, F)));
               when Type_Boolean =>
                  Put_Line (Image_Boolean
                              (Get_Boolean (N, F)));
               when Type_Iir_Staticness =>
                  Put_Line (Image_Iir_Staticness
                              (Get_Iir_Staticness (N, F)));
               when Type_Date_State_Type =>
                  Put_Line (Image_Date_State_Type
                              (Get_Date_State_Type (N, F)));
               when Type_Iir_All_Sensitized =>
                  Put_Line (Image_Iir_All_Sensitized
                              (Get_Iir_All_Sensitized (N, F)));
               when Type_Iir_Signal_Kind =>
                  Put_Line (Image_Iir_Signal_Kind
                              (Get_Iir_Signal_Kind (N, F)));
               when Type_Tri_State_Type =>
                  Put_Line (Image_Tri_State_Type
                              (Get_Tri_State_Type (N, F)));
               when Type_Iir_Pure_State =>
                  Put_Line (Image_Iir_Pure_State
                              (Get_Iir_Pure_State (N, F)));
               when Type_Iir_Delay_Mechanism =>
                  Put_Line (Image_Iir_Delay_Mechanism
                              (Get_Iir_Delay_Mechanism (N, F)));
               when Type_Iir_Predefined_Functions =>
                  Put_Line (Image_Iir_Predefined_Functions
                              (Get_Iir_Predefined_Functions (N, F)));
               when Type_Iir_Direction =>
                  Put_Line (Image_Iir_Direction
                              (Get_Iir_Direction (N, F)));
               when Type_Iir_Int32 =>
                  Put_Line (Iir_Int32'Image (Get_Iir_Int32 (N, F)));
               when Type_Int32 =>
                  Put_Line (Int32'Image (Get_Int32 (N, F)));
               when Type_Iir_Fp64 =>
                  Put_Line (Iir_Fp64'Image (Get_Iir_Fp64 (N, F)));
               when Type_Time_Stamp_Id =>
                  Put_Line (Image_Time_Stamp_Id
                              (Get_Time_Stamp_Id (N, F)));
               when Type_File_Checksum_Id =>
                  Put_Line (Image_File_Checksum_Id
                              (Get_File_Checksum_Id (N, F)));
               when Type_Token_Type =>
                  Put_Line (Image_Token_Type (Get_Token_Type (N, F)));
               when Type_Name_Id =>
                  Put_Line (Image_Name_Id (Get_Name_Id (N, F)));
            end case;
         end loop;
      end;
   end Disp_Iir;

   procedure Disp_Tree_For_Psl
     (N : Int32; Indent : Natural; Depth : Natural) is
   begin
      Disp_Iir (Iir (N), Indent, Depth);
   end Disp_Tree_For_Psl;

   procedure Disp_Tree (Tree : Iir;
                        Flat : Boolean := false) is
   begin
      if Flat then
         Disp_Iir (Tree, 1, 0);
      else
         Disp_Iir (Tree, 1, Max_Depth);
      end if;
   end Disp_Tree;
end Disp_Tree;
