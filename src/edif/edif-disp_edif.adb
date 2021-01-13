--  EDIF printer.
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Text_IO; use Ada.Text_IO;
with Types; use Types;
with Str_Table;
with Name_Table;

package body Edif.Disp_Edif is
   procedure Disp (N : Node; Indent : Natural);

   procedure Disp_Int32 (V : Int32)
   is
      S : constant String := Int32'Image (V);
   begin
      if S (1) = ' ' then
         Put (S (2 .. S'Last));
      else
         Put (S);
      end if;
   end Disp_Int32;

   procedure Disp_Symbol (S : Name_Id)
   is
      Img : constant String := Name_Table.Image (S);
   begin
      if Img (Img'First) not in 'a' .. 'z' then
         Put ('&');
      end if;
      Put (Img);
   end Disp_Symbol;

   procedure Disp_Indent (Indent : Natural) is
   begin
      Put ((1 .. 2 * Indent => ' '));
   end Disp_Indent;

   procedure Disp_Chain (Chain : Node; Indent : Natural)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         Disp (N, Indent);
         N := Get_Chain (N);
      end loop;
   end Disp_Chain;

   procedure Disp_Keyword_Head (Name : String; Indent : Natural) is
   begin
      Disp_Indent (Indent);
      Put ('(');
      Put (Name);
      Put (' ');
   end Disp_Keyword_Head;

   procedure Disp_Keyword_Tail is
   begin
      Put (')');
      New_Line;
   end Disp_Keyword_Tail;

   procedure Disp_Keyword (Name : String; Arg : Int32; Indent : Natural) is
   begin
      Disp_Keyword_Head (Name, Indent);
      Disp_Int32 (Arg);
      Disp_Keyword_Tail;
   end Disp_Keyword;

   procedure Disp_Keyword (Name : String; Arg : Node; Indent : Natural) is
   begin
      Disp_Keyword_Head (Name, Indent);
      Disp (Arg, Indent + 1);
      Disp_Keyword_Tail;
   end Disp_Keyword;

   procedure Disp_Keyword (Name : String; Arg : Name_Id; Indent : Natural) is
   begin
      Disp_Keyword_Head (Name, Indent);
      Disp_Symbol (Arg);
      Disp_Keyword_Tail;
   end Disp_Keyword;

   procedure Disp_Decl_Head (Name : String; N : Node; Indent : Natural) is
   begin
      Disp_Keyword_Head (Name, Indent);
      Disp (Get_Name (N), Indent);
      New_Line;
   end Disp_Decl_Head;

   procedure Disp_Decl_Tail (Indent : Natural) is
   begin
      Disp_Indent (Indent);
      Disp_Keyword_Tail;
   end Disp_Decl_Tail;

   procedure Disp_Opt (N : Node; Indent : Natural) is
   begin
      if N /= Null_Node then
         Disp (N, Indent);
      end if;
   end Disp_Opt;

   procedure Disp (N : Node; Indent : Natural) is
   begin
      if N = Null_Node then
         Put ("()");
         return;
      end if;

      case Get_Kind (N) is
         when N_Keyword =>
            declare
               El : Node;
            begin
               New_Line;
               Disp_Indent (Indent);
               Put ('(');
               Put (Name_Table.Image (Get_Keyword (N)));
               El := Get_CDR (N);
               while El /= Null_Node loop
                  Put (' ');
                  Disp (Get_CAR (El), Indent + 1);
                  El := Get_CDR (El);
               end loop;
               Put (')');
            end;

         when N_Symbol =>
            Disp_Symbol (Get_Symbol (N));

         when N_Number =>
            Disp_Int32 (Get_Number (N));

         when N_String =>
            Put ('"');
            Put (Str_Table.String_String8
                   (Get_String_Id (N), Nat32 (Get_String_Len (N))));
            Put ('"');

         when N_Edif =>
            Disp_Decl_Head ("edif", N, Indent);
            Disp_Keyword ("edifversion", Get_Edif_Version (N), Indent + 1);
            Disp_Keyword ("ediflevel", Get_Edif_Level (N), Indent + 1);
            Disp_Keyword ("keywordmap", Get_Keyword_Map (N), Indent + 1);
            Disp_Keyword ("status", Get_Status (N), Indent + 1);
            Disp_Chain (Get_External_Chain (N), Indent + 1);
            Disp_Chain (Get_Library_Chain (N), Indent + 1);
            Disp (Get_Design (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Library =>
            Disp_Decl_Head ("library", N, Indent);
            Disp_Keyword ("ediflevel", Get_Edif_Level (N), Indent + 1);
            Disp_Keyword ("technology", Get_Technology (N), Indent + 1);
            Disp_Chain (Get_Cells_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_External =>
            Disp_Decl_Head ("external", N, Indent);
            Disp_Keyword ("ediflevel", Get_Edif_Level (N), Indent + 1);
            Disp_Keyword ("technology", Get_Technology (N), Indent + 1);
            Disp_Chain (Get_Cells_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Cell =>
            Disp_Decl_Head ("cell", N, Indent);
            Disp_Keyword ("celltype", Get_Cell_Type (N), Indent + 1);
            Disp (Get_View (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_View =>
            Disp_Decl_Head ("view", N, Indent);
            Disp_Keyword ("viewtype", Get_View_Type (N), Indent + 1);
            Disp (Get_Interface (N), Indent + 1);
            declare
               Contents : constant Node := Get_Contents_Chain (N);
            begin
               if Contents /= Null_Node then
                  Disp_Keyword_Head ("contents", Indent + 1);
                  New_Line;
                  Disp_Chain (Contents, Indent + 2);
                  Disp_Indent (Indent + 1);
                  Disp_Keyword_Tail;
               end if;
            end;
            Disp_Decl_Tail (Indent);

         when N_Interface =>
            Disp_Keyword_Head ("interface", Indent);
            New_Line;
            Disp_Chain (Get_Ports_Chain (N), Indent + 1);
            Disp_Chain (Get_Properties_Chain (N), Indent + 1);
            Disp_Indent (Indent);
            Disp_Keyword_Tail;

         when N_Port =>
            Disp_Decl_Head ("port", N, Indent);
            Disp_Keyword_Head ("direction", Indent + 1);
            case Get_Direction (N) is
               when Dir_Input =>
                  Put ("input");
               when Dir_Output =>
                  Put ("output");
               when Dir_Inout =>
                  Put ("inout");
            end case;
            Disp_Keyword_Tail;
            Disp_Decl_Tail (Indent);

         when N_Rename =>
            Put ("(rename ");
            Disp (Get_Name (N), Indent);
            Put (' ');
            Disp (Get_String (N), Indent);
            Put (')');

         when N_Member =>
            Put ("(member ");
            Disp (Get_Name (N), Indent);
            Put (' ');
            Disp_Int32 (Get_Index (N));
            Put (')');

         when N_Array =>
            Put ("(array ");
            Disp (Get_Name (N), Indent);
            Put (' ');
            Disp_Int32 (Get_Array_Length (N));
            Put (')');

         when N_Instance =>
            Disp_Decl_Head ("instance", N, Indent);
            Disp (Get_Instance_Ref (N), Indent + 1);
            Disp_Chain (Get_Port_Instances_Chain (N), Indent + 1);
            Disp_Chain (Get_Properties_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Net =>
            Disp_Decl_Head ("net", N, Indent);
            Disp_Chain (Get_Joined_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_View_Ref =>
            Disp_Decl_Head ("viewref", N, Indent);
            Disp (Get_Cell_Ref (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Cell_Ref =>
            Disp_Keyword_Head ("cellref", Indent);
            Disp (Get_Name (N), Indent);
            Disp_Opt (Get_Library_Ref (N), Indent + 1);
            Disp_Keyword_Tail;

         when N_Port_Ref =>
            Disp_Keyword_Head ("portref", Indent);
            Disp (Get_Port (N), Indent);
            Disp_Opt (Get_Instance_Ref (N), Indent + 1);
            Disp_Keyword_Tail;

         when N_Property =>
            Disp_Keyword_Head ("property", Indent);
            Disp (Get_Name (N), Indent);
            Put (' ');
            Disp (Get_Value (N), Indent);
            Disp_Keyword_Tail;

         when N_Port_Instance =>
            Disp_Decl_Head ("portinstance", N, Indent);
            Disp_Chain (Get_Properties_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Design =>
            Disp_Decl_Head ("design", N, Indent);
            Disp (Get_Cell_Ref (N), Indent + 1);
            Disp_Chain (Get_Properties_Chain (N), Indent + 1);
            Disp_Decl_Tail (Indent);

         when N_Boolean =>
            if Get_Boolean (N) then
               Put ("(true)");
            else
               Put ("(false)");
            end if;

         when others =>
            Put ("??? " & Nkind'Image (Get_Kind (N)));
      end case;
   end Disp;

   procedure Disp_Node (N : Node) is
   begin
      Disp (N, 0);
   end Disp_Node;
end Edif.Disp_Edif;
