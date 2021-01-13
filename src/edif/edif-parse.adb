--  EDIF parser.
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

with Types; use Types;
with Std_Names; use Std_Names;
with Errorout; use Errorout;
with Edif.Tokens; use Edif.Tokens;
with Edif.Scans; use Edif.Scans;
with Edif.Nutils; use Edif.Nutils;

package body Edif.Parse is
   Parse_Error : exception;

   procedure Error_Msg_Parse (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, +Get_Token_Location, Msg, Args);
   end Error_Msg_Parse;

   procedure Error_Msg_Parse (Msg : String; Arg : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Errorout.Parse, +Get_Token_Location,
                  Msg, (1 => Arg));
   end Error_Msg_Parse;

   procedure Set_Token_Location (N : Node) is
   begin
      Set_Location (N, Get_Token_Location);
   end Set_Token_Location;

   function Parse_Simple return Node;

   procedure Parse_Simple_List_Body (Bod : Node)
   is
      Last : Node;
      El : Node;
   begin
      Last := Bod;
      while Current_Token /= Tok_Right_Paren loop
         if Current_Token = Tok_Eof then
            Error_Msg_Parse ("missing ')'");
            exit;
         end if;
         El := Create_Node (N_Chain);
         Set_Token_Location (El);
         Set_CDR (Last, El);
         Set_CAR (El, Parse_Simple);
         Last := El;
      end loop;

      Set_CDR (Last, Null_Node);

      if Current_Token = Tok_Right_Paren then
         --  Skip ')'.
         Scan;
      end if;
   end Parse_Simple_List_Body;

   function Parse_Simple_List return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Keyword);
      Set_Token_Location (Res);

      Set_Keyword (Res, Current_Identifier);

      --  Skip '(' + keyword.
      Scan;

      Parse_Simple_List_Body (Res);

      return Res;
   end Parse_Simple_List;

   function Parse_Symbol return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Symbol);

      Res := Create_Node (N_Symbol);
      Set_Token_Location (Res);
      Set_Symbol (Res, Current_Identifier);

      --  Skip symbol.
      Scan;

      return Res;
   end Parse_Symbol;

   function Parse_String return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_String);
      Set_Token_Location (Res);
      Set_String_Id (Res, Current_String);
      Set_String_Len (Res, Current_String_Len);

      --  Skip string.
      Scan;

      return Res;
   end Parse_String;

   function Parse_Number return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Number);
      Set_Token_Location (Res);
      Set_Number (Res, Current_Number);

      --  Skip number.
      Scan;

      return Res;
   end Parse_Number;

   function Parse_Simple return Node is
   begin
      case Current_Token is
         when Tok_Keyword =>
            return Parse_Simple_List;

         when Tok_Right_Paren =>
            Error_Msg_Parse ("unexpected ')'");

            --  Skip it.
            Scan;

            return Parse_Simple;

         when Tok_Eof =>
            Error_Msg_Parse ("unexpected end of file");
            return Null_Node;

         when Tok_Symbol =>
            return Parse_Symbol;

         when Tok_String =>
            return Parse_String;

         when Tok_Number =>
            return Parse_Number;
      end case;
   end Parse_Simple;

   function Parse_File_Simple return Node
   is
      Res : Node;
   begin
      --  Start the scanner.
      Scan;

      Res := Parse_Simple;

      return Res;
   end Parse_File_Simple;

   procedure Expect_Keyword (Id : Name_Id) is
   begin
      if Current_Token /= Tok_Keyword
        or else Current_Identifier /= Id
      then
         Error_Msg_Parse ("keyword %i expected here", +Id);
         raise Parse_Error;
      end if;
   end Expect_Keyword;

   procedure Expect_Symbol is
   begin
      if Current_Token /= Tok_Symbol then
         Error_Msg_Parse ("symbol expected here");
         raise Parse_Error;
      end if;
   end Expect_Symbol;

   procedure Expect_String is
   begin
      if Current_Token /= Tok_String then
         Error_Msg_Parse ("string expected here");
         raise Parse_Error;
      end if;
   end Expect_String;

   procedure Skip_Right_Paren is
   begin
      while Current_Token = Tok_Keyword loop
         Error_Msg_Parse
           ("unexpected %i keyword, skipped", +Current_Identifier);

         declare
            Count : Natural;
         begin
            Count := 1;
            loop
               --  Skip.
               Scan;

               if Current_Token = Tok_Right_Paren then
                  Count := Count - 1;
                  exit when Count = 0;
               end if;
            end loop;

            --  Skip ')'.
            Scan;
         end;
      end loop;

      if Current_Token /= Tok_Right_Paren then
         Error_Msg_Parse ("')' expected here");
      else
         --  Skip ')'.
         Scan;
      end if;
   end Skip_Right_Paren;

   procedure Expect_Number is
   begin
      if Current_Token /= Tok_Number then
         Error_Msg_Parse ("number expected here");
         raise Parse_Error;
      end if;
   end Expect_Number;

   procedure Skip_Comments is
   begin
      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Comment
      loop
         --  Skip '(comment'.
         Scan;

         if Current_Token = Tok_String then
            --  Skip string.
            Scan;
         else
            Error_Msg_Parse ("string expected after comment");
         end if;

         --  Skip ')' (for comment).
         Skip_Right_Paren;
      end loop;
   end Skip_Comments;

   function Parse_Rename return Node
   is
      Res : Node;
   begin
      pragma Assert (Current_Token = Tok_Keyword);

      --  Skip 'rename'.
      Scan;

      Res := Create_Node (N_Rename);
      Expect_Symbol;
      Set_Name (Res, Parse_Symbol);

      Expect_String;
      Set_String (Res, Parse_String);

      Skip_Right_Paren;

      return Res;
   end Parse_Rename;

   function Parse_Name return Node is
   begin
      if Current_Token = Tok_Symbol then
         return Parse_Symbol;
      elsif Current_Token = Tok_Keyword then
         if Current_Identifier = Name_Rename then
            return Parse_Rename;
         end if;
      end if;

      Error_Msg_Parse ("symbol or rename expected");
      raise Parse_Error;
   end Parse_Name;

   function Parse_Boolean return Node
   is
      Res : Node;
   begin
      if Current_Token /= Tok_Keyword then
         Error_Msg_Parse ("true or false expected");
         raise Parse_Error;
      end if;

      Res := Create_Node (N_Boolean);
      Set_Token_Location (Res);

      if Current_Identifier = Name_True then
         Set_Boolean (Res, True);
      elsif Current_Identifier = Name_False then
         Set_Boolean (Res, False);
      else
         Error_Msg_Parse ("true or false expected");
      end if;

      --  Skip keyword.
      Scan;

      --  Skip ')' (for true/false).
      Skip_Right_Paren;

      return Res;
   end Parse_Boolean;

   function Parse_Userdata return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Userdata);
      Set_Token_Location (Res);

      --  Skip '(property' or '(userdata'.
      Scan;

      Set_Name (Res, Parse_Name);

      Parse_Simple_List_Body (Res);

      return Res;
   end Parse_Userdata;

   function Parse_Property return Node
   is
      Prop : Node;
      Ptype : Name_Id;
   begin
      Prop := Create_Node (N_Property);
      Set_Token_Location (Prop);

      --  Skip '(property' or '(userdata'.
      Scan;

      Set_Name (Prop, Parse_Name);

      if Current_Token /= Tok_Keyword then
         Error_Msg_Parse ("property value expected");
         raise Parse_Error;
      end if;

      Ptype := Current_Identifier;

      --  Skip type keyword.
      Scan;

      case Ptype is
         when Name_String =>
            Set_Value (Prop, Parse_String);

         when Name_Integer =>
            Set_Value (Prop, Parse_Number);

         when Name_Boolean =>
            Set_Value (Prop, Parse_Boolean);

         when Name_Number =>
            Set_Value (Prop, Parse_Simple);

         when others =>
            Error_Msg_Parse ("unknown property type %i", +Ptype);
            raise Parse_Error;
      end case;

      --  Skip ')' (for value).
      Skip_Right_Paren;

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Unit
      then
         --  Skip '(unit'.
         Scan;

         Expect_Symbol;
         Set_Unit (Prop, Current_Identifier);
         Scan;

         --  Skip ')' (for unit).
         Skip_Right_Paren;
      end if;

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Owner
      then
         --  Skip '(owner'.
         Scan;

         Expect_String;
            Set_Owner (Prop, Parse_String);

            --  Skip ')' (for owner).
            Skip_Right_Paren;
      end if;

      --  Skip ')' (for property).
      Skip_Right_Paren;

      return Prop;
   end Parse_Property;

   procedure Parse_Properties (Constr : in out Constr_Type)
   is
      El : Node;
   begin
      while Current_Token = Tok_Keyword loop
         case Current_Identifier is
            when Name_Property =>
               El := Parse_Property;
            when Name_Userdata =>
               El := Parse_Userdata;
            when others =>
               exit;
         end case;
         Append_Node (Constr, El);
      end loop;
   end Parse_Properties;

   procedure Parse_Properties (N : Node)
   is
      Constr : Constr_Type;
   begin
      Init_Constr (Constr);

      Parse_Properties (Constr);

      Set_Properties_Chain (N, Get_Constr_Chain (Constr));
   end Parse_Properties;

   function Parse_Edif_Version return Int32
   is
      Res : Int32;
   begin
      Expect_Keyword (Name_Edifversion);

      --  Skip '(edifversion'.
      Scan;

      --  Major
      --  FIXME: small number.
      Expect_Number;
      Res := Current_Number * 100;
      Scan;

      --  Minor
      Expect_Number;
      Res := Res + Current_Number * 10;
      Scan;

      --  Revision
      Expect_Number;
      Res := Res + Current_Number;
      Scan;

      --  Skip ')'.
      Skip_Right_Paren;

      return Res;
   end Parse_Edif_Version;

   function Parse_Edif_Level return Int32
   is
      Res : Int32;
   begin
      Expect_Keyword (Name_Ediflevel);

      --  Skip '(ediflevel'.
      Scan;

      Expect_Number;
      Res := Current_Number;

      --  Skip number.
      Scan;

      --  Skip ')'.
      Skip_Right_Paren;

      return Res;
   end Parse_Edif_Level;

   function Parse_Technology return Node is
   begin
      Expect_Keyword (Name_Technology);

      return Parse_Simple_List;
   end Parse_Technology;

   procedure Parse_Designator_Opt (N : Node) is
   begin
      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Designator
      then
         --  Skip '(designator'.
         Scan;

         Expect_String;
         Set_Designator (N, Parse_String);

         --  Skip ')' (for designator).
         Skip_Right_Paren;
      end if;
   end Parse_Designator_Opt;

   function Parse_Array return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Array);
      Set_Token_Location (Res);

      --  Skip '(array'.
      Scan;

      Set_Name (Res, Parse_Name);

      Expect_Number;
      Set_Array_Length (Res, Current_Number);

      --  Skip number.
      Scan;

      --  Skip ')' (for array).
      Skip_Right_Paren;

      return Res;
   end Parse_Array;

   function Parse_Port return Node
   is
      -- Constr : Constr_Type;
      Res : Node;
      Name : Node;
   begin
      Res := Create_Node (N_Port);
      Set_Token_Location (Res);

      --  Skip '(port'.
      Scan;

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Array
      then
         Name := Parse_Array;
      else
         Name := Parse_Name;
      end if;
      Set_Name (Res, Name);

      Expect_Keyword (Name_Direction);
      --  Skip '(direction'.
      Scan;

      Expect_Symbol;
      case Current_Identifier is
         when Name_Input =>
            Set_Direction (Res, Dir_Input);
         when Name_Output =>
            Set_Direction (Res, Dir_Output);
         when Name_Inout =>
            Set_Direction (Res, Dir_Inout);
         when others =>
            Error_Msg_Parse
              ("unhandled port direction %i", +Current_Identifier);
            raise Parse_Error;
      end case;

      --  Skip symbol.
      Scan;

      --  Skip ')' (for direction).
      Skip_Right_Paren;

      Parse_Designator_Opt (Res);

      Parse_Properties (Res);

      --  Skip ')' (for port).
      Skip_Right_Paren;

      return Res;
   end Parse_Port;

   function Parse_Library_Ref_Opt return Node
   is
      Res : Node;
   begin
      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Libraryref
      then
         --  Skip '(libraryref'.
         Scan;

         Expect_Symbol;
         Res := Parse_Symbol;

         --  Skip ')' (for libraryref).
         Skip_Right_Paren;
      else
         Res := Null_Node;
      end if;

      return Res;
   end Parse_Library_Ref_Opt;

   function Parse_Cell_Ref return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Cell_Ref);
      Set_Token_Location (Res);

      --  Skip '(cellref'.
      Scan;

      Expect_Symbol;
      Set_Name (Res, Parse_Symbol);

      Set_Library_Ref (Res, Parse_Library_Ref_Opt);

      --  Skip ')' (for cellref).
      Skip_Right_Paren;

      return Res;
   end Parse_Cell_Ref;

   function Parse_View_Ref return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_View_Ref);
      Set_Token_Location (Res);

      Expect_Keyword (Name_Viewref);

      --  Skip '(viewref'.
      Scan;

      Set_Name (Res, Parse_Name);

      Set_Cell_Ref (Res, Parse_Cell_Ref);

      --  Skip ')' (for viewref).
      Skip_Right_Paren;

      return Res;
   end Parse_View_Ref;

   procedure Parse_Port_Instances (N : Node)
   is
      Constr : Constr_Type;
      Inst : Node;
   begin
      Init_Constr (Constr);

      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Portinstance
      loop
         Inst := Create_Node (N_Port_Instance);
         Set_Token_Location (Inst);
         Append_Node (Constr, Inst);

         --  Skip '(portinstance'.
         Scan;

         Set_Name (Inst, Parse_Name);
         Parse_Properties (Inst);

         --  Skip ')' (for portinstance).
         Skip_Right_Paren;

      end loop;

      Set_Port_Instances_Chain (N, Get_Constr_Chain (Constr));
   end Parse_Port_Instances;

   function Parse_Instance return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Instance);
      Set_Token_Location (Res);

      --  Skip '(instance'.
      Scan;

      Set_Name (Res, Parse_Name);
      Set_Instance_Ref (Res, Parse_View_Ref);

      Parse_Port_Instances (Res);

      Parse_Properties (Res);

      --  Skip ')' (for instance).
      Skip_Right_Paren;

      return Res;
   end Parse_Instance;

   function Parse_Member return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Member);
      Set_Token_Location (Res);

      --  Skip '(member'.
      Scan;

      Set_Name (Res, Parse_Name);

      Expect_Number;
      Set_Index (Res, Current_Number);

      --  Skip number.
      Scan;

      --  Skip ')' (for member).
      Skip_Right_Paren;

      return Res;
   end Parse_Member;

   function Parse_Net return Node
   is
      Constr : Constr_Type;
      Res : Node;
      Ref : Node;
   begin
      Res := Create_Node (N_Net);
      Set_Token_Location (Res);

      --  Skip '(net'.
      Scan;

      Set_Name (Res, Parse_Name);

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Joined
      then
         Init_Constr (Constr);

         --  Skip '(joined'.
         Scan;

         while Current_Token = Tok_Keyword
           and then Current_Identifier = Name_Portref
         loop
            Ref := Create_Node (N_Port_Ref);
            Set_Token_Location (Ref);
            Append_Node (Constr, Ref);

            --  Skip '(portref'.
            Scan;

            if Current_Token = Tok_Keyword
              and then Current_Identifier = Name_Member
            then
               Set_Port (Ref, Parse_Member);
            else
               Set_Port (Ref, Parse_Name);
            end if;

            if Current_Token = Tok_Keyword
              and then Current_Identifier = Name_Instanceref
            then
               --  Skip '(instanceref'.
               Scan;

               Set_Instance_Ref (Ref, Parse_Name);

               --  Skip ')' (for instanceref).
               Skip_Right_Paren;
            end if;

            --  Skip ')' (for portref).
            Skip_Right_Paren;
         end loop;
         Set_Joined_Chain (Res, Get_Constr_Chain (Constr));

         --  Skip ')' (for joined).
         Skip_Right_Paren;
      end if;

      Parse_Properties (Res);

      --  Skip ')' (for net).
      Skip_Right_Paren;

      return Res;
   end Parse_Net;

   function Parse_Interface return Node
   is
      Constr : Constr_Type;
      Res : Node;
   begin
      Res := Create_Node (N_Interface);
      Set_Token_Location (Res);

      --  Skip '(interface'.
      Scan;

      Parse_Designator_Opt (Res);

      Init_Constr (Constr);
      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Port
      loop
         Append_Node (Constr, Parse_Port);
      end loop;
      Set_Ports_Chain (Res, Get_Constr_Chain (Constr));

      Parse_Designator_Opt (Res);

      Parse_Properties (Res);

      --  Skip ')' (for interface).
      Skip_Right_Paren;

      return Res;
   end Parse_Interface;

   function Parse_View return Node
   is
      Constr : Constr_Type;
      Prop_Constr : Constr_Type;
      Res : Node;
   begin
      Res := Create_Node (N_View);
      Set_Token_Location (Res);

      --  Skip '(view'.
      Scan;

      Set_Name (Res, Parse_Name);

      Expect_Keyword (Name_Viewtype);

      --  Skip '(viewtype'.
      Scan;

      Expect_Symbol;
      Set_View_Type (Res, Current_Identifier);

      --  Skip symbol.
      Scan;

      --  Skip ')' (for viewtype).
      Skip_Right_Paren;

      Expect_Keyword (Name_Interface);
      Set_Interface (Res, Parse_Interface);

      Init_Constr (Prop_Constr);
      Parse_Properties (Prop_Constr);

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Contents
      then
         --  Skip '(contents'.
         Scan;

         Init_Constr (Constr);
         while Current_Token = Tok_Keyword loop
            case Current_Identifier is
               when Name_Instance =>
                  Append_Node (Constr, Parse_Instance);
               when Name_Net =>
                  Append_Node (Constr, Parse_Net);
               when Name_Userdata =>
                  Append_Node (Constr, Parse_Userdata);
               when others =>
                  Error_Msg_Parse
                    ("%i not supported in contents", +Current_Identifier);
                  raise Parse_Error;
            end case;
         end loop;
         Set_Contents_Chain (Res, Get_Constr_Chain (Constr));

         --  Skip ')' (for contents).
         Skip_Right_Paren;
      end if;

      Parse_Properties (Prop_Constr);
      Set_Properties_Chain (Res, Get_Constr_Chain (Prop_Constr));

      --  Skip ')' (for view).
      Skip_Right_Paren;

      return Res;
   end Parse_View;

   function Parse_Cell return Node
   is
      Res : Node;
      Status : Node;
      Prop_Constr : Constr_Type;
      pragma Unreferenced (Status);
   begin
      Res := Create_Node (N_Cell);

      --  Skip '(cell'.
      Scan;

      Set_Name (Res, Parse_Name);

      Expect_Keyword (Name_Celltype);

      --  Skip '(celltype'.
      Scan;

      Expect_Symbol;
      Set_Cell_Type (Res, Current_Identifier);

      --  Skip symbol.
      Scan;

      --  Skip ')' (for celltype).
      Skip_Right_Paren;

      Skip_Comments;

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Status
      then
         Status := Parse_Simple_List;
         --  FIXME: free.
      end if;

      Init_Constr (Prop_Constr);
      Parse_Properties (Prop_Constr);

      Set_View (Res, Parse_View);

      --  Properties can appear at several places...
      Parse_Properties (Prop_Constr);
      Set_Properties_Chain (Res, Get_Constr_Chain (Prop_Constr));

      --  Skip ')' (for cell).
      Skip_Right_Paren;

      return Res;
   end Parse_Cell;

   procedure Parse_Library_Body (Res : Node)
   is
      Constr : Constr_Type;
   begin
      --  Skip '(external'.
      Scan;

      Set_Name (Res, Parse_Name);

      Set_Edif_Level (Res, Parse_Edif_Level);
      Set_Technology (Res, Parse_Technology);

      Init_Constr (Constr);
      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Cell
      loop
         Append_Node (Constr, Parse_Cell);
      end loop;
      Set_Cells_Chain (Res, Get_Constr_Chain (Constr));

      --  Skip ')' (for external).
      Skip_Right_Paren;
   end Parse_Library_Body;

   function Parse_Library return Node
   is
      Res : Node;
   begin
      Expect_Keyword (Name_Library);

      Res := Create_Node (N_Library);
      Set_Token_Location (Res);

      Parse_Library_Body (Res);

      return Res;
   end Parse_Library;

   function Parse_External return Node
   is
      Res : Node;
   begin
      Expect_Keyword (Name_External);

      Res := Create_Node (N_External);
      Set_Token_Location (Res);

      Parse_Library_Body (Res);

      return Res;
   end Parse_External;

   function Parse_Design return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Design);
      Set_Token_Location (Res);

      --  Skip '(design'.
      Scan;

      Set_Name (Res, Parse_Name);
      Set_Cell_Ref (Res, Parse_Cell_Ref);
      Parse_Properties (Res);

      --  Skip ')' (for design).
      Skip_Right_Paren;

      return Res;
   end Parse_Design;

   function Parse_Edif200 return Node
   is
      Res : Node;
      Constr : Constr_Type;
   begin
      --  Start the scanner.
      Scan;

      Expect_Keyword (Name_Edif);

      --  Skip '(edif'
      Scan;

      Res := Create_Node (N_Edif);

      Set_Name (Res, Parse_Name);
      Set_Edif_Version (Res, Parse_Edif_Version);
      Set_Edif_Level (Res, Parse_Edif_Level);

      Expect_Keyword (Name_Keywordmap);
      --  Skip '(keywordmap'.
      Scan;
      Set_Keyword_Map (Res, Parse_Simple);
      --  Skip ')' (for keywordmap).
      Skip_Right_Paren;

      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Status
      then
         Set_Status (Res, Parse_Simple);
      end if;

      Skip_Comments;

      --  Parse externals.
      Init_Constr (Constr);
      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_External
      loop
         Append_Node (Constr, Parse_External);
      end loop;
      Set_External_Chain (Res, Get_Constr_Chain (Constr));

      --  Parse libraries.
      Init_Constr (Constr);
      while Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Library
      loop
         Append_Node (Constr, Parse_Library);
      end loop;
      Set_Library_Chain (Res, Get_Constr_Chain (Constr));

      --  Parse design.
      if Current_Token = Tok_Keyword
        and then Current_Identifier = Name_Design
      then
         Set_Design (Res, Parse_Design);
      end if;

      --  Skip ')' (for edif).
      Skip_Right_Paren;

      if Current_Token /= Tok_Eof then
         Error_Msg_Parse ("end of file expected");
      end if;

      return Res;
   end Parse_Edif200;

end Edif.Parse;
