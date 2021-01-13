--  Lexical analysis for protect directive.
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

separate (Vhdl.Scanner)
package body Directive_Protect is
   function Scan_Expression_List return Boolean;

   --  Scan/parse a keyword expression.
   --  Initial spaces must have been skipped.
   --  Return False in case of error.
   function Scan_Keyword_Expression return Boolean is
   begin
      if Characters_Kind (Source (Pos)) not in Letter then
         Error_Msg_Scan ("identifier expected in protect directive");
         return False;
      end if;

      Scan_Identifier (False);
      if Current_Token /= Tok_Identifier then
         Error_Msg_Scan (+Get_Token_Location, "keyword must be an identifier");
         return False;
      end if;

      Skip_Spaces;
      if Source (Pos) /= '=' then
         return True;
      end if;

      --  Eat '='.
      Pos := Pos + 1;
      Skip_Spaces;

      case Source (Pos) is
         when 'A' .. 'Z' | 'a' .. 'z' =>
            Scan_Identifier (False);
         when '0' .. '9' =>
            Scan_Literal;
         when '"' =>
            Scan_String;
         when '(' =>
            --  Eat '('.
            Pos := Pos + 1;
            Skip_Spaces;

            if not Scan_Expression_List then
               return False;
            end if;

            Skip_Spaces;
            if Source (Pos) /= ')' then
               Error_Msg_Scan ("')' expected at end of protect keyword list");
               return False;
            end if;

            --  Eat ')'.
            Pos := Pos + 1;

         when others =>
            --  Ok, we don't handle all the letters, nor extended identifiers.
            Error_Msg_Scan ("literal expected in protect tool directive");
            return False;
      end case;

      return True;
   end Scan_Keyword_Expression;

   --  Scan: keyword_expression { , keyword_expression }
   function Scan_Expression_List return Boolean is
   begin
      loop
         if not Scan_Keyword_Expression then
            return False;
         end if;

         Skip_Spaces;

         if Source (Pos) /= ',' then
            return True;
         end if;

         --  Eat ','.
         Pos := Pos + 1;

         Skip_Spaces;
      end loop;
   end Scan_Expression_List;

   --  LRM08 24.1 Protect tool directives
   --  protect_directive ::=
   --    `PROTECT keyword_expression {, keyword_expression }
   procedure Scan_Protect_Directive is
   begin
      if Scan_Expression_List then
         if not Is_EOL (Source (Pos)) then
            Error_Msg_Scan ("end of line expected in protect directive");
         end if;
      end if;

      Skip_Until_EOL;
   end Scan_Protect_Directive;
end Directive_Protect;
