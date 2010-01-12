--  VHDL lexical scanner.
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Errorout; use Errorout;
with Name_Table;
with Files_Map; use Files_Map;
with Std_Names;
with Str_Table;
with Flags; use Flags;

package body Scan is

   -- This classification is a simplification of the categories of LRM93 13.1
   -- LRM93 13.1
   -- The only characters allowed in the text of a VHDL description are the
   -- graphic characters and format effector.

   type Character_Kind_Type is
      (
   -- Neither a format effector nor a graphic character.
       Invalid,
       Format_Effector,
       Upper_Case_Letter,
       Digit,
       Special_Character,
       Space_Character,
       Lower_Case_Letter,
       Other_Special_Character);

   -- LRM93 13.1
   -- BASIC_GRAPHIC_CHARACTER ::=
   --   UPPER_CASE_LETTER | DIGIT | SPECIAL_CHARACTER | SPACE_CHARACTER
   --subtype Basic_Graphic_Character is
   --  Character_Kind_Type range Upper_Case_Letter .. Space_Character;

   -- LRM93 13.1
   -- GRAPHIC_CHARACTER ::=
   --   BASIC_GRAPHIC_CHARACTER | LOWER_CASE_LETTER | OTHER_SPECIAL_CHARACTER
   -- Note: There is 191 graphic character.
   subtype Graphic_Character is
     Character_Kind_Type range Upper_Case_Letter .. Other_Special_Character;

   -- LRM93 13.1
   -- The characters included in each of the categories of basic graphic
   -- characters are defined as follows:
   type Character_Array is array (Character) of Character_Kind_Type;
   Characters_Kind : constant Character_Array :=
     (NUL .. BS => Invalid,

      -- Format effectors are the ISO (and ASCII) characters called horizontal
      -- tabulation, vertical tabulation, carriage return, line feed, and form
      -- feed.
      HT | LF | VT | FF | CR => Format_Effector,

      SO .. US => Invalid,

      -- 1. upper case letters
      'A' .. 'Z' | UC_A_Grave .. UC_O_Diaeresis |
      UC_O_Oblique_Stroke .. UC_Icelandic_Thorn => Upper_Case_Letter,

      -- 2. digits
      '0' .. '9' => Digit,

      -- 3. special characters
      Quotation | '#' | '&' | ''' | '(' | ')' | '+' | ',' | '-' | '.' | '/'
        | ':' | ';' | '<' | '=' | '>' | '[' | ']'
        | '_' | '|' | '*' => Special_Character,

      -- 4. the space characters
      ' ' | No_Break_Space => Space_Character,

      -- 5. lower case letters
      'a' .. 'z' | LC_German_Sharp_S .. LC_O_Diaeresis |
      LC_O_Oblique_Stroke .. LC_Y_Diaeresis => Lower_Case_Letter,

      -- 6. other special characters
      '!' | '$' | '%' | '@' | '?' | '\' | '^' | '`' | '{' | '}' | '~'
        | Inverted_Exclamation .. Inverted_Question | Multiplication_Sign |
        Division_Sign => Other_Special_Character,

      --  '¡'    -- INVERTED EXCLAMATION MARK
      --  '¢'    -- CENT SIGN
      --  '£'    -- POUND SIGN
      --  '¤'    -- CURRENCY SIGN
      --  '¥'    -- YEN SIGN
      --  '¦'    -- BROKEN BAR
      --  '§'    -- SECTION SIGN
      --  '¨'    -- DIAERESIS
      --  '©'    -- COPYRIGHT SIGN
      --  'ª'    -- FEMININE ORDINAL INDICATOR
      --  '«'    -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      --  '¬'    -- NOT SIGN
      --  '­'    -- SOFT HYPHEN
      --  '®'    -- REGISTERED SIGN
      --  '¯'    -- MACRON
      --  '°'    -- DEGREE SIGN
      --  '±'    -- PLUS-MINUS SIGN
      --  '²'    -- SUPERSCRIPT TWO
      --  '³'    -- SUPERSCRIPT THREE
      --  '´'    -- ACUTE ACCENT
      --  'µ'    -- MICRO SIGN
      --  '¶'    -- PILCROW SIGN
      --  '·'    -- MIDDLE DOT
      --  '¸'    -- CEDILLA
      --  '¹'    -- SUPERSCRIPT ONE
      --  'º'    -- MASCULINE ORDINAL INDICATOR
      --  '»'    -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      --  '¼'    -- VULGAR FRACTION ONE QUARTER
      --  '½'    -- VULGAR FRACTION ONE HALF
      --  '¾'    -- VULGAR FRACTION THREE QUARTERS
      --  '¿'    -- INVERTED QUESTION MARK
      --  '×'    -- MULTIPLICATION SIGN
      --  '÷'    -- DIVISION SIGN

      DEL .. APC => Invalid);

   -- The context contains the whole internal state of the scanner, ie
   -- it can be used to push/pop a lexical analysis, to restart the
   -- scanner from a context marking a previous point.
   type Scan_Context is record
         Source: File_Buffer_Acc;
         Source_File: Source_File_Entry;
         Line_Number: Natural;
         Line_Pos: Source_Ptr;
         Pos: Source_Ptr;
         Token_Pos: Source_Ptr;
         File_Len: Source_Ptr;
         File_Name: Name_Id;
         Token: Token_Type;
         Prev_Token: Token_Type;
         Str_Id : String_Id;
         Str_Len : Nat32;
         Identifier: Name_Id;
         Int64: Iir_Int64;
         Fp64: Iir_Fp64;
   end record;

   -- The current context.
   -- Default value is an invalid context.
   Current_Context: Scan_Context := (Source => null,
                                     Source_File => No_Source_File_Entry,
                                     Line_Number => 0,
                                     Line_Pos => 0,
                                     Pos => 0,
                                     Token_Pos => 0,
                                     File_Len => 0,
                                     File_Name => Null_Identifier,
                                     Token => Tok_Invalid,
                                     Prev_Token => Tok_Invalid,
                                     Identifier => Null_Identifier,
                                     Str_Id => Null_String,
                                     Str_Len => 0,
                                     Int64 => 0,
                                     Fp64 => 0.0);

   Source: File_Buffer_Acc renames Current_Context.Source;
   Pos: Source_Ptr renames Current_Context.Pos;

   -- When CURRENT_TOKEN is an identifier, its name_id is stored into
   -- this global variable.
   -- Function current_text can be used to convert it into an iir.
   function Current_Identifier return Name_Id is
   begin
      return Current_Context.Identifier;
   end Current_Identifier;

   procedure Invalidate_Current_Identifier is
   begin
      Current_Context.Identifier := Null_Identifier;
   end Invalidate_Current_Identifier;

   procedure Invalidate_Current_Token is
   begin
      if Current_Token /= Tok_Invalid then
         Current_Context.Prev_Token := Current_Token;
         Current_Token := Tok_Invalid;
      end if;
   end Invalidate_Current_Token;

   function Current_String_Id return String_Id is
   begin
      return Current_Context.Str_Id;
   end Current_String_Id;

   function Current_String_Length return Nat32 is
   begin
      return Current_Context.Str_Len;
   end Current_String_Length;

   function Current_Iir_Int64 return Iir_Int64 is
   begin
      return Current_Context.Int64;
   end Current_Iir_Int64;

   function Current_Iir_Fp64 return Iir_Fp64 is
   begin
      return Current_Context.Fp64;
   end Current_Iir_Fp64;

   function Get_Current_File return Name_Id is
   begin
      return Current_Context.File_Name;
   end Get_Current_File;

   function Get_Current_Source_File return Source_File_Entry is
   begin
      return Current_Context.Source_File;
   end Get_Current_Source_File;

   function Get_Current_Line return Natural is
   begin
      return Current_Context.Line_Number;
   end Get_Current_Line;

   function Get_Current_Column return Natural
   is
      Col : Natural;
      Name : Name_Id;
   begin
      Coord_To_Position
        (Current_Context.Source_File,
         Current_Context.Line_Pos,
         Integer (Current_Context.Pos - Current_Context.Line_Pos),
         Name, Col);
      return Col;
   end Get_Current_Column;

   function Get_Token_Column return Natural
   is
      Col : Natural;
      Name : Name_Id;
   begin
      Coord_To_Position
        (Current_Context.Source_File,
         Current_Context.Line_Pos,
         Integer (Current_Context.Token_Pos - Current_Context.Line_Pos),
         Name, Col);
      return Col;
   end Get_Token_Column;

   function Get_Token_Position return Source_Ptr is
   begin
      return Current_Context.Token_Pos;
   end Get_Token_Position;

   function Get_Position return Source_Ptr is
   begin
      return Current_Context.Pos;
   end Get_Position;

   procedure Set_File (Source_File : Source_File_Entry)
   is
      N_Source: File_Buffer_Acc;
   begin
      if Current_Context.Source /= null then
         raise Internal_Error;
      end if;
      if Source_File = No_Source_File_Entry then
         raise Internal_Error;
      end if;
      N_Source := Get_File_Source (Source_File);
      Current_Context :=
        (Source => N_Source,
         Source_File => Source_File,
         Line_Number => 1,
         Line_Pos => 0,
         Pos => N_Source'First,
         Token_Pos => 0, -- should be invalid,
         File_Len => Get_File_Length (Source_File),
         File_Name => Get_File_Name (Source_File),
         Token => Tok_Invalid,
         Prev_Token => Tok_Invalid,
         Identifier => Null_Identifier,
         Str_Id => Null_String,
         Str_Len => 0,
         Int64 => -1,
         Fp64 => 0.0);
      Current_Token := Tok_Invalid;
   end Set_File;

   procedure Set_Current_Position (Position: Source_Ptr)
   is
      Loc : Location_Type;
      Offset: Natural;
      File_Entry : Source_File_Entry;
   begin
      if Current_Context.Source = null then
         raise Internal_Error;
      end if;
      Current_Token := Tok_Invalid;
      Current_Context.Pos := Position;
      Loc := File_Pos_To_Location (Current_Context.Source_File,
                                   Current_Context.Pos);
      Location_To_Coord (Loc,
                         File_Entry, Current_Context.Line_Pos,
                         Current_Context.Line_Number, Offset);
   end Set_Current_Position;

   procedure Close_File is
   begin
      Current_Context.Source := null;
   end Close_File;

   -- Emit an error when a character above 128 was found.
   -- This must be called only in vhdl87.
   procedure Error_8bit is
   begin
      Error_Msg_Scan ("8 bits characters not allowed in vhdl87");
   end Error_8bit;

   -- Emit an error when a separator is expected.
   procedure Error_Separator is
   begin
      Error_Msg_Scan ("a separator is required here");
   end Error_Separator;

   -- scan a decimal literal or a based literal.
   --
   -- LRM93 13.4.1
   -- DECIMAL_LITERAL ::= INTEGER [ . INTEGER ] [ EXPONENT ]
   -- EXPONENT ::= E [ + ] INTEGER | E - INTEGER
   --
   -- LRM93 13.4.2
   -- BASED_LITERAL ::= BASE # BASED_INTEGER [ . BASED_INTEGER ] # EXPONENT
   -- BASE ::= INTEGER
   procedure Scan_Literal is separate;

   -- Scan a string literal.
   --
   -- LRM93 13.6
   -- A string literal is formed by a sequence of graphic characters
   -- (possibly none) enclosed between two quotation marks used as string
   -- brackets.
   -- STRING_LITERAL ::= " { GRAPHIC_CHARACTER } "
   --
   -- IN: for a string, at the call of this procedure, the current character
   -- must be either '"' or '%'.
   procedure Scan_String
   is
      -- The quotation character (can be " or %).
      Mark: Character;
      -- Current character.
      C : Character;
      --  Current length.
      Length : Nat32;
   begin
      Mark := Source (Pos);
      if Mark /= Quotation and then Mark /= '%' then
         raise Internal_Error;
      end if;
      Pos := Pos + 1;
      Length := 0;
      Current_Context.Str_Id := Str_Table.Start;
      loop
         C := Source (Pos);
         if C = Mark then
            -- LRM93 13.6
            -- If a quotation mark value is to be represented in the sequence
            -- of character values, then a pair of adjacent quoatation
            -- characters marks must be written at the corresponding place
            -- within the string literal.
            -- LRM93 13.10
            -- Any pourcent sign within the sequence of characters must then
            -- be doubled, and each such doubled percent sign is interpreted
            -- as a single percent sign value.
            -- The same replacement is allowed for a bit string literal,
            -- provieded that both bit string brackets are replaced.
            Pos := Pos + 1;
            exit when Source (Pos) /= Mark;
         end if;

         case Characters_Kind (C) is
            when Format_Effector =>
               Error_Msg_Scan ("format effector not allowed in a string");
               exit;
            when Invalid =>
               Error_Msg_Scan
                 ("invalid character not allowed, even in a string");
            when Graphic_Character =>
               if Vhdl_Std = Vhdl_87 and then C > Character'Val (127) then
                  Error_8bit;
               end if;
         end case;

         if C = Quotation and Mark = '%' then
            -- LRM93 13.10
            -- The quotation marks (") used as string brackets at both ends of
            -- a string literal can be replaced by percent signs (%), provided
            -- that the enclosed sequence of characters constains no quotation
            -- marks, and provided that both string brackets are replaced.
            Error_Msg_Scan
              ("'""' cannot be used in a string delimited with '%'");
         end if;

         Length := Length + 1;
         Str_Table.Append (C);
         Pos := Pos + 1;
      end loop;

      Str_Table.Finish;

      Current_Token := Tok_String;
      Current_Context.Str_Len := Length;
   end Scan_String;

   -- Scan a bit string literal.
   --
   -- LRM93 13.7
   -- A bit string literal is formed by a sequence of extended digits
   -- (possibly none) enclosed between two quotations used as bit string
   -- brackets, preceded by a base specifier.
   -- BIT_STRING_LITERAL ::= BASE_SPECIFIER " [ BIT_VALUE ] "
   -- BIT_VALUE ::= EXTENDED_DIGIT { [ UNDERLINE ] EXTENDED_DIGIT }
   --
   -- The current character must be a base specifier, followed by '"' or '%'.
   -- The base must be valid.
   procedure Scan_Bit_String
   is
      -- The base specifier.
      Base_Len : Nat32 range 1 .. 4;
      -- The quotation character (can be " or %).
      Mark: Character;
      -- Current character.
      C : Character;
      --  Current length.
      Length : Nat32;
      --  Digit value.
      V : Natural;
   begin
      case Source (Pos) is
         when 'x' | 'X' =>
            Base_Len := 4;
         when 'o' | 'O' =>
            Base_Len := 3;
         when 'b' | 'B' =>
            Base_Len := 1;
         when others =>
            raise Internal_Error;
      end case;
      Pos := Pos + 1;
      Mark := Source (Pos);
      if Mark /= Quotation and then Mark /= '%' then
         raise Internal_Error;
      end if;
      Pos := Pos + 1;
      Length := 0;
      Current_Context.Str_Id := Str_Table.Start;
      loop
         << Again >> null;
         C := Source (Pos);
         Pos := Pos + 1;
         exit when C = Mark;

         -- LRM93 13.7
         -- If the base specifier is 'B', the extended digits in the bit
         -- value are restricted to 0 and 1.
         -- If the base specifier is 'O', the extended digits int the bit
         -- value are restricted to legal digits in the octal number
         -- system, ie, the digits 0 through 7.
         -- If the base specifier is 'X', the extended digits are all digits
         -- together with the letters A through F.
         case C is
            when '0' .. '9' =>
               V := Character'Pos (C) - Character'Pos ('0');
            when 'A' .. 'F' =>
               V := Character'Pos (C) - Character'Pos ('A') + 10;
            when 'a' .. 'f' =>
               V := Character'Pos (C) - Character'Pos ('a') + 10;
            when '_' =>
               if Source (Pos) = '_' then
                  Error_Msg_Scan
                    ("double underscore not allowed in a bit string");
               end if;
               if Source (Pos - 2) = Mark then
                  Error_Msg_Scan
                    ("underscore not allowed at the start of a bit string");
               elsif Source (Pos) = Mark then
                  Error_Msg_Scan
                    ("underscore not allowed at the end of a bit string");
               end if;
               goto Again;
            when others =>
               Error_Msg_Scan
                 ("character '" & C & "' not allowed in a bit string");
               goto Again;
         end case;

         case Base_Len is
            when 1 =>
               if V > 1 then
                  Error_Msg_Scan ("invalid character in a binary bit string");
               end if;
               Str_Table.Append (C);
            when 2 =>
               raise Internal_Error;
            when 3 =>
               if V > 7 then
                  Error_Msg_Scan ("invalid character in a octal bit string");
               end if;
               for I in 1 .. 3 loop
                  if (V / 4) = 1 then
                     Str_Table.Append ('1');
                  else
                     Str_Table.Append ('0');
                  end if;
                  V := (V mod 4) * 2;
               end loop;
            when 4 =>
               for I in 1 .. 4 loop
                  if (V / 8) = 1 then
                     Str_Table.Append ('1');
                  else
                     Str_Table.Append ('0');
                  end if;
                  V := (V mod 8) * 2;
               end loop;
         end case;
         Length := Length + Base_Len;
      end loop;

      Str_Table.Finish;

      if Length = 0 then
         Error_Msg_Scan ("empty bit string is not allowed");
      end if;
      Current_Token := Tok_Bit_String;
      Current_Context.Int64 := Iir_Int64 (Base_Len);
      Current_Context.Str_Len := Length;
   end Scan_Bit_String;

   -- LRM93 13.3.1
   -- Basic Identifiers
   -- A basic identifier consists only of letters, digits, and underlines.
   -- BASIC_IDENTIFIER ::= LETTER { [ UNDERLINE ] LETTER_OR_DIGIT }
   -- LETTER_OR_DIGIT ::= LETTER | DIGIT
   -- LETTER ::= UPPER_CASE_LETTER | LOWER_CASE_LETTER
   --
   -- NB: At the call of this procedure, the current character must be a legal
   -- character for a basic identifier.
   procedure Scan_Identifier
   is
      use Name_Table;
      C : Character;
      Len : Natural;
   begin
      -- This is an identifier or a key word.
      Len := 0;
      loop
         -- source (pos) is correct.
         -- LRM93 13.3.1
         --   All characters if a basic identifier are signifiant, including
         --   any underline character inserted between a letter or digit and
         --   an adjacent letter or digit.
         --   Basic identifiers differing only in the use of the corresponding
         --   upper and lower case letters are considered as the same.
         -- This is achieved by converting all upper case letters into
         -- equivalent lower case letters.
         -- The opposite (converting in lower case letters) is not possible,
         -- because two characters have no upper-case equivalent.
         C := Source (Pos);
         case Characters_Kind (C) is
            when Upper_Case_Letter =>
               if Vhdl_Std = Vhdl_87 and C > 'Z' then
                  Error_8bit;
               end if;
               Len := Len + 1;
               Name_Buffer (Len) := Ada.Characters.Handling.To_Lower (C);
            when Lower_Case_Letter | Digit =>
               if Vhdl_Std = Vhdl_87 and C > 'z' then
                  Error_8bit;
               end if;
               Len := Len + 1;
               Name_Buffer (Len) := C;
            when Special_Character =>
               -- The current character is legal in an identifier.
               if C = '_' then
                  if Source (Pos + 1) = '_' then
                     Error_Msg_Scan ("two underscores can't be consecutive");
                  end if;
                  Len := Len + 1;
                  Name_Buffer (Len) := C;
               else
                  exit;
               end if;
            when others =>
               exit;
         end case;
         Pos := Pos + 1;
      end loop;

      if Source (Pos - 1) = '_' then
         if not Flag_Psl then
            --  Some PSL reserved words finish with '_'.  This case is handled
            --  later.
            Error_Msg_Scan ("identifier cannot finish with '_'");
         end if;
         Pos := Pos - 1;
         Len := Len - 1;
         C := '_';
      end if;

      -- LRM93 13.2
      -- At least one separator is required between an identifier or an
      -- abstract literal and an adjacent identifier or abstract literal.
      case Characters_Kind (C) is
         when Digit
           | Upper_Case_Letter
           | Lower_Case_Letter =>
            raise Internal_Error;
         when Other_Special_Character =>
            if Vhdl_Std /= Vhdl_87 and then C = '\' then
               Error_Separator;
            end if;
         when Invalid
           | Format_Effector
           | Space_Character
           | Special_Character =>
            null;
      end case;
      Name_Length := Len;

      -- Hash it.
      Current_Context.Identifier := Name_Table.Get_Identifier;
      if Current_Identifier in Std_Names.Name_Id_Keywords then
         -- LRM93 13.9
         --   The identifiers listed below are called reserved words and are
         --   reserved for signifiances in the language.
         -- IN: this is also achieved in packages std_names and tokens.
         if Current_Identifier > Std_Names.Name_Last_Vhdl87
           and then Vhdl_Std = Vhdl_87
         then
            if Flags.Warn_Reserved_Word then
               Warning_Msg_Scan
                 ("using """ & Name_Buffer (1 .. Name_Length)
                  & """ vhdl93 reserved word as a vhdl87 identifier");
               Warning_Msg_Scan
                 ("(use option --std=93 to compile as vhdl93)");
            end if;
            Current_Token := Tok_Identifier;
         elsif Current_Identifier > Std_Names.Name_Last_Vhdl93
           and then Vhdl_Std < Vhdl_00
         then
            if Flags.Warn_Reserved_Word then
               Warning_Msg_Scan
                 ("using """ & Name_Buffer (1 .. Name_Length)
                  & """ vhdl00 reserved word as an identifier");
            end if;
            Current_Token := Tok_Identifier;
         else
            Current_Token := Token_Type'Val
              (Token_Type'Pos (Tok_First_Keyword)
               + Current_Identifier - Std_Names.Name_First_Keyword);
         end if;
      elsif Flag_Psl then
         case Current_Identifier is
            when Std_Names.Name_Clock =>
               Current_Token := Tok_Psl_Clock;
            when Std_Names.Name_Const =>
               Current_Token := Tok_Psl_Const;
            when Std_Names.Name_Boolean =>
               Current_Token := Tok_Psl_Boolean;
            when Std_Names.Name_Sequence =>
               Current_Token := Tok_Psl_Sequence;
            when Std_Names.Name_Property =>
               Current_Token := Tok_Psl_Property;
            when Std_Names.Name_Inf =>
               Current_Token := Tok_Inf;
            when Std_Names.Name_Within =>
               Current_Token := Tok_Within;
            when Std_Names.Name_Abort =>
               Current_Token := Tok_Abort;
            when Std_Names.Name_Before =>
               Current_Token := Tok_Before;
            when Std_Names.Name_Always =>
               Current_Token := Tok_Always;
            when Std_Names.Name_Never =>
               Current_Token := Tok_Never;
            when Std_Names.Name_Eventually =>
               Current_Token := Tok_Eventually;
            when Std_Names.Name_Next_A =>
               Current_Token := Tok_Next_A;
            when Std_Names.Name_Next_E =>
               Current_Token := Tok_Next_E;
            when Std_Names.Name_Next_Event =>
               Current_Token := Tok_Next_Event;
            when Std_Names.Name_Next_Event_A =>
               Current_Token := Tok_Next_Event_A;
            when Std_Names.Name_Next_Event_E =>
               Current_Token := Tok_Next_Event_E;
            when Std_Names.Name_Until =>
               Current_Token := Tok_Until;
            when others =>
               Current_Token := Tok_Identifier;
               if C = '_' then
                  Error_Msg_Scan ("identifiers cannot finish with '_'");
               end if;
         end case;
      else
         Current_Token := Tok_Identifier;
      end if;
   end Scan_Identifier;

   --  LRM93 13.3.2
   --  EXTENDED_IDENTIFIER ::= \ GRAPHIC_CHARACTER { GRAPHIC_CHARACTER } \
   --
   -- Create an (extended) indentifier.
   -- Extended identifiers are stored as they appear (leading and tailing
   -- backslashes, doubling backslashes inside).
   procedure Scan_Extended_Identifier
   is
      use Name_Table;
   begin
      -- LRM93 13.3.2
      --   Moreover, every extended identifiers is distinct from any basic
      --   identifier.
      -- This is satisfied by storing '\' in the name table.
      Name_Length := 1;
      Name_Buffer (1) := '\';
      loop
         --  Next character.
         Pos := Pos + 1;

         if Source (Pos) = '\' then
            -- LRM93 13.3.2
            -- If a backslash is to be used as one of the graphic characters
            -- of an extended literal, it must be doubled.
            -- LRM93 13.3.2
            -- (a doubled backslash couting as one character)
            Name_Length := Name_Length + 1;
            Name_Buffer (Name_Length) := '\';

            Pos := Pos + 1;

            exit when Source (Pos) /= '\';
         end if;

         -- source (pos) is correct.
         case Characters_Kind (Source (Pos)) is
            when Format_Effector =>
               Error_Msg_Scan ("format effector in extended identifier");
               exit;
            when Graphic_Character =>
               null;
            when Invalid =>
               Error_Msg_Scan ("invalid character in extended identifier");
         end case;
         Name_Length := Name_Length + 1;
         -- LRM93 13.3.2
         -- Extended identifiers differing only in the use of corresponding
         -- upper and lower case letters are distinct.
         Name_Buffer (Name_Length) := Source (Pos);
      end loop;

      if Name_Length <= 2 then
         Error_Msg_Scan ("empty extended identifier is not allowed");
      end if;

      -- LRM93 13.2
      -- At least one separator is required between an identifier or an
      -- abstract literal and an adjacent identifier or abstract literal.
      case Characters_Kind (Source (Pos)) is
         when Digit
           | Upper_Case_Letter
           | Lower_Case_Letter =>
            Error_Separator;
         when Invalid
           | Format_Effector
           | Space_Character
           | Special_Character
           | Other_Special_Character =>
            null;
      end case;

      -- Hash it.
      Current_Context.Identifier := Name_Table.Get_Identifier;
      Current_Token := Tok_Identifier;
   end Scan_Extended_Identifier;

   procedure Convert_Identifier
   is
      procedure Error_Bad is
      begin
         Error_Msg_Option ("bad character in identifier");
      end Error_Bad;

      procedure Error_8bit is
      begin
         Error_Msg_Option ("8 bits characters not allowed in vhdl87");
      end Error_8bit;

      use Name_Table;
      C : Character;
   begin
      if Name_Length = 0 then
         Error_Msg_Option ("identifier required");
         return;
      end if;

      if Name_Buffer (1) = '\' then
         --  Extended identifier.
         if Vhdl_Std = Vhdl_87 then
            Error_Msg_Option ("extended identifiers not allowed in vhdl87");
            return;
         end if;

         if Name_Length < 3 then
            Error_Msg_Option ("extended identifier is too short");
            return;
         end if;
         if Name_Buffer (Name_Length) /= '\' then
            Error_Msg_Option ("extended identifier must finish with a '\'");
            return;
         end if;
         for I in 2 .. Name_Length - 1 loop
            C := Name_Buffer (I);
            case Characters_Kind (C) is
               when Format_Effector =>
                  Error_Msg_Option ("format effector in extended identifier");
                  return;
               when Graphic_Character =>
                  if C = '\' then
                     if Name_Buffer (I + 1) /= '\'
                       or else I = Name_Length - 1
                     then
                        Error_Msg_Option ("anti-slash must be doubled "
                                          & "in extended identifier");
                        return;
                     end if;
                  end if;
               when Invalid =>
                  Error_Bad;
            end case;
         end loop;
      else
         --  Identifier
         for I in 1 .. Name_Length loop
            C := Name_Buffer (I);
            case Characters_Kind (C) is
               when Upper_Case_Letter =>
                  if Vhdl_Std = Vhdl_87 and C > 'Z' then
                     Error_8bit;
                  end if;
                  Name_Buffer (I) := Ada.Characters.Handling.To_Lower (C);
               when Lower_Case_Letter | Digit =>
                  if Vhdl_Std = Vhdl_87 and C > 'z' then
                     Error_8bit;
                  end if;
               when Special_Character =>
                  -- The current character is legal in an identifier.
                  if C = '_' then
                     if I = 1 then
                        Error_Msg_Option
                          ("identifier cannot start with an underscore");
                        return;
                     end if;
                     if Name_Buffer (I - 1) = '_' then
                        Error_Msg_Option
                          ("two underscores can't be consecutive");
                        return;
                     end if;
                     if I = Name_Length then
                        Error_Msg_Option
                          ("identifier cannot finish with an underscore");
                        return;
                     end if;
                  else
                     Error_Bad;
                  end if;
               when others =>
                  Error_Bad;
            end case;
         end loop;
      end if;
   end Convert_Identifier;

   --  Scan an identifier within a comment.  Only lower case letters are
   --  allowed.
   function Scan_Comment_Identifier return Boolean
   is
      use Name_Table;
      Len : Natural;
      C : Character;
   begin
      --  Skip spaces.
      while Source (Pos) = ' ' or Source (Pos) = HT loop
         Pos := Pos + 1;
      end loop;

      --  The identifier shall start with a lower case letter.
      if Source (Pos) not in 'a' .. 'z' then
         return False;
      end if;

      --  Scan the identifier (in lower cases).
      Len := 0;
      loop
         C := Source (Pos);
         exit when C not in 'a' .. 'z' and C /= '_';
         Len := Len + 1;
         Name_Buffer (Len) := C;
         Pos := Pos + 1;
      end loop;

      --  Shall be followed by a space or a new line.
      case C is
         when ' ' | HT | LF | CR =>
            null;
         when others =>
            return False;
      end case;

      Name_Length := Len;
      return True;
   end Scan_Comment_Identifier;

   function Scan_Comment return Boolean
   is
      use Std_Names;
      Id : Name_Id;
   begin
      if not Scan_Comment_Identifier then
         return False;
      end if;

      -- Hash it.
      Id := Name_Table.Get_Identifier;

      case Id is
         when Name_Psl =>
            if not Scan_Comment_Identifier then
               return False;
            end if;
            case Name_Table.Get_Identifier is
               when Name_Property =>
                  Current_Token := Tok_Psl_Property;
               when Name_Sequence =>
                  Current_Token := Tok_Psl_Sequence;
               when Name_Endpoint =>
                  Current_Token := Tok_Psl_Endpoint;
               when Name_Assert =>
                  Current_Token := Tok_Psl_Assert;
               when Name_Default =>
                  Current_Token := Tok_Psl_Default;
               when others =>
                  return False;
            end case;
            Flag_Scan_In_Comment := True;
            return True;
         when others =>
            return False;
      end case;
   end Scan_Comment;

   function Scan_Exclam_Mark return Boolean is
   begin
      if Source (Pos) = '!' then
         Pos := Pos + 1;
         return True;
      else
         return False;
      end if;
   end Scan_Exclam_Mark;

   function Scan_Underscore return Boolean is
   begin
      if Source (Pos) = '_' then
         Pos := Pos + 1;
         return True;
      else
         return False;
      end if;
   end Scan_Underscore;

   -- Get a new token.
   procedure Scan is
   begin
      if Current_Token /= Tok_Invalid then
         Current_Context.Prev_Token := Current_Token;
      end if;

      << Again >> null;

      -- Skip commonly used separators.
      while Source(Pos) = ' ' or Source(Pos) = HT loop
         Pos := Pos + 1;
      end loop;

      Current_Context.Token_Pos := Pos;
      Current_Context.Identifier := Null_Identifier;

      case Source (Pos) is
         when HT | ' ' =>
            --  Must have already been skipped just above.
            raise Internal_Error;
         when NBSP =>
            if Vhdl_Std = Vhdl_87 then
               Error_Msg_Scan ("NBSP character not allowed in vhdl87");
            end if;
            Pos := Pos + 1;
            goto Again;
         when VT | FF =>
            Pos := Pos + 1;
            goto Again;
         when LF | CR =>
            -- Accept CR, LF, CR+LF or LF+CR as line separator.
            if (Source (Pos) = LF and then Source (Pos + 1) = CR)
              or else (Source (Pos) = CR and then Source (Pos + 1) = LF)
            then
               Pos := Pos + 2;
            else
               Pos := Pos + 1;
            end if;
            Current_Context.Line_Number := Current_Context.Line_Number + 1;
            Current_Context.Line_Pos := Pos;
            File_Add_Line_Number
              (Current_Context.Source_File, Current_Context.Line_Number, Pos);
            if Flag_Newline then
               Current_Token := Tok_Newline;
               return;
            end if;
            goto Again;
         when '-' =>
            if Source (Pos + 1) = '-' then
               -- This is a comment.
               -- LRM93 13.8
               --   A comment starts with two adjacent hyphens and extends up
               --   to the end of the line.
               --   A comment can appear on any line line of a VHDL
               --   description.
               --   The presence or absence of comments has no influence on
               --   wether a description is legal or illegal.
               --   Futhermore, comments do not influence the execution of a
               --   simulation module; their sole purpose is the enlightenment
               --   of the human reader.
               -- GHDL note: As a consequence, an obfruscating comment
               --  is out of purpose, and a warning could be reported :-)
               Pos := Pos + 2;

               --  Scan inside a comment.  So we just ignore the two dashes.
               if Flag_Scan_In_Comment then
                  goto Again;
               end if;

               --  Handle keywords in comment (PSL).
               if Flag_Comment_Keyword
                 and then Scan_Comment
               then
                  return;
               end if;

               --  LRM93 13.2
               --  In any case, a sequence of one or more format
               --  effectors other than horizontal tabulation must
               --  cause at least one end of line.
               while Source (Pos) /= CR and Source (Pos) /= LF and
                 Source (Pos) /= VT and Source (Pos) /= FF and
                 Source (Pos) /= Files_Map.EOT
               loop
                  if not Flags.Mb_Comment
                    and then Characters_Kind (Source (Pos)) = Invalid
                  then
                     Error_Msg_Scan ("invalid character, even in a comment");
                  end if;
                  Pos := Pos + 1;
               end loop;
               if Flag_Comment then
                  Current_Token := Tok_Comment;
                  return;
               end if;
               goto Again;
            elsif Flag_Psl and then Source (Pos + 1) = '>' then
               Current_Token := Tok_Minus_Greater;
               Pos := Pos + 2;
               return;
            else
               Current_Token := Tok_Minus;
               Pos := Pos + 1;
               return;
            end if;
         when '+' =>
            Current_Token := Tok_Plus;
            Pos := Pos + 1;
            return;
         when '*' =>
            if Source (Pos + 1) = '*' then
               Current_Token := Tok_Double_Star;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Star;
               Pos := Pos + 1;
            end if;
            return;
         when '/' =>
            if Source (Pos + 1) = '=' then
               Current_Token := Tok_Not_Equal;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Slash;
               Pos := Pos + 1;
            end if;
            return;
         when '(' =>
            Current_Token := Tok_Left_Paren;
            Pos := Pos + 1;
            return;
         when ')' =>
            Current_Token := Tok_Right_Paren;
            Pos := Pos + 1;
            return;
         when '|' =>
            if Flag_Psl then
               if Source (Pos + 1) = '|' then
                  Current_Token := Tok_Bar_Bar;
                  Pos := Pos + 2;
               elsif Source (Pos + 1) = '-'
                 and then Source (Pos + 2) = '>'
               then
                  Current_Token := Tok_Bar_Arrow;
                  Pos := Pos + 3;
               elsif Source (Pos + 1) = '='
                 and then Source (Pos + 2) = '>'
               then
                  Current_Token := Tok_Bar_Double_Arrow;
                  Pos := Pos + 3;
               else
                  Current_Token := Tok_Bar;
                  Pos := Pos + 1;
               end if;
            else
               Current_Token := Tok_Bar;
               Pos := Pos + 1;
            end if;
            return;
         when '!' =>
            if Flag_Psl then
               Current_Token := Tok_Exclam_Mark;
            else
               --  LRM93 13.10
               --  A vertical line (|) can be replaced by an exclamation
               --  mark (!)  where used as a delimiter.
               Current_Token := Tok_Bar;
            end if;
            Pos := Pos + 1;
            return;
         when ':' =>
            if Source (Pos + 1) = '=' then
               Current_Token := Tok_Assign;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Colon;
               Pos := Pos + 1;
            end if;
            return;
         when ';' =>
            Current_Token := Tok_Semi_Colon;
            Pos := Pos + 1;
            return;
         when ',' =>
            Current_Token := Tok_Comma;
            Pos := Pos + 1;
            return;
         when '.' =>
            if Source (Pos + 1) = '.' then
               --  Be Ada friendly...
               Error_Msg_Scan ("'..' is invalid in vhdl, replaced by 'to'");
               Current_Token := Tok_To;
               Pos := Pos + 2;
               return;
            end if;
            Current_Token := Tok_Dot;
            Pos := Pos + 1;
            return;
         when '&' =>
            if Flag_Psl and then Source (Pos + 1) = '&' then
               Current_Token := Tok_And_And;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Ampersand;
               Pos := Pos + 1;
            end if;
            return;
         when '<' =>
            if Source (Pos + 1) = '=' then
               Current_Token := Tok_Less_Equal;
               Pos := Pos + 2;
            elsif Source (Pos + 1) = '>' then
               Current_Token := Tok_Box;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Less;
               Pos := Pos + 1;
            end if;
            return;
         when '>' =>
            if Source (Pos + 1) = '=' then
               Current_Token := Tok_Greater_Equal;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Greater;
               Pos := Pos + 1;
            end if;
            return;
         when '=' =>
            if Source (Pos + 1) = '>' then
               Current_Token := Tok_Double_Arrow;
               Pos := Pos + 2;
            else
               Current_Token := Tok_Equal;
               Pos := Pos + 1;
            end if;
            return;
         when ''' =>
            -- Handle cases such as character'('a')
            -- FIXME: what about f ()'length ? or .all'length
            if Current_Context.Prev_Token /= Tok_Identifier
              and then Current_Context.Prev_Token /= Tok_Character
              and then Source (Pos + 2) = '''
            then
               -- LRM93 13.5
               -- A character literal is formed by enclosing one of the 191
               -- graphic character (...) between two apostrophe characters.
               -- CHARACTER_LITERAL ::= ' GRAPHIC_CHARACTER '
               if Characters_Kind (Source (Pos + 1)) not in Graphic_Character
               then
                  Error_Msg_Scan
                    ("a character literal can only be a graphic character");
               elsif Vhdl_Std = Vhdl_87
                 and then Source (Pos + 1) > Character'Val (127)
               then
                  Error_8bit;
               end if;
               Current_Token := Tok_Character;
               Current_Context.Identifier :=
                 Name_Table.Get_Identifier (Source (Pos + 1));
               Pos := Pos + 3;
               return;
            else
               Current_Token := Tok_Tick;
               Pos := Pos + 1;
            end if;
            return;
         when '0' .. '9' =>
            Scan_Literal;

            --  LRM 13.2
            --  At least one separator is required between an identifier or
            --  an abstract literal and an adjacent identifier or abstract
            --  literal.
            case Characters_Kind (Source (Pos)) is
               when Digit =>
                  raise Internal_Error;
               when Upper_Case_Letter
                 | Lower_Case_Letter =>
                  --  Could call Error_Separator, but use a clearer message
                  --  for this common case.
                  --  Note: the term "unit name" is not correct here, since it
                  --  can be any identifier or even a keyword; however it is
                  --  probably the most common case (eg 10ns).
                  Error_Msg_Scan
                    ("space is required between number and unit name");
               when Other_Special_Character =>
                  if Vhdl_Std /= Vhdl_87 and then Source (Pos) = '\' then
                     Error_Separator;
                  end if;
               when Invalid
                 | Format_Effector
                 | Space_Character
                 | Special_Character =>
                  null;
            end case;
            return;
         when '#' =>
            Error_Msg_Scan ("'#' is used for based literals and "
                            & "must be preceded by a base");
            -- Cannot easily continue.
            raise Compilation_Error;
         when Quotation | '%' =>
            Scan_String;
            return;
         when '[' =>
            if Flag_Psl then
               if Source (Pos + 1) = '*' then
                  Current_Token := Tok_Brack_Star;
                  Pos := Pos + 2;
               elsif Source (Pos + 1) = '+'
                 and then Source (Pos + 2) = ']'
               then
                  Current_Token := Tok_Brack_Plus_Brack;
                  Pos := Pos + 3;
               elsif Source (Pos + 1) = '-'
                 and then Source (Pos + 2) = '>'
               then
                  Current_Token := Tok_Brack_Arrow;
                  Pos := Pos + 3;
               elsif Source (Pos + 1) = '=' then
                  Current_Token := Tok_Brack_Equal;
                  Pos := Pos + 2;
               else
                  Current_Token := Tok_Left_Bracket;
                  Pos := Pos + 1;
               end if;
            else
               if Vhdl_Std = Vhdl_87 then
                  Error_Msg_Scan
                    ("'[' is an invalid character in vhdl87, replaced by '('");
                  Current_Token := Tok_Left_Paren;
               else
                  Current_Token := Tok_Left_Bracket;
               end if;
               Pos := Pos + 1;
            end if;
            return;
         when ']' =>
            if Vhdl_Std = Vhdl_87 and not Flag_Psl then
               Error_Msg_Scan
                 ("']' is an invalid character in vhdl87, replaced by ')'");
               Current_Token := Tok_Right_Paren;
            else
               Current_Token := Tok_Right_Bracket;
            end if;
            Pos := Pos + 1;
            return;
         when '{' =>
            if Flag_Psl then
               Current_Token := Tok_Left_Curly;
            else
               Error_Msg_Scan ("'{' is an invalid character, replaced by '('");
               Current_Token := Tok_Left_Paren;
            end if;
            Pos := Pos + 1;
            return;
         when '}' =>
            if Flag_Psl then
               Current_Token := Tok_Right_Curly;
            else
               Error_Msg_Scan ("'}' is an invalid character, replaced by ')'");
               Current_Token := Tok_Right_Paren;
            end if;
            Pos := Pos + 1;
            return;
         when '\' =>
            if Vhdl_Std = Vhdl_87 then
               Error_Msg_Scan
                 ("extended identifiers are not allowed in vhdl87");
            end if;
            Scan_Extended_Identifier;
            return;
         when '^' =>
            Error_Msg_Scan ("'^' is not a VHDL operator, use 'xor'");
            Pos := Pos + 1;
            Current_Token := Tok_Xor;
            return;
         when '~' =>
            Error_Msg_Scan ("'~' is not a VHDL operator, use 'not'");
            Pos := Pos + 1;
            Current_Token := Tok_Not;
            return;
         when '$' | '?' | '`'
           | Inverted_Exclamation .. Inverted_Question
           | Multiplication_Sign | Division_Sign =>
            Error_Msg_Scan ("character """ & Source (Pos)
                            & """ can only be used in strings or comments");
            Pos := Pos + 1;
            goto Again;
         when '@' =>
            if Flag_Psl then
               Current_Token := Tok_Arobase;
               Pos := Pos + 1;
               return;
            else
               Error_Msg_Scan
                 ("character """ & Source (Pos)
                    & """ can only be used in strings or comments");
               Pos := Pos + 1;
               goto Again;
            end if;
         when '_' =>
            Error_Msg_Scan ("an identifier can't start with '_'");
            Pos := Pos + 1;
            goto Again;
         when 'B' | 'b' | 'O' | 'o' | 'X' | 'x' =>
            if Source (Pos + 1) = Quotation or else Source (Pos + 1) = '%' then
               -- LRM93 13.7
               -- BASE_SPECIFIER ::= B | O | X
               -- A letter in a bit string literal (either an extended digit or
               -- the base specifier) can be written either in lower case or
               -- in upper case, with the same meaning.
               Scan_Bit_String;
            else
               Scan_Identifier;
            end if;
            return;
         when 'A' | 'C' .. 'N' | 'P' .. 'W' | 'Y'| 'Z'
           | 'a' | 'c' .. 'n' | 'p' .. 'w' | 'y'| 'z' =>
            Scan_Identifier;
            return;
         when UC_A_Grave .. UC_O_Diaeresis
           | UC_O_Oblique_Stroke .. UC_Icelandic_Thorn =>
            if Vhdl_Std = Vhdl_87 then
               Error_Msg_Scan
                 ("upper case letters above 128 are not allowed in vhdl87");
            end if;
            Scan_Identifier;
            return;
         when LC_German_Sharp_S .. LC_O_Diaeresis
           | LC_O_Oblique_Stroke .. LC_Y_Diaeresis =>
            if Vhdl_Std = Vhdl_87 then
               Error_Msg_Scan
                 ("lower case letters above 128 are not allowed in vhdl87");
            end if;
            Scan_Identifier;
            return;
         when NUL .. ETX | ENQ .. BS | SO .. US | DEL .. APC =>
            Error_Msg_Scan
              ("control character that is not CR, LF, FF, HT or VT " &
               "is not allowed");
            Pos := Pos + 1;
            goto Again;
         when Files_Map.EOT =>
            if Pos >= Current_Context.File_Len then
               --  FIXME: should conditionnaly emit a warning if the file
               --   is not terminated by an end of line.
               Current_Token := Tok_Eof;
            else
               Error_Msg_Scan ("EOT is not allowed inside the file");
               Pos := Pos + 1;
               goto Again;
            end if;
            return;
      end case;
   end Scan;

   function Get_Token_Location return Location_Type is
   begin
      return File_Pos_To_Location
        (Current_Context.Source_File, Current_Context.Token_Pos);
   end Get_Token_Location;
end Scan;
