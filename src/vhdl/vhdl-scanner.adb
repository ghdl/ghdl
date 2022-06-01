--  VHDL lexical scanner.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Errorout; use Errorout;
with Name_Table;
with Files_Map; use Files_Map;
with Std_Names;
with Str_Table;
with Flags; use Flags;

package body Vhdl.Scanner is

   -- This classification is a simplification of the categories of LRM93 13.1
   -- LRM93 13.1
   -- The only characters allowed in the text of a VHDL description are the
   -- graphic characters and format effector.

   type Character_Kind_Type is
      (
       -- Neither a format effector nor a graphic character.
       Invalid,
       Format_Effector,
       Lower_Case_Letter,
       Upper_Case_Letter,
       Digit,
       Special_Character,
       Space_Character,
       Other_Special_Character
      );

   --  LRM93 13.1
   --  basic_graphic_character ::=
   --    upper_case_letter | digit | special_character | space_character
   --
   --subtype Basic_Graphic_Character is
   --  Character_Kind_Type range Upper_Case_Letter .. Space_Character;

   --  LRM93 13.1
   --  graphic_character ::=
   --    basic_graphic_character | lower_case_letter | other_special_character
   --
   --  Note: There are 191 graphic characters.
   subtype Graphic_Character is
     Character_Kind_Type range Lower_Case_Letter .. Other_Special_Character;

   --  letter ::= upper_case_letter | lower_case_letter
   subtype Letter is
     Character_Kind_Type range Lower_Case_Letter .. Upper_Case_Letter;

   -- LRM93 13.1
   -- The characters included in each of the categories of basic graphic
   -- characters are defined as follows:
   type Character_Array is array (Character) of Character_Kind_Type;
   pragma Suppress_Initialization (Character_Array);
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
      '"' | '#' | '&' | ''' | '(' | ')' | '+' | ',' | '-' | '.' | '/'
        | ':' | ';' | '<' | '=' | '>' | '[' | ']'
        | '_' | '|' | '*' => Special_Character,

      -- 4. the space characters
      ' ' | NBSP => Space_Character,

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
      Source : File_Buffer_Acc;
      Source_File : Source_File_Entry;
      Line_Number : Natural;
      Line_Pos : Source_Ptr;
      Prev_Pos : Source_Ptr;
      Token_Pos : Source_Ptr;
      Pos : Source_Ptr;
      File_Len : Source_Ptr;
      Token : Token_Type;
      Prev_Token : Token_Type;

      --  Tokens are ignored because of 'translate_off'.
      Translate_Off : Boolean;

      --  Additional values for the current token.
      Bit_Str_Base : Character;
      Bit_Str_Sign : Character;
      Str_Id : String8_Id;
      Str_Len : Nat32;
      Identifier: Name_Id;
      Lit_Int64 : Int64;
      Lit_Fp64 : Fp64;
   end record;
   pragma Suppress_Initialization (Scan_Context);

   -- The current context.
   -- Default value is an invalid context.
   Current_Context: Scan_Context := (Source => null,
                                     Source_File => No_Source_File_Entry,
                                     Line_Number => 0,
                                     Line_Pos => 0,
                                     Pos => 0,
                                     Prev_Pos => 0,
                                     Token_Pos => 0,
                                     File_Len => 0,
                                     Token => Tok_Invalid,
                                     Prev_Token => Tok_Invalid,
                                     Translate_Off => False,
                                     Identifier => Null_Identifier,
                                     Bit_Str_Base => ' ',
                                     Bit_Str_Sign => ' ',
                                     Str_Id => Null_String8,
                                     Str_Len => 0,
                                     Lit_Int64 => 0,
                                     Lit_Fp64 => 0.0);

   function Get_Current_Coord return Source_Coord_Type is
   begin
      return (File => Get_Current_Source_File,
              Line_Pos => Current_Context.Line_Pos,
              Line => Get_Current_Line,
              Offset => Get_Current_Offset);
   end Get_Current_Coord;

   function Get_Token_Coord return Source_Coord_Type is
   begin
      return (File => Get_Current_Source_File,
              Line_Pos => Current_Context.Line_Pos,
              Line => Get_Current_Line,
              Offset => Get_Token_Offset);
   end Get_Token_Coord;

   -- Disp a message during scan.
   -- The current location is automatically displayed before the message.
   -- Disp a message during scan.
   procedure Error_Msg_Scan (Msg: String) is
   begin
      Report_Msg (Msgid_Error, Scan, Get_Current_Coord, Msg);
   end Error_Msg_Scan;

   procedure Error_Msg_Scan (Loc : Source_Coord_Type; Msg: String) is
   begin
      Report_Msg (Msgid_Error, Scan, Loc, Msg);
   end Error_Msg_Scan;

   procedure Error_Msg_Scan (Msg: String; Arg1 : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Scan, Get_Current_Coord, Msg, (1 => Arg1));
   end Error_Msg_Scan;

   -- Disp a message during scan.
   procedure Warning_Msg_Scan (Id : Msgid_Warnings;
                               Msg: String;
                               Arg1 : Earg_Type) is
   begin
      Report_Msg (Id, Scan, Get_Current_Coord, Msg, (1 => Arg1));
   end Warning_Msg_Scan;

   procedure Warning_Msg_Scan (Id : Msgid_Warnings;
                               Msg: String;
                               Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Id, Scan, Get_Current_Coord, Msg, Args);
   end Warning_Msg_Scan;

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

   function Current_String_Id return String8_Id is
   begin
      return Current_Context.Str_Id;
   end Current_String_Id;

   function Current_String_Length return Nat32 is
   begin
      return Current_Context.Str_Len;
   end Current_String_Length;

   function Get_Bit_String_Base return Character is
   begin
      return Current_Context.Bit_Str_Base;
   end Get_Bit_String_Base;

   function Get_Bit_String_Sign return Character is
   begin
      return Current_Context.Bit_Str_Sign;
   end Get_Bit_String_Sign;

   function Current_Iir_Int64 return Int64 is
   begin
      return Current_Context.Lit_Int64;
   end Current_Iir_Int64;

   function Current_Iir_Fp64 return Fp64 is
   begin
      return Current_Context.Lit_Fp64;
   end Current_Iir_Fp64;

   function Get_Current_Source_File return Source_File_Entry is
   begin
      return Current_Context.Source_File;
   end Get_Current_Source_File;

   function Get_Current_Line return Natural is
   begin
      return Current_Context.Line_Number;
   end Get_Current_Line;

   function Get_Current_Offset return Natural is
   begin
      return Natural (Current_Context.Pos - Current_Context.Line_Pos);
   end Get_Current_Offset;

   function Get_Token_Offset return Natural is
   begin
      return Natural (Current_Context.Token_Pos - Current_Context.Line_Pos);
   end Get_Token_Offset;

   function Get_Token_Position return Source_Ptr is
   begin
      return Current_Context.Token_Pos;
   end Get_Token_Position;

   function Get_Token_Length return Int32 is
   begin
      return Int32 (Current_Context.Pos - Current_Context.Token_Pos);
   end Get_Token_Length;

   function Get_Position return Source_Ptr is
   begin
      return Current_Context.Pos;
   end Get_Position;

   function Get_Token_Location return Location_Type is
   begin
      return File_Pos_To_Location
        (Current_Context.Source_File, Current_Context.Token_Pos);
   end Get_Token_Location;

   function Get_Prev_Location return Location_Type is
   begin
      return File_Pos_To_Location
        (Current_Context.Source_File, Current_Context.Prev_Pos);
   end Get_Prev_Location;

   procedure Set_File (Source_File : Source_File_Entry)
   is
      N_Source: File_Buffer_Acc;
   begin
      pragma Assert (Current_Context.Source = null);
      pragma Assert (Source_File /= No_Source_File_Entry);
      N_Source := Get_File_Source (Source_File);
      Current_Context := (Source => N_Source,
                          Source_File => Source_File,
                          Line_Number => 1,
                          Line_Pos => 0,
                          Prev_Pos => N_Source'First,
                          Pos => N_Source'First,
                          Token_Pos => 0, -- should be invalid,
                          File_Len => Get_File_Length (Source_File),
                          Token => Tok_Invalid,
                          Prev_Token => Tok_Invalid,
                          Translate_Off => False,
                          Identifier => Null_Identifier,
                          Bit_Str_Base => ' ',
                          Bit_Str_Sign => ' ',
                          Str_Id => Null_String8,
                          Str_Len => 0,
                          Lit_Int64 => -1,
                          Lit_Fp64 => 0.0);
      Current_Token := Tok_Invalid;
   end Set_File;

   function Detect_Encoding_Errors return Boolean
   is
      C : constant Character := Source (Pos);
   begin
      --  No need to check further if first character is plain ASCII-7
      if C >= ' ' and C < Character'Val (127) then
         return False;
      end if;

      --  UTF-8 BOM is EF BB BF
      if Source (Pos + 0) = Character'Val (16#ef#)
        and then Source (Pos + 1) = Character'Val (16#bb#)
        and then Source (Pos + 2) = Character'Val (16#bf#)
      then
         Error_Msg_Scan
           ("source encoding must be latin-1 (UTF-8 BOM detected)");
         return True;
      end if;

      --  UTF-16 BE BOM is FE FF
      if Source (Pos + 0) = Character'Val (16#fe#)
        and then Source (Pos + 1) = Character'Val (16#ff#)
      then
         Error_Msg_Scan
           ("source encoding must be latin-1 (UTF-16 BE BOM detected)");
         return True;
      end if;

      --  UTF-16 LE BOM is FF FE
      if Source (Pos + 0) = Character'Val (16#ff#)
        and then Source (Pos + 1) = Character'Val (16#fe#)
      then
         Error_Msg_Scan
           ("source encoding must be latin-1 (UTF-16 LE BOM detected)");
         return True;
      end if;

      --  Certainly weird, but scanner/parser will catch it.
      return False;
   end Detect_Encoding_Errors;

   procedure Set_Current_Position (Position: Source_Ptr)
   is
      Loc : Location_Type;
      Offset: Natural;
      File_Entry : Source_File_Entry;
   begin
      --  Scanner must have been initialized.
      pragma Assert (Current_Context.Source /= null);

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

   --  Scan a string literal.
   --
   --  LRM93 13.6 / LRM08 15.7
   --  A string literal is formed by a sequence of graphic characters
   --  (possibly none) enclosed between two quotation marks used as string
   --  brackets.
   --  STRING_LITERAL ::= " { GRAPHIC_CHARACTER } "
   --
   --  IN: for a string, at the call of this procedure, the current character
   --  must be either '"' or '%'.
   procedure Scan_String
   is
      -- The quotation character (can be " or %).
      Mark: Character;
      -- Current character.
      C : Character;
      --  Current length.
      Length : Nat32;
   begin
      --  String delimiter.
      Mark := Source (Pos);
      pragma Assert (Mark = '"' or else Mark = '%');

      Pos := Pos + 1;
      Length := 0;
      Current_Context.Str_Id := Str_Table.Create_String8;
      loop
         C := Source (Pos);
         if C = Mark then
            --  LRM93 13.6
            --  If a quotation mark value is to be represented in the sequence
            --  of character values, then a pair of adjacent quoatation
            --  characters marks must be written at the corresponding place
            --  within the string literal.
            --  LRM93 13.10
            --  Any pourcent sign within the sequence of characters must then
            --  be doubled, and each such doubled percent sign is interpreted
            --  as a single percent sign value.
            --  The same replacement is allowed for a bit string literal,
            --  provieded that both bit string brackets are replaced.
            Pos := Pos + 1;
            exit when Source (Pos) /= Mark;
         end if;

         case Characters_Kind (C) is
            when Format_Effector =>
               if Mark = '%' then
                  --  No matching '%' has been found.  Consider '%' was used
                  --  as the remainder operator, instead of 'rem'.  This will
                  --  improve the error message.
                  Error_Msg_Scan
                    (+Get_Token_Location,
                     "'%%' is not a vhdl operator, use 'rem'");
                  Current_Token := Tok_Rem;
                  Pos := Current_Context.Token_Pos + 1;
                  return;
               end if;
               if C = CR or C = LF then
                  Error_Msg_Scan
                    ("string cannot be multi-line, use concatenation");
               else
                  Error_Msg_Scan ("format effector not allowed in a string");
               end if;
               exit;
            when Invalid =>
               if C = Files_Map.EOT
                 and then Pos >= Current_Context.File_Len
               then
                  Error_Msg_Scan ("string not terminated at end of file");
                  exit;
               end if;

               Error_Msg_Scan
                 ("invalid character not allowed, even in a string");
            when Graphic_Character =>
               if Vhdl_Std = Vhdl_87 and then C > Character'Val (127) then
                  Error_8bit;
               end if;
         end case;

         if C = '"' and Mark = '%' then
            --  LRM93 13.10
            --  The quotation marks (") used as string brackets at both ends of
            --  a string literal can be replaced by percent signs (%), provided
            --  that the enclosed sequence of characters constains no quotation
            --  marks, and provided that both string brackets are replaced.
            Error_Msg_Scan
              ("'""' cannot be used in a string delimited with '%%'");
         end if;

         Length := Length + 1;
         Str_Table.Append_String8 (Character'Pos (C));
         Pos := Pos + 1;
      end loop;

      Current_Token := Tok_String;
      Current_Context.Str_Len := Length;
   end Scan_String;

   --  Scan a bit string literal.
   --
   --  LRM93 13.7
   --  A bit string literal is formed by a sequence of extended digits
   --  (possibly none) enclosed between two quotations used as bit string
   --  brackets, preceded by a base specifier.
   --  BIT_STRING_LITERAL ::= BASE_SPECIFIER " [ BIT_VALUE ] "
   --  BIT_VALUE ::= EXTENDED_DIGIT { [ UNDERLINE ] EXTENDED_DIGIT }
   --
   --  The current character must be a base specifier, followed by '"' or '%'.
   --  The base must be valid.
   procedure Scan_Bit_String (Base_Log : Nat32)
   is
      --  Position of character '0'.
      Pos_0 : constant Nat8 := Character'Pos ('0');

      --  Used for the base.
      subtype Nat4 is Natural range 1 .. 4;
      Base : constant Nat32 := 2 ** Nat4 (Base_Log);

      -- The quotation character (can be " or %).
      Orig_Pos : constant Source_Ptr := Pos;
      Mark     : constant Character := Source (Orig_Pos);
      -- Current character.
      C : Character;
      --  Current length.
      Length : Nat32;
      --  Digit value.
      V, D : Nat8;
      --  True if invalid character already found, to avoid duplicate message.
      Has_Invalid : Boolean;
   begin
      pragma Assert (Mark = '"' or else Mark = '%');
      Pos := Pos + 1;
      Length := 0;
      Has_Invalid := False;
      Current_Context.Str_Id := Str_Table.Create_String8;
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
               --  LRM93 13.7
               --  A letter in a bit string literal (...) can be written either
               --  in lowercase or in upper case, with the same meaning.
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
            when '"' =>
               pragma Assert (Mark = '%');
               Error_Msg_Scan
                 ("'""' cannot close a bit string opened by '%%'");
               exit;
            when '%' =>
               pragma Assert (Mark = '"');
               Error_Msg_Scan
                 ("'%%' cannot close a bit string opened by '""'");
               exit;
            when others =>
               if Characters_Kind (C) in Graphic_Character then
                  if Vhdl_Std >= Vhdl_08 then
                     V := Nat8'Last;
                  else
                     if not Has_Invalid then
                        Error_Msg_Scan ("invalid character in bit string");
                        Has_Invalid := True;
                     end if;
                     --  Continue the bit string
                     V := 0;
                  end if;
               else
                  if Mark = '%' then
                     Error_Msg_Scan
                       (+File_Pos_To_Location
                          (Current_Context.Source_File, Orig_Pos),
                        "'%%' is not a vhdl operator, use 'rem'");
                     Current_Token := Tok_Rem;
                     Pos := Orig_Pos + 1;
                     return;
                  else
                     Error_Msg_Scan ("bit string not terminated");
                     Pos := Pos - 1;
                  end if;
                  exit;
               end if;
         end case;

         --  Expand bit value.
         if Vhdl_Std >= Vhdl_08 and V > Base then
            --  Expand as graphic character.
            for I in 1 .. Base_Log loop
               Str_Table.Append_String8_Char (C);
            end loop;
         else
            --  Expand as extended digits.
            case Base_Log is
               when 1 =>
                  if V > 1 then
                     Error_Msg_Scan
                       ("invalid character in a binary bit string");
                     V := 1;
                  end if;
                  Str_Table.Append_String8 (Pos_0 + V);
               when 3 =>
                  if V > 7 then
                     Error_Msg_Scan
                       ("invalid character in a octal bit string");
                     V := 7;
                  end if;
                  for I in 1 .. 3 loop
                     D := V / 4;
                     Str_Table.Append_String8 (Pos_0 + D);
                     V := (V - 4 * D) * 2;
                  end loop;
               when 4 =>
                  for I in 1 .. 4 loop
                     D := V / 8;
                     Str_Table.Append_String8 (Pos_0 + D);
                     V := (V - 8 * D) * 2;
                  end loop;
               when others =>
                  raise Internal_Error;
            end case;
         end if;

         Length := Length + Base_Log;
      end loop;

      --  Note: the length of the bit string may be 0.

      Current_Token := Tok_Bit_String;
      Current_Context.Str_Len := Length;
   end Scan_Bit_String;

   --  Scan a decimal bit string literal.  For base specifier D the algorithm
   --  is rather different: all the graphic characters shall be digits, and we
   --  need to use a (not very efficient) arbitrary precision multiplication.
   procedure Scan_Dec_Bit_String
   is
      use Str_Table;

      Id : String8_Id;

      --  Position of character '0'.
      Pos_0 : constant Nat8 := Character'Pos ('0');

      -- Current character.
      C : Character;
      --  Current length.
      Length : Nat32;
      --  Digit value.
      V, D : Nat8;

      type Carries_Type is array (0 .. 3) of Nat8;
      Carries : Carries_Type;
      No_Carries : constant Carries_Type := (others => Pos_0);

      --  Shift right carries.  Note the Carries (0) is the LSB.
      procedure Shr_Carries is
      begin
         Carries := (Carries (1), Carries (2), Carries (3), Pos_0);
      end Shr_Carries;

      procedure Append_Carries is
      begin
         --  Expand the bit string.  Note that position 1 of the string8 is
         --  the MSB.
         while Carries /= No_Carries loop
            Append_String8 (Pos_0);
            Length := Length + 1;
            for I in reverse 2 .. Length loop
               Set_Element_String8 (Id, I, Element_String8 (Id, I - 1));
            end loop;
            Set_Element_String8 (Id, 1, Carries (0));
            Shr_Carries;
         end loop;
      end Append_Carries;

      --  Add 1 to Carries.  Overflow is not allowed and should be prevented by
      --  construction.
      procedure Add_One_To_Carries is
      begin
         for I in Carries'Range loop
            if Carries (I) = Pos_0 then
               Carries (I) := Pos_0 + 1;
               --  End of propagation.
               exit;
            else
               Carries (I) := Pos_0;
               --  Continue propagation.
               pragma Assert (I < Carries'Last);
            end if;
         end loop;
      end Add_One_To_Carries;
   begin
      pragma Assert (Source (Pos) = '"' or Source (Pos) = '%');
      Pos := Pos + 1;
      Length := 0;
      Id := Create_String8;
      Current_Context.Str_Id := Id;
      loop
         << Again >> null;
         C := Source (Pos);
         Pos := Pos + 1;
         exit when C = '"';

         if C in '0' .. '9' then
            V := Character'Pos (C) - Character'Pos ('0');
         elsif C = '_' then
            if Source (Pos) = '_' then
               Error_Msg_Scan
                 ("double underscore not allowed in a bit string");
            end if;
            if Source (Pos - 2) = '"' then
               Error_Msg_Scan
                 ("underscore not allowed at the start of a bit string");
            elsif Source (Pos) = '"' then
               Error_Msg_Scan
                 ("underscore not allowed at the end of a bit string");
            end if;
            goto Again;
         else
            if Characters_Kind (C) in Graphic_Character then
               Error_Msg_Scan
                 ("graphic character not allowed in decimal bit string");
               --  Continue the bit string
               V := 0;
            else
               Error_Msg_Scan ("bit string not terminated");
               Pos := Pos - 1;
               exit;
            end if;
         end if;

         --  Multiply by 10.
         Carries := (others => Pos_0);
         for I in reverse 1 .. Length loop
            --  Shift by 1 (*2).
            D := Element_String8 (Id, I);
            Set_Element_String8 (Id, I, Carries (0));
            Shr_Carries;
            --  Add D and D * 4.
            if D /= Pos_0 then
               Add_One_To_Carries;
               --  Add_Four_To_Carries:
               for I in 2 .. 3 loop
                  if Carries (I) = Pos_0 then
                     Carries (I) := Pos_0 + 1;
                     --  End of propagation.
                     exit;
                  else
                     Carries (I) := Pos_0;
                     --  Continue propagation.
                  end if;
               end loop;
            end if;
         end loop;
         Append_Carries;

         --  Add V.
         for I in Carries'Range loop
            D := V / 2;
            Carries (I) := Pos_0 + (V - 2 * D);
            V := D;
         end loop;
         for I in reverse 1 .. Length loop
            D := Element_String8 (Id, I);
            if D /= Pos_0 then
               Add_One_To_Carries;
            end if;
            Set_Element_String8 (Id, I, Carries (0));
            Shr_Carries;
            exit when Carries = No_Carries;
         end loop;
         Append_Carries;
      end loop;

      Current_Token := Tok_Bit_String;
      Current_Context.Str_Len := Length;
   end Scan_Dec_Bit_String;

   --  LRM08 15.2 Character set
   --  For each uppercase letter, there is a corresponding lowercase letter;
   --  and for each lowercase letter except [y diaeresis] and [german sharp s],
   --  there is a corresponding uppercase letter.
   type Character_Map is array (Character) of Character;
   To_Lower_Map : constant Character_Map :=
     (
      --  Uppercase ASCII letters.
      'A' => 'a',
      'B' => 'b',
      'C' => 'c',
      'D' => 'd',
      'E' => 'e',
      'F' => 'f',
      'G' => 'g',
      'H' => 'h',
      'I' => 'i',
      'J' => 'j',
      'K' => 'k',
      'L' => 'l',
      'M' => 'm',
      'N' => 'n',
      'O' => 'o',
      'P' => 'p',
      'Q' => 'q',
      'R' => 'r',
      'S' => 's',
      'T' => 't',
      'U' => 'u',
      'V' => 'v',
      'W' => 'w',
      'X' => 'x',
      'Y' => 'y',
      'Z' => 'z',

      --  Lowercase ASCII letters.
      'a' => 'a',
      'b' => 'b',
      'c' => 'c',
      'd' => 'd',
      'e' => 'e',
      'f' => 'f',
      'g' => 'g',
      'h' => 'h',
      'i' => 'i',
      'j' => 'j',
      'k' => 'k',
      'l' => 'l',
      'm' => 'm',
      'n' => 'n',
      'o' => 'o',
      'p' => 'p',
      'q' => 'q',
      'r' => 'r',
      's' => 's',
      't' => 't',
      'u' => 'u',
      'v' => 'v',
      'w' => 'w',
      'x' => 'x',
      'y' => 'y',
      'z' => 'z',

      --  Uppercase Latin-1 letters.
      UC_A_Grave          => LC_A_Grave,
      UC_A_Acute          => LC_A_Acute,
      UC_A_Circumflex     => LC_A_Circumflex,
      UC_A_Tilde          => LC_A_Tilde,
      UC_A_Diaeresis      => LC_A_Diaeresis,
      UC_A_Ring           => LC_A_Ring,
      UC_AE_Diphthong     => LC_AE_Diphthong,
      UC_C_Cedilla        => LC_C_Cedilla,
      UC_E_Grave          => LC_E_Grave,
      UC_E_Acute          => LC_E_Acute,
      UC_E_Circumflex     => LC_E_Circumflex,
      UC_E_Diaeresis      => LC_E_Diaeresis,
      UC_I_Grave          => LC_I_Grave,
      UC_I_Acute          => LC_I_Acute,
      UC_I_Circumflex     => LC_I_Circumflex,
      UC_I_Diaeresis      => LC_I_Diaeresis,
      UC_Icelandic_Eth    => LC_Icelandic_Eth,
      UC_N_Tilde          => LC_N_Tilde,
      UC_O_Grave          => LC_O_Grave,
      UC_O_Acute          => LC_O_Acute,
      UC_O_Circumflex     => LC_O_Circumflex,
      UC_O_Tilde          => LC_O_Tilde,
      UC_O_Diaeresis      => LC_O_Diaeresis,
      UC_O_Oblique_Stroke => LC_O_Oblique_Stroke,
      UC_U_Grave          => LC_U_Grave,
      UC_U_Acute          => LC_U_Acute,
      UC_U_Circumflex     => LC_U_Circumflex,
      UC_U_Diaeresis      => LC_U_Diaeresis,
      UC_Y_Acute          => LC_Y_Acute,
      UC_Icelandic_Thorn  => LC_Icelandic_Thorn,

      --  Lowercase Latin-1 letters.
      LC_A_Grave          => LC_A_Grave,
      LC_A_Acute          => LC_A_Acute,
      LC_A_Circumflex     => LC_A_Circumflex,
      LC_A_Tilde          => LC_A_Tilde,
      LC_A_Diaeresis      => LC_A_Diaeresis,
      LC_A_Ring           => LC_A_Ring,
      LC_AE_Diphthong     => LC_AE_Diphthong,
      LC_C_Cedilla        => LC_C_Cedilla,
      LC_E_Grave          => LC_E_Grave,
      LC_E_Acute          => LC_E_Acute,
      LC_E_Circumflex     => LC_E_Circumflex,
      LC_E_Diaeresis      => LC_E_Diaeresis,
      LC_I_Grave          => LC_I_Grave,
      LC_I_Acute          => LC_I_Acute,
      LC_I_Circumflex     => LC_I_Circumflex,
      LC_I_Diaeresis      => LC_I_Diaeresis,
      LC_Icelandic_Eth    => LC_Icelandic_Eth,
      LC_N_Tilde          => LC_N_Tilde,
      LC_O_Grave          => LC_O_Grave,
      LC_O_Acute          => LC_O_Acute,
      LC_O_Circumflex     => LC_O_Circumflex,
      LC_O_Tilde          => LC_O_Tilde,
      LC_O_Diaeresis      => LC_O_Diaeresis,
      LC_O_Oblique_Stroke => LC_O_Oblique_Stroke,
      LC_U_Grave          => LC_U_Grave,
      LC_U_Acute          => LC_U_Acute,
      LC_U_Circumflex     => LC_U_Circumflex,
      LC_U_Diaeresis      => LC_U_Diaeresis,
      LC_Y_Acute          => LC_Y_Acute,
      LC_Icelandic_Thorn  => LC_Icelandic_Thorn,

      --  Lowercase latin-1 characters without corresponding uppercase one.
      LC_Y_Diaeresis      => LC_Y_Diaeresis,
      LC_German_Sharp_S   => LC_German_Sharp_S,

      --  Not a letter.
      others => NUL);

   procedure Error_Too_Long is
   begin
      Error_Msg_Scan ("identifier is too long (>"
                        & Natural'Image (Max_Name_Length - 1) & ")");
   end Error_Too_Long;

   -- LRM93 13.3.1
   -- Basic Identifiers
   -- A basic identifier consists only of letters, digits, and underlines.
   -- BASIC_IDENTIFIER ::= LETTER { [ UNDERLINE ] LETTER_OR_DIGIT }
   -- LETTER_OR_DIGIT ::= LETTER | DIGIT
   -- LETTER ::= UPPER_CASE_LETTER | LOWER_CASE_LETTER
   --
   -- NB: At the call of this procedure, the current character must be a legal
   -- character for a basic identifier.
   procedure Scan_Identifier (Allow_PSL : Boolean)
   is
      use Name_Table;
      --  Local copy for speed-up.
      Source : constant File_Buffer_Acc := Current_Context.Source;
      P : Source_Ptr;

      --  Current and next character.
      C : Character;

      Buffer : String (1 .. Max_Name_Length);
      Len : Natural;
   begin
      -- This is an identifier or a key word.
      Len := 0;
      P := Pos;

      loop
         --  Source (pos) is correct.
         --  LRM93 13.3.1
         --   All characters if a basic identifier are signifiant, including
         --   any underline character inserted between a letter or digit and
         --   an adjacent letter or digit.
         --   Basic identifiers differing only in the use of the corresponding
         --   upper and lower case letters are considered as the same.
         --
         --  GHDL: This is achieved by converting all upper case letters into
         --  equivalent lower case letters.
         --  The opposite (converting to upper lower case letters) is not
         --  possible because two characters have no upper-case equivalent.
         C := Source (P);
         case C is
            when 'A' .. 'Z' =>
               C := Character'Val
                 (Character'Pos (C)
                    + Character'Pos ('a') - Character'Pos ('A'));
            when 'a' .. 'z' | '0' .. '9' =>
               null;
            when '_' =>
               if Source (P + 1) = '_' then
                  --  Need to set the current position for the error message.
                  Pos := P + 1;
                  Error_Msg_Scan ("two underscores can't be consecutive");
               end if;
            when ' ' | ')' | '.' | ';' | ':' =>
               exit;
            when others =>
               --  Non common case.
               case Characters_Kind (C) is
                  when Upper_Case_Letter | Lower_Case_Letter =>
                     if Vhdl_Std = Vhdl_87 then
                        Error_8bit;
                     end if;
                     C := To_Lower_Map (C);
                     pragma Assert (C /= NUL);
                  when Digit =>
                     raise Internal_Error;
                  when others =>
                     exit;
               end case;
         end case;

         --  Put character in name buffer.  FIXME: compute the hash at the same
         --  time ?
         if Len >= Max_Name_Length - 1 then
            if Len = Max_Name_Length -1 then
               Error_Msg_Scan ("identifier is too long (>"
                                 & Natural'Image (Max_Name_Length - 1) & ")");
               --  Accept this last one character, so that no error for the
               --  following characters.
               Len := Len + 1;
               Buffer (Len) := C;
            end if;
         else
            Len := Len + 1;
            Buffer (Len) := C;
         end if;

         --  Next character.
         P := P + 1;
      end loop;

      if Source (P - 1) = '_' then
         if Allow_PSL then
            --  Some PSL reserved words finish with '_'.
            P := P - 1;
            Len := Len - 1;
            C := '_';
         else
            --  Eat the trailing underscore.
            Pos := P - 1;
            Error_Msg_Scan ("an identifier cannot finish with '_'");
         end if;
      end if;

      --  Update position in the scan context.
      Pos := P;

      -- LRM93 13.2
      -- At least one separator is required between an identifier or an
      -- abstract literal and an adjacent identifier or abstract literal.
      case Characters_Kind (C) is
         when Digit
           | Upper_Case_Letter
           | Lower_Case_Letter =>
            raise Internal_Error;
         when Other_Special_Character | Special_Character =>
            if (C = '"' or C = '%') and then Len <= 2 then
               if C = '%' and Vhdl_Std >= Vhdl_08 then
                  Error_Msg_Scan ("'%%' not allowed in vhdl 2008 "
                                    & "(was replacement character)");
                  --  Continue as a bit string.
               end if;

               --  Good candidate for bit string.

               --  LRM93 13.7
               --  BASE_SPECIFIER ::= B | O | X
               --
               --  A letter in a bit string literal (either an extended digit
               --  or the base specifier) can be written either in lower case
               --  or in upper case, with the same meaning.
               --
               --  LRM08 15.8 Bit string literals
               --  BASE_SPECICIER ::=
               --     B | O | X | UB | UO | UX | SB | SO | SX | D
               --
               --  An extended digit and the base specifier in a bit string
               --  literal can be written either in lowercase or in uppercase,
               --  with the same meaning.
               declare
                  Base : Nat32;
                  Cl : constant Character := Buffer (Len);
                  Cf : constant Character := Buffer (1);
               begin
                  Current_Context.Bit_Str_Base := Cl;
                  if Cl = 'b' then
                     Base := 1;
                  elsif Cl = 'o' then
                     Base := 3;
                  elsif Cl = 'x' then
                     Base := 4;
                  elsif Vhdl_Std >= Vhdl_08 and Len = 1 and Cf = 'd' then
                     Current_Context.Bit_Str_Sign := ' ';
                     Scan_Dec_Bit_String;
                     return;
                  else
                     Base := 0;
                  end if;
                  if Base > 0 then
                     if Len = 1 then
                        Current_Context.Bit_Str_Sign := ' ';
                        Scan_Bit_String (Base);
                        return;
                     elsif Vhdl_Std >= Vhdl_08
                       and then (Cf = 's' or Cf = 'u')
                     then
                        Current_Context.Bit_Str_Sign := Cf;
                        Scan_Bit_String (Base);
                        return;
                     end if;
                  end if;
               end;
            elsif Vhdl_Std > Vhdl_87 and then C = '\' then
               --  Start of extended identifier.  Cannot follow an identifier.
               Error_Separator;
            end if;

         when Invalid =>
            --  Improve error message for use of UTF-8 quote marks.
            --  It's possible because in the sequence of UTF-8 bytes for the
            --  quote marks, there are invalid character (in the 128-160
            --  range).
            if C = Character'Val (16#80#)
              and then Buffer (Len) = Character'Val (16#e2#)
              and then (Source (Pos + 1) = Character'Val (16#98#)
                          or else Source (Pos + 1) = Character'Val (16#99#))
            then
               --  UTF-8 left or right single quote mark.
               if Len > 1 then
                  --  The first byte (0xe2) is part of the identifier.  An
                  --  error will be detected as the next byte (0x80) is
                  --  invalid.  Remove the first byte from the identifier, and
                  --  let's catch the error later.
                  Len := Len - 1;
                  Pos := Pos - 1;
               else
                  Error_Msg_Scan ("invalid use of UTF8 character for '");
                  Pos := Pos + 2;

                  --  Distinguish between character literal and tick.  Don't
                  --  care about possible invalid character literal, as in any
                  --  case we have already emitted an error message.
                  if Current_Context.Prev_Token /= Tok_Identifier
                    and then Current_Context.Prev_Token /= Tok_Character
                    and then
                    (Source (Pos + 1) = '''
                       or else
                       (Source (Pos + 1) = Character'Val (16#e2#)
                          and then Source (Pos + 2) = Character'Val (16#80#)
                          and then Source (Pos + 3) = Character'Val (16#99#)))
                  then
                     Current_Token := Tok_Character;
                     Current_Context.Identifier :=
                       Name_Table.Get_Identifier (Source (Pos));
                     if Source (Pos + 1) = ''' then
                        Pos := Pos + 2;
                     else
                        Pos := Pos + 4;
                     end if;
                  else
                     Current_Token := Tok_Tick;
                  end if;
                  return;
               end if;
            end if;
         when Format_Effector
           | Space_Character =>
            null;
      end case;

      -- Hash it.
      Current_Context.Identifier := Get_Identifier (Buffer (1 .. Len));
      Current_Token := Tok_Identifier;
   end Scan_Identifier;

   procedure Scan_Psl_Keyword_Em (Tok : Token_Type; Tok_Em : Token_Type) is
   begin
      if Source (Pos) = '!' then
         Pos := Pos + 1;
         Current_Token := Tok_Em;
      else
         Current_Token := Tok;
      end if;
   end Scan_Psl_Keyword_Em;
   pragma Inline (Scan_Psl_Keyword_Em);

   procedure Scan_Psl_Keyword_Em_Un
     (Tok, Tok_Em, Tok_Un, Tok_Em_Un : Token_Type) is
   begin
      if Source (Pos) = '!' then
         Pos := Pos + 1;
         if Source (Pos) = '_' then
            Pos := Pos + 1;
            Current_Token := Tok_Em_Un;
         else
            Current_Token := Tok_Em;
         end if;
      elsif Source (Pos) = '_' then
         Pos := Pos + 1;
         Current_Token := Tok_Un;
      else
         Current_Token := Tok;
      end if;
   end Scan_Psl_Keyword_Em_Un;
   pragma Inline (Scan_Psl_Keyword_Em_Un);

   procedure Identifier_To_Token
   is
      use Std_Names;
   begin
      if Current_Identifier in Name_Id_Keywords then
         -- LRM93 13.9
         --   The identifiers listed below are called reserved words and are
         --   reserved for signifiances in the language.
         -- IN: this is also achieved in packages std_names and tokens.
         Current_Token := Token_Type'Val
           (Token_Type'Pos (Tok_First_Keyword)
              + Current_Identifier - Name_First_Keyword);
         case Current_Identifier is
            when Name_Id_AMS_Reserved_Words =>
               if not AMS_Vhdl then
                  if Is_Warning_Enabled (Warnid_Reserved_Word) then
                     Warning_Msg_Scan
                       (Warnid_Reserved_Word,
                        "using %i AMS-VHDL reserved word as an identifier",
                        +Current_Identifier);
                  end if;
                  Current_Token := Tok_Identifier;
               end if;
            when Name_Id_Vhdl08_Reserved_Words =>
               if Vhdl_Std < Vhdl_08 then
                  --  Some vhdl08 reserved words are PSL keywords.
                  if Flag_Psl then
                     case Current_Identifier is
                        when Name_Prev =>
                           Current_Token := Tok_Prev;
                        when Name_Stable =>
                           Current_Token := Tok_Stable;
                        when Name_Rose =>
                           Current_Token := Tok_Rose;
                        when Name_Fell =>
                           Current_Token := Tok_Fell;
                        when Name_Onehot =>
                           Current_Token := Tok_Onehot;
                        when Name_Onehot0 =>
                           Current_Token := Tok_Onehot0;
                        when Name_Sequence =>
                           Current_Token := Tok_Sequence;
                        when Name_Property =>
                           Current_Token := Tok_Property;
                        when Name_Assume =>
                           Current_Token := Tok_Assume;
                        when Name_Cover =>
                           Current_Token := Tok_Cover;
                        when Name_Default =>
                           Current_Token := Tok_Default;
                        when Name_Restrict =>
                           Current_Token := Tok_Restrict;
                        when Name_Restrict_Guarantee =>
                           Current_Token := Tok_Restrict_Guarantee;
                        when Name_Vmode =>
                           Current_Token := Tok_Vmode;
                        when Name_Vprop =>
                           Current_Token := Tok_Vprop;
                        when Name_Vunit =>
                           Current_Token := Tok_Vunit;
                        when Name_Inherit =>
                           Current_Token := Tok_Inherit;
                        when others =>
                           Current_Token := Tok_Identifier;
                     end case;
                  else
                     Current_Token := Tok_Identifier;
                  end if;
                  if Is_Warning_Enabled (Warnid_Reserved_Word)
                    and then Current_Token = Tok_Identifier
                  then
                     Warning_Msg_Scan
                       (Warnid_Reserved_Word,
                        "using %i vhdl-2008 reserved word as an identifier",
                        +Current_Identifier);
                  end if;
               end if;
            when Name_Id_Vhdl00_Reserved_Words =>
               if Vhdl_Std < Vhdl_00 then
                  if Is_Warning_Enabled (Warnid_Reserved_Word) then
                     Warning_Msg_Scan
                       (Warnid_Reserved_Word,
                        "using %i vhdl-2000 reserved word as an identifier",
                        +Current_Identifier);
                  end if;
                  Current_Token := Tok_Identifier;
               end if;
            when Name_Id_Vhdl93_Reserved_Words =>
               if Vhdl_Std = Vhdl_87 then
                  if Is_Warning_Enabled (Warnid_Reserved_Word) then
                     Report_Start_Group;
                     Warning_Msg_Scan
                       (Warnid_Reserved_Word,
                        "using %i vhdl93 reserved word as a vhdl87 identifier",
                        +Current_Identifier);
                     Warning_Msg_Scan
                       (Warnid_Reserved_Word,
                        "(use option --std=93 to compile as vhdl93)");
                     Report_End_Group;
                  end if;
                  Current_Token := Tok_Identifier;
               end if;
            when Name_Id_Vhdl87_Reserved_Words =>
               if Flag_Psl then
                  if Current_Token = Tok_Until then
                     Scan_Psl_Keyword_Em_Un (Tok_Until, Tok_Until_Em,
                                             Tok_Until_Un, Tok_Until_Em_Un);
                  elsif Current_Token = Tok_Next then
                     Scan_Psl_Keyword_Em (Tok_Next, Tok_Next_Em);
                  end if;
               end if;
            when others =>
               raise Program_Error;
         end case;
      elsif Flag_Psl then
         case Current_Identifier is
            when Name_Prev =>
               Current_Token := Tok_Prev;
            when Name_Stable =>
               Current_Token := Tok_Stable;
            when Name_Rose =>
               Current_Token := Tok_Rose;
            when Name_Fell =>
               Current_Token := Tok_Fell;
            when Name_Onehot =>
               Current_Token := Tok_Onehot;
            when Name_Onehot0 =>
               Current_Token := Tok_Onehot0;
            when Name_Clock =>
               Current_Token := Tok_Psl_Clock;
            when Name_Const =>
               Current_Token := Tok_Psl_Const;
            when Name_Boolean =>
               Current_Token := Tok_Psl_Boolean;
            when Name_Sequence =>
               Current_Token := Tok_Sequence;
            when Name_Property =>
               Current_Token := Tok_Property;
            when Name_Endpoint =>
               Current_Token := Tok_Psl_Endpoint;
            when Name_Assume =>
               Current_Token := Tok_Assume;
            when Name_Cover =>
               Current_Token := Tok_Cover;
            when Name_Default =>
               Current_Token := Tok_Default;
            when Name_Restrict =>
               Current_Token := Tok_Restrict;
            when Name_Restrict_Guarantee =>
               Current_Token := Tok_Restrict_Guarantee;
            when Name_Inf =>
               Current_Token := Tok_Inf;
            when Name_Within =>
               Current_Token := Tok_Within;
            when Name_Abort =>
               Current_Token := Tok_Abort;
            when Name_Async_Abort =>
               Current_Token := Tok_Async_Abort;
            when Name_Sync_Abort =>
               Current_Token := Tok_Sync_Abort;
            when Name_Before =>
               Scan_Psl_Keyword_Em_Un (Tok_Before, Tok_Before_Em,
                                       Tok_Before_Un, Tok_Before_Em_Un);
            when Name_Always =>
               Current_Token := Tok_Always;
            when Name_Never =>
               Current_Token := Tok_Never;
            when Name_Eventually =>
               if Source (Pos) = '!' then
                  Pos := Pos + 1;
               else
                  Error_Msg_Scan ("'!' expected after 'eventually'");
               end if;
               Current_Token := Tok_Eventually_Em;
            when Name_Next_A =>
               Scan_Psl_Keyword_Em (Tok_Next_A, Tok_Next_A_Em);
            when Name_Next_E =>
               Scan_Psl_Keyword_Em (Tok_Next_E, Tok_Next_E_Em);
            when Name_Next_Event =>
               Scan_Psl_Keyword_Em (Tok_Next_Event, Tok_Next_Event_Em);
            when Name_Next_Event_A =>
               Scan_Psl_Keyword_Em (Tok_Next_Event_A, Tok_Next_Event_A_Em);
            when Name_Next_Event_E =>
               Scan_Psl_Keyword_Em (Tok_Next_Event_E, Tok_Next_Event_E_Em);
            when Name_Until =>
               raise Internal_Error;
            when others =>
               Current_Token := Tok_Identifier;
               if Source (Pos - 1) = '_' then
                  Error_Msg_Scan ("identifiers cannot finish with '_'");
               end if;
         end case;
      end if;
   end Identifier_To_Token;

   --  LRM93 13.3.2
   --  EXTENDED_IDENTIFIER ::= \ GRAPHIC_CHARACTER { GRAPHIC_CHARACTER } \
   --
   --  Create an (extended) indentifier.
   --  Extended identifiers are stored as they appear (leading and tailing
   --  backslashes, doubling backslashes inside).
   procedure Scan_Extended_Identifier
   is
      use Name_Table;
      Buffer : String (1 .. Max_Name_Length);
      Len : Natural;
      C : Character;
   begin
      --  LRM93 13.3.2
      --  Moreover, every extended identifiers is distinct from any basic
      --  identifier.
      --  GHDL: This is satisfied by storing '\' in the name table.
      Len := 1;
      Buffer (1) := '\';
      loop
         --  Next character.
         Pos := Pos + 1;
         C := Source (Pos);

         if C = '\' then
            --  LRM93 13.3.2
            --  If a backslash is to be used as one of the graphic characters
            --  of an extended literal, it must be doubled.
            --  LRM93 13.3.2
            --  (a doubled backslash couting as one character)
            if Len >= Max_Name_Length - 1 then
               if Len = Max_Name_Length - 1 then
                  Error_Too_Long;
                  --  Accept this last one.
                  Len := Len + 1;
                  Buffer (Len) := C;
               end if;
            else
               Len := Len + 1;
               Buffer (Len) := C;
            end if;

            Pos := Pos + 1;
            C := Source (Pos);

            exit when C /= '\';
         end if;

         case Characters_Kind (C) is
            when Format_Effector =>
               Error_Msg_Scan ("format effector in extended identifier");
               exit;
            when Graphic_Character =>
               null;
            when Invalid =>
               if C = Files_Map.EOT
                 and then Pos >= Current_Context.File_Len
               then
                  Error_Msg_Scan
                    ("extended identifier not terminated at end of file");
               elsif C = LF or C = CR then
                  Error_Msg_Scan
                    ("extended identifier not terminated at end of line");
               else
                  Error_Msg_Scan ("invalid character in extended identifier");
               end if;
               exit;
         end case;

         --  LRM93 13.3.2
         --  Extended identifiers differing only in the use of corresponding
         --  upper and lower case letters are distinct.
         if Len >= Max_Name_Length - 1 then
            if Len = Max_Name_Length - 1 then
               Error_Too_Long;
               --  Accept this last one.
               Len := Len + 1;
               Buffer (Len) := C;
            end if;
         else
            Len := Len + 1;
            Buffer (Len) := C;
         end if;
      end loop;

      if Len <= 2 then
         Error_Msg_Scan ("empty extended identifier is not allowed");
      end if;

      --  LRM93 13.2
      --  At least one separator is required between an identifier or an
      --  abstract literal and an adjacent identifier or abstract literal.
      case Characters_Kind (C) is
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
      Current_Context.Identifier := Get_Identifier (Buffer (1 .. Len));
      Current_Token := Tok_Identifier;
   end Scan_Extended_Identifier;

   procedure Convert_Identifier (Str : in out String; Err : out Boolean)
   is
      F : constant Integer := Str'First;

      procedure Error_Bad is
      begin
         Error_Msg_Option ("bad character in identifier");
      end Error_Bad;

      procedure Error_8bit is
      begin
         Error_Msg_Option ("8 bits characters not allowed in vhdl87");
      end Error_8bit;

      C : Character;
   begin
      Err := True;

      if Str'Length = 0 then
         Error_Msg_Option ("identifier required");
         return;
      end if;

      if Str (F) = '\' then
         --  Extended identifier.
         if Vhdl_Std = Vhdl_87 then
            Error_Msg_Option ("extended identifiers not allowed in vhdl87");
            return;
         end if;

         if Str'Last < F + 2 then
            Error_Msg_Option ("extended identifier is too short");
            return;
         end if;
         if Str (Str'Last) /= '\' then
            Error_Msg_Option ("extended identifier must finish with a '\'");
            return;
         end if;
         for I in F + 1 .. Str'Last - 1 loop
            C := Str (I);
            case Characters_Kind (C) is
               when Format_Effector =>
                  Error_Msg_Option ("format effector in extended identifier");
                  return;
               when Graphic_Character =>
                  if C = '\' then
                     if Str (I + 1) /= '\'
                       or else I = Str'Last - 1
                     then
                        Error_Msg_Option ("anti-slash must be doubled "
                                            & "in extended identifier");
                        return;
                     end if;
                  end if;
               when Invalid =>
                  Error_Bad;
                  return;
            end case;
         end loop;
      else
         --  Identifier
         for I in F .. Str'Last loop
            C := Str (I);
            case Characters_Kind (C) is
               when Upper_Case_Letter =>
                  if Vhdl_Std = Vhdl_87 and C > 'Z' then
                     Error_8bit;
                     return;
                  end if;
                  Str (I) := To_Lower_Map (C);
               when Lower_Case_Letter | Digit =>
                  if Vhdl_Std = Vhdl_87 and C > 'z' then
                     Error_8bit;
                     return;
                  end if;
               when Special_Character =>
                  -- The current character is legal in an identifier.
                  if C = '_' then
                     if I = 1 then
                        Error_Msg_Option
                          ("an identifier cannot start with an underscore");
                        return;
                     end if;
                     if Str (I - 1) = '_' then
                        Error_Msg_Option
                          ("two underscores can't be consecutive");
                        return;
                     end if;
                     if I = Str'Last then
                        Error_Msg_Option
                          ("an identifier cannot finish with an underscore");
                        return;
                     end if;
                  else
                     Error_Bad;
                     return;
                  end if;
               when others =>
                  Error_Bad;
                  return;
            end case;
         end loop;
      end if;
      Err := False;
   end Convert_Identifier;

   --  Internal scanner function: return True if C must be considered as a line
   --  terminator.  This also includes EOT (which terminates the file or is
   --  invalid).
   function Is_EOL (C : Character) return Boolean is
   begin
      case C is
         when CR | LF | VT | FF | Files_Map.EOT =>
            return True;
         when others =>
            return False;
      end case;
   end Is_EOL;

   --  Advance scanner till the first non-space character.
   procedure Skip_Spaces is
   begin
      while Source (Pos) = ' ' or Source (Pos) = HT loop
         Pos := Pos + 1;
      end loop;
   end Skip_Spaces;

   --  Eat all characters until end-of-line (not included).
   procedure Skip_Until_EOL is
   begin
      while not Is_EOL (Source (Pos)) loop
         --  Don't warn about invalid character, it's somewhat out of the
         --  scope.
         Pos := Pos + 1;
      end loop;
   end Skip_Until_EOL;

   --  Scan an identifier within a comment.  Only lower case letters are
   --  allowed.
   procedure Scan_Comment_Identifier (Id : out Name_Id; Create : Boolean)
   is
      use Name_Table;
      Buffer : String (1 .. Max_Name_Length);
      Len : Natural;
      C : Character;
   begin
      Id := Null_Identifier;
      Skip_Spaces;

      --  The identifier shall start with a letter.
      case Source (Pos) is
         when 'a' .. 'z'
            | 'A' .. 'Z' =>
            null;
         when others =>
            return;
      end case;

      --  Scan the identifier.
      Len := 0;
      loop
         C := Source (Pos);
         case C is
            when 'a' .. 'z' =>
               null;
            when 'A' .. 'Z' =>
               C := Character'Val (Character'Pos (C) + 32);
            when '_' =>
               null;
            when others =>
               exit;
         end case;
         Len := Len + 1;
         Buffer (Len) := C;
         Pos := Pos + 1;
      end loop;

      --  Shall be followed by a space or a new line.
      if not (C = ' ' or else C = HT or else Is_EOL (C)) then
         return;
      end if;

      if Create then
         Id := Get_Identifier (Buffer (1 .. Len));
      else
         Id := Get_Identifier_No_Create (Buffer (1 .. Len));
      end if;
   end Scan_Comment_Identifier;

   package Directive_Protect is
      --  Called to scan a protect tool directive.
      procedure Scan_Protect_Directive;
   end Directive_Protect;

   --  Body is put in a separate file to avoid pollution.
   package body Directive_Protect is separate;

   --  Called to scan a tool directive.
   procedure Scan_Tool_Directive
   is
      procedure Error_Missing_Directive is
      begin
         Error_Msg_Scan ("tool directive required after '`'");
         Skip_Until_EOL;
      end Error_Missing_Directive;

      C : Character;
   begin
      --  The current character is '`'.
      Pos := Pos + 1;
      Skip_Spaces;

      --  Check and scan identifier.
      C := Source (Pos);
      if Characters_Kind (C) not in Letter then
         Error_Missing_Directive;
         return;
      end if;

      Scan_Identifier (False);

      if Current_Token /= Tok_Identifier then
         Error_Missing_Directive;
         return;
      end if;

      Skip_Spaces;

      --  Dispatch according to the identifier.
      if Current_Identifier = Std_Names.Name_Protect then
         Directive_Protect.Scan_Protect_Directive;
      else
         Error_Msg_Scan
           ("unknown tool directive %i ignored", +Current_Identifier);
         Skip_Until_EOL;
      end if;
   end Scan_Tool_Directive;

   --  Skip until new_line after translate_on/translate_off.
   procedure Scan_Translate_On_Off (Id : Name_Id) is
   begin
      --  Expect new line.
      Skip_Spaces;

      if not Is_EOL (Source (Pos)) then
         Warning_Msg_Scan (Warnid_Pragma, "garbage ignored after '%i'", +Id);
         loop
            Pos := Pos + 1;
            exit when Is_EOL (Source (Pos));
         end loop;
      end if;
   end Scan_Translate_On_Off;

   procedure Scan_Translate_Off is
   begin
      if Current_Context.Translate_Off then
         Warning_Msg_Scan (Warnid_Pragma, "nested 'translate_off' ignored");
         return;
      end if;

      --  'pragma translate_off' has just been scanned.
      Scan_Translate_On_Off (Std_Names.Name_Translate_Off);

      Current_Context.Translate_Off := True;

      --  Recursive scan until 'translate_on' is scanned.
      loop
         Scan;
         if not Current_Context.Translate_Off then
            --  That token is discarded.
            pragma Assert (Current_Token = Tok_Line_Comment);
            Flag_Comment := False;
            exit;
         elsif Current_Token = Tok_Eof then
            Warning_Msg_Scan (Warnid_Pragma,
                              "unterminated 'translate_off'");
            Current_Context.Translate_Off := False;
            exit;
         end if;
      end loop;

      --  The scanner is now at the EOL of the translate_on or at the EOF.
      --  Continue scanning.
   end Scan_Translate_Off;

   procedure Scan_Translate_On is
   begin
      if not Current_Context.Translate_Off then
         Warning_Msg_Scan
           (Warnid_Pragma,
            "'translate_on' without coresponding 'translate_off'");
         return;
      end if;

      --  'pragma translate_off' has just been scanned.
      Scan_Translate_On_Off (Std_Names.Name_Translate_On);

      Current_Context.Translate_Off := False;

      --  Return a token that will be discarded.
      Flag_Comment := True;
   end Scan_Translate_On;

   procedure Scan_Comment_Pragma
   is
      use Std_Names;
      Id : Name_Id;
   begin
      Scan_Comment_Identifier (Id, True);
      case Id is
         when Null_Identifier =>
            Warning_Msg_Scan
              (Warnid_Pragma, "incomplete pragma directive ignored");
         when Name_Translate =>
            Scan_Comment_Identifier (Id, False);
            case Id is
               when Name_On =>
                  Scan_Translate_On;
               when Name_Off =>
                  Scan_Translate_Off;
               when others =>
                  Warning_Msg_Scan
                    (Warnid_Pragma,
                     "pragma translate must be followed by 'on' or 'off'");
            end case;
         when Name_Translate_Off
           |  Name_Synthesis_Off =>
            Scan_Translate_Off;
         when Name_Translate_On
           |  Name_Synthesis_On =>
            Scan_Translate_On;
         when Name_Label
           |  Name_Label_Applies_To
           |  Name_Return_Port_Name
           |  Name_Map_To_Operator
           |  Name_Type_Function
           |  Name_Built_In =>
            --  Used by synopsys, discarded.
            Skip_Until_EOL;
         when others =>
            Warning_Msg_Scan
              (Warnid_Pragma, "unknown pragma %i ignored", +Id);
      end case;
   end Scan_Comment_Pragma;

   --  Scan tokens within a comment.  Return TRUE if Current_Token was set,
   --  return FALSE to discard the comment (ie treat it like a real comment).
   function Scan_Comment return Boolean
   is
      use Std_Names;
      Id : Name_Id;
   begin
      Scan_Comment_Identifier (Id, False);

      if Id = Null_Identifier then
         return False;
      end if;

      case Id is
         when Name_Psl =>
            --  Accept tokens after '-- psl'.
            if Flag_Psl_Comment then
               Flag_Psl := True;
               Flag_Scan_In_Comment := True;
               return True;
            end if;
         when Name_Pragma
           | Name_Synthesis
           | Name_Synopsys =>
            if Flag_Pragma_Comment then
               Scan_Comment_Pragma;
               return False;
            end if;
         when others =>
            null;
      end case;
      return False;
   end Scan_Comment;

   --  The Scan_Next_Line procedure must be called after each end-of-line to
   --  register to next line number.  This is called by Scan_CR_Newline and
   --  Scan_LF_Newline.
   procedure Scan_Next_Line is
   begin
      Files_Map.Skip_Gap (Current_Context.Source_File, Pos);
      Current_Context.Line_Number := Current_Context.Line_Number + 1;
      Current_Context.Line_Pos := Pos;
      File_Add_Line_Number
        (Current_Context.Source_File, Current_Context.Line_Number, Pos);
   end Scan_Next_Line;

   --  Scan a CR end-of-line.
   procedure Scan_CR_Newline is
   begin
      -- Accept CR or CR+LF as line separator.
      if Source (Pos + 1) = LF then
         Pos := Pos + 2;
      else
         Pos := Pos + 1;
      end if;
      Scan_Next_Line;
   end Scan_CR_Newline;

   --  Scan a LF end-of-line.
   procedure Scan_LF_Newline is
   begin
      -- Accept LF or LF+CR as line separator.
      if Source (Pos + 1) = CR then
         Pos := Pos + 2;
      else
         Pos := Pos + 1;
      end if;
      Scan_Next_Line;
   end Scan_LF_Newline;

   --  Emit an error message for an invalid character.
   procedure Error_Bad_Character is
   begin
      --  Technically character literals, string literals, extended
      --  identifiers and comments.
      Error_Msg_Scan ("character %c can only be used in strings or comments",
                      +Source (Pos));
   end Error_Bad_Character;

   procedure Scan_Block_Comment is
   begin
      Current_Context.Prev_Pos := Pos;
      Current_Context.Token_Pos := Pos;

      loop
         case Source (Pos) is
            when '/' =>
               --  LRM08 15.9
               --  Moreover, an occurrence of a solidus character
               --  immediately followed by an asterisk character
               --  within a delimited comment is not interpreted as
               --  the start of a nested delimited comment.
               if Source (Pos + 1) = '*' then
                  Warning_Msg_Scan (Warnid_Nested_Comment,
                                    "'/*' found within a block comment");
               end if;
               Pos := Pos + 1;
            when '*' =>
               if Source (Pos + 1) = '/' then
                  if Pos > Current_Context.Token_Pos then
                     --  There are characters before the end of comment, so
                     --  first return them.
                     Current_Token := Tok_Block_Comment_Text;
                  else
                     Pos := Pos + 2;
                     Current_Token := Tok_Block_Comment_End;
                  end if;
                  return;
               else
                  Pos := Pos + 1;
               end if;
            when CR =>
               if Pos > Current_Context.Token_Pos then
                  --  There are characters before the CR, so
                  --  first return them.
                  Current_Token := Tok_Block_Comment_Text;
               else
                  Scan_CR_Newline;
                  Current_Token := Tok_Newline;
               end if;
               return;
            when LF =>
               if Pos > Current_Context.Token_Pos then
                  --  There are characters before the LF, so
                  --  first return them.
                  Current_Token := Tok_Block_Comment_Text;
               else
                  Scan_LF_Newline;
                  Current_Token := Tok_Newline;
               end if;
               return;
            when Files_Map.EOT =>
               if Pos >= Current_Context.File_Len then
                  --  Point at the start of the comment.
                  Error_Msg_Scan
                    (+Get_Token_Location,
                     "block comment not terminated at end of file");
                  Current_Token := Tok_Eof;
                  return;
               end if;
               Pos := Pos + 1;
            when others =>
               Pos := Pos + 1;
         end case;
      end loop;
   end Scan_Block_Comment;

   -- Get a new token.
   procedure Scan is
   begin
      if Current_Token /= Tok_Invalid then
         Current_Context.Prev_Token := Current_Token;
      end if;

      Current_Context.Prev_Pos := Pos;

      << Again >> null;

      --  Skip commonly used separators.
      --  (Like Skip_Spaces but manually inlined for speed).
      while Source (Pos) = ' ' or Source (Pos) = HT loop
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
         when LF =>
            Scan_LF_Newline;
            if Flag_Newline then
               Current_Token := Tok_Newline;
               return;
            end if;
            goto Again;
         when CR =>
            Scan_CR_Newline;
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
               --   whether a description is legal or illegal.
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
               if Flag_Comment_Keyword and then Scan_Comment then
                  goto Again;
               end if;

               --  LRM93 13.2
               --  In any case, a sequence of one or more format
               --  effectors other than horizontal tabulation must
               --  cause at least one end of line.
               while not Is_EOL (Source (Pos)) loop
                  --  LRM93 13.1
                  --  The only characters allowed in the text of a VHDL
                  --  description are the graphic characters and the format
                  --  effectors.

                  --  LRM02 13.1 Character set
                  --  The only characters allowed in the text of a VHDL
                  --  description (except within comments -- see 13.8) [...]
                  --
                  --  LRM02 13.8 Comments
                  --  A comment [...] may contain any character except the
                  --  format effectors vertical tab, carriage return, line
                  --  feed and form feed.
                  if not (Flags.Mb_Comment
                          or Flags.Flag_Relaxed_Rules
                          or Vhdl_Std >= Vhdl_02)
                    and then Characters_Kind (Source (Pos)) = Invalid
                  then
                     Error_Msg_Scan ("invalid character, even in a comment "
                                       & "(turn off with -C)");
                  end if;
                  Pos := Pos + 1;
               end loop;
               if Flag_Comment then
                  Current_Token := Tok_Line_Comment;
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
            elsif Source (Pos + 1) = '*' then
               --  LRM08 15.9 Comments
               --  A delimited comment start with a solidus (slash) character
               --  immediately followed by an asterisk character and extends up
               --  to the first subsequent occurrence of an asterisk character
               --  immediately followed by a solidus character.
               if Vhdl_Std < Vhdl_08 then
                  Error_Msg_Scan
                    ("block comment are not allowed before vhdl 2008");
               end if;

               --  Skip '/*'.
               Pos := Pos + 2;

               if Flag_Comment then
                  Current_Token := Tok_Block_Comment_Start;
                  return;
               end if;

               loop
                  Scan_Block_Comment;
                  exit when Current_Token = Tok_Block_Comment_End
                    or else Current_Token = Tok_Eof;
               end loop;
               goto Again;
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
               if Source (Pos + 1) = '=' then
                  --  != is not allowed in VHDL, but be friendly with C users.
                  Error_Msg_Scan
                    (+Get_Token_Location, "Use '/=' for inequality in vhdl");
                  Current_Token := Tok_Not_Equal;
                  Pos := Pos + 1;
               else
                  --  LRM93 13.10
                  --  A vertical line (|) can be replaced by an exclamation
                  --  mark (!) where used as a delimiter.
                  Current_Token := Tok_Bar;
               end if;
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
            case Source (Pos + 1) is
               when '=' =>
                  Current_Token := Tok_Less_Equal;
                  Pos := Pos + 2;
               when '>' =>
                  Current_Token := Tok_Box;
                  Pos := Pos + 2;
               when '<' =>
                  Current_Token := Tok_Double_Less;
                  Pos := Pos + 2;
               when '-' =>
                  if Flag_Psl and then Source (Pos + 2) = '>' then
                     Current_Token := Tok_Equiv_Arrow;
                     Pos := Pos + 3;
                  else
                     Current_Token := Tok_Less;
                     Pos := Pos + 1;
                  end if;
               when others =>
                  Current_Token := Tok_Less;
                  Pos := Pos + 1;
            end case;
            return;
         when '>' =>
            case Source (Pos + 1) is
               when '=' =>
                  Current_Token := Tok_Greater_Equal;
                  Pos := Pos + 2;
               when '>' =>
                  Current_Token := Tok_Double_Greater;
                  Pos := Pos + 2;
               when others =>
                  Current_Token := Tok_Greater;
                  Pos := Pos + 1;
            end case;
            return;
         when '=' =>
            if Source (Pos + 1) = '=' then
               if AMS_Vhdl then
                  Current_Token := Tok_Equal_Equal;
               else
                  Error_Msg_Scan
                    ("'==' is not the vhdl equality, replaced by '='");
                  Current_Token := Tok_Equal;
               end if;
               Pos := Pos + 2;
            elsif Source (Pos + 1) = '>' then
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
            elsif Source (Pos + 1) = ''' then
               Error_Msg_Scan ("empty quote is not allowed in vhdl");
               Current_Token := Tok_Character;
               Current_Context.Identifier := Name_Table.Get_Identifier (' ');
               Pos := Pos + 2;
               return;
            else
               Current_Token := Tok_Tick;
               Pos := Pos + 1;
            end if;
            return;
         when '0' .. '9' =>
            Scan_Literal;

            --  LRM93 13.2
            --  At least one separator is required between an identifier or
            --  an abstract literal and an adjacent identifier or abstract
            --  literal.
            case Characters_Kind (Source (Pos)) is
               when Digit =>
                  --  Happen if d#ddd# is followed by a number.
                  Error_Msg_Scan ("space is required between numbers");
               when Upper_Case_Letter
                 | Lower_Case_Letter =>
                  --  Could call Error_Separator, but use a clearer message
                  --  for this common case.
                  --  Note: the term "unit name" is not correct here, since
                  --  it can be any identifier or even a keyword; however it
                  --  is probably the most common case (eg 10ns).
                  if Vhdl_Std >= Vhdl_08 and then Current_Token = Tok_Integer
                  then
                     Current_Token := Tok_Integer_Letter;
                  else
                     Error_Msg_Scan
                       ("space is required between number and unit name");
                  end if;
               when Other_Special_Character =>
                  if Vhdl_Std > Vhdl_87 and then Source (Pos) = '\' then
                     --  Start of extended identifier.
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
            --  Skip.
            Pos := Pos + 1;
            goto Again;
         when '"' =>
            Scan_String;
            return;
         when '%' =>
            if Vhdl_Std >= Vhdl_08 then
               Error_Msg_Scan
                 ("'%%' not allowed in vhdl 2008 (was replacement character)");
               --  Continue as a string.
            end if;
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
            Current_Token := Tok_Left_Curly;
            Pos := Pos + 1;
            return;
         when '}' =>
            Current_Token := Tok_Right_Curly;
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
            if Vhdl_Std >= Vhdl_08 then
               Current_Token := Tok_Caret;
            else
               Current_Token := Tok_Xor;
               Error_Msg_Scan ("'^' is not a VHDL operator, use 'xor'");
            end if;
            Pos := Pos + 1;
            return;
         when '~' =>
            Error_Msg_Scan ("'~' is not a VHDL operator, use 'not'");
            Pos := Pos + 1;
            Current_Token := Tok_Not;
            return;
         when '?' =>
            if Vhdl_Std < Vhdl_08 then
               Error_Bad_Character;
               Pos := Pos + 1;
               goto Again;
            else
               if Source (Pos + 1) = '<' then
                  if Source (Pos + 2) = '=' then
                     Current_Token := Tok_Match_Less_Equal;
                     Pos := Pos + 3;
                  else
                     Current_Token := Tok_Match_Less;
                     Pos := Pos + 2;
                  end if;
               elsif Source (Pos + 1) = '>' then
                  if Source (Pos + 2) = '=' then
                     Current_Token := Tok_Match_Greater_Equal;
                     Pos := Pos + 3;
                  else
                     Current_Token := Tok_Match_Greater;
                     Pos := Pos + 2;
                  end if;
               elsif Source (Pos + 1) = '?' then
                  Current_Token := Tok_Condition;
                  Pos := Pos + 2;
               elsif Source (Pos + 1) = '=' then
                  Current_Token := Tok_Match_Equal;
                  Pos := Pos + 2;
               elsif Source (Pos + 1) = '/'
                 and then Source (Pos + 2) = '='
               then
                  Current_Token := Tok_Match_Not_Equal;
                  Pos := Pos + 3;
               else
                  Current_Token := Tok_Question_Mark;
                  Pos := Pos + 1;
               end if;
            end if;
            return;
         when '`' =>
            if Vhdl_Std >= Vhdl_08 then
               Scan_Tool_Directive;
            else
               Error_Bad_Character;
               Skip_Until_EOL;
            end if;
            goto Again;
         when '$'
           | Inverted_Exclamation .. Inverted_Question
           | Multiplication_Sign | Division_Sign =>
            Error_Bad_Character;
            Pos := Pos + 1;
            goto Again;
         when '@' =>
            if Vhdl_Std >= Vhdl_08 or Flag_Psl then
               Current_Token := Tok_Arobase;
               Pos := Pos + 1;
               return;
            else
               Error_Bad_Character;
               Pos := Pos + 1;
               goto Again;
            end if;
         when '_' =>
            Error_Msg_Scan ("an identifier can't start with '_'");
            Scan_Identifier (Flag_Psl);
            --  Cannot be a reserved word.
            return;
         when 'A' .. 'Z' | 'a' .. 'z' =>
            Scan_Identifier (Flag_Psl);
            if Current_Token = Tok_Identifier then
               Identifier_To_Token;
            end if;
            return;
         when UC_A_Grave .. UC_O_Diaeresis
           | UC_O_Oblique_Stroke .. UC_Icelandic_Thorn
           | LC_German_Sharp_S .. LC_O_Diaeresis
           | LC_O_Oblique_Stroke .. LC_Y_Diaeresis =>
            if Vhdl_Std = Vhdl_87 then
               Error_Msg_Scan
                 ("non 7-bit latin-1 letters are not allowed in vhdl87");
            end if;
            Scan_Identifier (False);
            --  Not a reserved word.
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
      --  Not reachable: all case should use goto Again or return.
   end Scan;

   function Is_Whitespace (C : Character) return Boolean is
   begin
      if C = ' ' then
         return True;
      elsif Vhdl_Std > Vhdl_87 and C = NBSP then
         return True;
      else
         return False;
      end if;
   end Is_Whitespace;
end Vhdl.Scanner;
