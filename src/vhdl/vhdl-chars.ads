--  VHDL characters.
--  Copyright (C) 2026 Tristan Gingold
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

package Vhdl.Chars is

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

   --  Return TRUE iff C is a whitespace.
   --  LRM93 13.2 Lexical elements, separators, and delimiters
   --  A space character (SPACE or NBSP) ...
   function Is_Whitespace (C : Character) return Boolean;

   --  Convert (canonicalize) identifier STR.
   --  Upper case letters are converted into lower case.
   --  Lexical checks are performed.
   --  This procedure is not used by Scan, but should be used for identifiers
   --  given in the command line.
   --  Errors are directly reported through error_msg_option, and ERR set.
   --  Also, Vhdl_Std should be set.
   procedure Convert_Identifier (Str : in out String; Err : out Boolean);
end Vhdl.Chars;
