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
with Types; use Types;
with Vhdl.Tokens; use Vhdl.Tokens;

package Vhdl.Scanner is
   -- Global variables
   -- The token that was just scanned.
   -- When the token was eaten, you can call invalidate_current_token to
   -- set it to tok_invalid.
   -- Current_token should not be written outside of scan package.
   -- It can be replaced by a function call.
   Current_Token: Token_Type := Tok_Invalid;

   --  Maximal length for identifiers.
   Max_Name_Length : constant Natural := 1024;

   -- Simply set current_token to tok_invalid.
   procedure Invalidate_Current_Token;
   pragma Inline (Invalidate_Current_Token);

   -- When CURRENT_TOKEN is an tok_identifier, tok_char or tok_string,
   -- its name_id can be got via this function.
   function Current_Identifier return Name_Id;
   pragma Inline (Current_Identifier);

   -- Get current string identifier and length.
   function Current_String_Id return String8_Id;
   function Current_String_Length return Nat32;
   pragma Inline (Current_String_Id);
   pragma Inline (Current_String_Length);

   --  When the current token is Tok_Bit_String, return the base ('b', 'o',
   --  'x' or 'd') and the sign ('s', 'u', or ' ' for none).
   function Get_Bit_String_Base return Character;
   function Get_Bit_String_Sign return Character;
   pragma Inline (Get_Bit_String_Base);
   pragma Inline (Get_Bit_String_Sign);

   -- Set Current_identifier to null_identifier.
   -- Can be used to catch bugs.
   procedure Invalidate_Current_Identifier;
   pragma Inline (Invalidate_Current_Identifier);

   -- When CURRENT_TOKEN is tok_integer, returns the value.
   -- When CURRENT_TOKEN is tok_bit_string, returns the log of the base.
   function Current_Iir_Int64 return Int64;
   pragma Inline (Current_Iir_Int64);

   -- When CURRENT_TOKEN is tok_real, it returns the value.
   function Current_Iir_Fp64 return Fp64;
   pragma Inline (Current_Iir_Fp64);

   -- Advances the lexical analyser.  Put a new token into current_token.
   procedure Scan;

   --  Advances the lexical analyzer within a block comment.
   procedure Scan_Block_Comment;

   -- Initialize the scanner with file SOURCE_FILE.
   procedure Set_File (Source_File : Source_File_Entry);

   --  This function can be called just after Set_File to detect UTF BOM
   --  patterns.  It reports an error if a BOM is present and return True.
   --  Silently return False if no error detected.
   function Detect_Encoding_Errors return Boolean;

   procedure Set_Current_Position (Position: Source_Ptr);

   -- Finalize the scanner.
   procedure Close_File;

   --  If true comments are reported as a token.
   Flag_Comment : Boolean := False;

   --  If true newlines are reported as a token.
   Flag_Newline : Boolean := False;

   --  If true also scan PSL tokens.
   Flag_Psl : Boolean := False;

   --  If true handle PSL embedded in comments: '--  psl' is ignored.
   Flag_Psl_Comment : Boolean := False;

   --  If true, handle pragma translate_on/off.
   Flag_Pragma_Comment : Boolean := False;

   --  If true, ignore '--'.  This is automatically set when Flag_Psl_Comment
   --  is true and a starting PSL keyword has been identified.
   --  Must be reset to false by the parser.
   Flag_Scan_In_Comment : Boolean := False;

   --  If true scan for keywords in comments.  Must be enabled if
   --  Flag_Psl_Comment or Flag_Pragma_Comment is true.
   Flag_Comment_Keyword : Boolean := False;

   --  Get the current location, or the location of the current token.
   --  Since a token cannot spread over lines, file and line of the current
   --  token are the same as those of the current position.
   --  The offset is the offset in the current line.
   function Get_Current_Source_File return Source_File_Entry;
   function Get_Current_Line return Natural;
   function Get_Current_Offset return Natural;
   function Get_Position return Source_Ptr;
   function Get_Token_Location return Location_Type;
   function Get_Token_Offset return Natural;
   function Get_Token_Position return Source_Ptr;
   function Get_Token_Length return Int32;

   function Get_Current_Coord return Source_Coord_Type;
   function Get_Token_Coord return Source_Coord_Type;

   --  Return the initial location before the current token (ie before all
   --  the blanks, comments and newlines have been skipped).  Useful for the
   --  location of a missing token.
   function Get_Prev_Location return Location_Type;

   --  Convert (canonicalize) identifier STR.
   --  Upper case letters are converted into lower case.
   --  Lexical checks are performed.
   --  This procedure is not used by Scan, but should be used for identifiers
   --  given in the command line.
   --  Errors are directly reported through error_msg_option, and ERR set.
   --  Also, Vhdl_Std should be set.
   procedure Convert_Identifier (Str : in out String; Err : out Boolean);

   --  Return TRUE iff C is a whitespace.
   --  LRM93 13.2 Lexical elements, separators, and delimiters
   --  A space character (SPACE or NBSP) ...
   function Is_Whitespace (C : Character) return Boolean;
end Vhdl.Scanner;
