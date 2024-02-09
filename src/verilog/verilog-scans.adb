--  Verilog tokenizer
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;

with Tables;
with Files_Map; use Files_Map;
with Errorout; use Errorout;
with Name_Table; use Name_Table;
with Std_Names;
with Str_Table;

with Verilog.Bn_Tables; use Verilog.Bn_Tables;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Errors; use Verilog.Errors;

package body Verilog.Scans is
   --  True iff scanning for BSV.
   function Is_BSV return Boolean;
   pragma Inline (Is_BSV);

   --  Maximum length of identifiers or names.
   Max_Name_Length : constant := 512;

   --  Scanner mode.  Verilog is a little bit unregular as some tokens exist
   --  only in specific contexts.
   type Scan_Mode_Type is
     (
      Scan_Mode_Normal,

      --  The previous token was a base ('h, 'o or 'b).  Scan the next token
      --  as a based number.
      Scan_Mode_Hexa,
      Scan_Mode_Dec,
      Scan_Mode_Octal,
      Scan_Mode_Binary
     );

   type Context_Kind is
     (
      Kind_Nil,
      Kind_File,
      Kind_Pp_String_In_File,
      Kind_Macro
     );

   --  If true, the current source description is ignored (by an `ifdef or
   --  an`ifndef).
   Scan_Ignore : Boolean := False;

   --  If true, a macro is being defined.
   Scan_In_Define : Boolean := False;

   type Scan_In_Kind is
     (
      Scan_In_Text,
--      Scan_In_Define2,
      Scan_In_Line_Comment,
      Scan_In_Block_Comment
     );

   Scan_In : Scan_In_Kind := Scan_In_Text;

   type Bool_Array is array (0 .. 31) of Boolean;
   pragma Pack (Bool_Array);

   type Context_Type (Kind : Context_Kind);
   type Context_Acc is access Context_Type;

   type Context_Type (Kind : Context_Kind) is record
      --  Previous element in the stack.
      Prev : Context_Acc;

      --  Saved position and file.
      --  For a file, this is the include location (or 0 for the first file).
      --  For a macro, this is the location of the macro expansion.
      Pos : Source_Ptr;
      File : Source_File_Entry;

      case Kind is
         when Kind_File =>
            --  Length of the file.  This is used to catch EOF embedded in the
            --  file.
            File_Length : Source_Ptr;

            --  Number of the current line.
            Line_Number : Natural;

            --  Position of the start of the line.
            Line_Pos : Source_Ptr;

         when Kind_Macro =>
            --  Macro being expanded.
            Macro : Macro_Acc;

            --  Index in the macro definition tokens array.
            Macro_Idx : Natural;

            --  Tokens for arguments.
            Macro_Args : Macro_Args_Arr_Acc;

            --  Argument being replaced (0 if none).
            Macro_Arg : Natural;

            --  Index of the argument token.
            Macro_Arg_Idx : Natural;
         when Kind_Nil
           | Kind_Pp_String_In_File =>
            null;
      end case;
   end record;

   Current_Kind : Context_Kind := Kind_Nil;
   Scan_Mode : Scan_Mode_Type;

   Source_File : Source_File_Entry;
   Pos : Source_Ptr;
   Token_Pos : Source_Ptr;

   Translate_Off : Boolean;

   Current_Macro : Macro_Acc;

   Current_Context : Context_Acc := null;

   --  Not required to be saved.
   Source : File_Buffer_Acc := null;

   Current_Pp_Str_End : Source_Ptr;
   Current_Pp_Str_Arg : Natural;

   --  Next index in COND_ELSE.
   --  If COND_INDEX is 0, then the text is not in a `ifdef.
   Cond_Index : Natural;

   --  True if an `else was scanned.
   Cond_Else : Bool_Array;

   subtype Macro_Slot is Int32 range 0 .. 511;
   type Macro_Arr is array (Macro_Slot) of Macro_Acc;
   Macros_Table : Macro_Arr := (others => null);

   function Id_To_Macro_Slot (Id : Name_Id) return Macro_Slot is
   begin
      return Macro_Slot (Id mod Macros_Table'Length);
   end Id_To_Macro_Slot;

   function Find_Macro (Id : Name_Id) return Macro_Acc
   is
      Res : Macro_Acc;
   begin
      Res := Macros_Table (Id_To_Macro_Slot (Id));
      loop
         if Res = null then
            return null;
         end if;
         if Res.Id = Id then
            return Res;
         end if;
         Res := Res.Next;
      end loop;
   end Find_Macro;

   function Is_BSV return Boolean is
   begin
      return Language = Language_BSV;
   end Is_BSV;

   function Get_Scan_Coord return Source_Coord_Type
   is
      Coord : Source_Coord_Type;
   begin
      if Current_Context.Kind = Kind_File then
         --  The coord is directly available.  This common case is quickly
         --  handled.
         return (File => Source_File,
                 Line_Pos => Current_Context.Line_Pos,
                 Line => Current_Context.Line_Number,
                 Offset => Natural (Pos - Current_Context.Line_Pos));
      else
         --  Need to compute coord from location.  Slower.
         File_Pos_To_Coord (Source_File, Pos,
                            Coord.Line_Pos, Coord.Line, Coord.Offset);
         Coord.File := Source_File;
         return Coord;
      end if;
   end Get_Scan_Coord;

   --  Return the coord in the current file, ignoring any macro/argument
   --  expansion.
   function Get_Source_Coord return Source_Coord_Type
   is
      Ctxt : Context_Acc;
   begin
      if Current_Context.Kind = Kind_File then
         return Get_Scan_Coord;
      end if;

      Ctxt := Current_Context;

      while Ctxt.Prev.Kind /= Kind_File loop
         Ctxt := Ctxt.Prev;
      end loop;

      return +File_Pos_To_Location (Ctxt.File, Ctxt.Pos);
   end Get_Source_Coord;

   function Get_Token_Location return Location_Type is
   begin
      return File_Pos_To_Location (Source_File, Token_Pos);
   end Get_Token_Location;

   function Get_Token_Width return Natural is
   begin
      return Natural (Pos - Token_Pos);
   end Get_Token_Width;

   function Get_Current_Macro return Macro_Acc is
   begin
      return Current_Macro;
   end Get_Current_Macro;

   procedure Scan_Report_Msg_Context
   is
      Ctxt : Context_Acc;
      Coord : Source_Coord_Type;
   begin
      Ctxt := Current_Context;
      loop
         case Ctxt.Kind is
            when Kind_File =>
               exit when Ctxt.Prev = null;
               Coord := +File_Pos_To_Location (Ctxt.File, Ctxt.Pos);
               Report_Msg (Msgid_Note, Errorout.Scan, Coord,
                           " (included here)", No_Eargs);
            when Kind_Macro =>
               Coord := +File_Pos_To_Location (Ctxt.File, Ctxt.Pos);
               Report_Msg (Msgid_Note, Errorout.Scan, Coord,
                           " (in expansion of macro %i)",
                           (1 => +Ctxt.Macro.Id));
            when Kind_Pp_String_In_File =>
               null;
            when Kind_Nil =>
               raise Program_Error;
         end case;
         Ctxt := Ctxt.Prev;
      end loop;
   end Scan_Report_Msg_Context;

   procedure Error_Msg_Scan (Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Msgid_Error, Errorout.Scan, Get_Scan_Coord, Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Error_Msg_Scan;

   --  Warning at the current location.
   procedure Warning_Msg_Scan
     (Id : Msgid_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Id, Errorout.Scan, Get_Scan_Coord, Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Warning_Msg_Scan;

   --  Warning at a different location (such as token location).
   procedure Warning_Msg_Scan (Id : Msgid_Type;
                               Loc : Location_Type;
                               Msg : String;
                               Args : Earg_Arr := No_Eargs) is
   begin
      Report_Start_Group;
      Report_Msg (Id, Errorout.Scan, +Loc, Msg, Args);
      Scan_Report_Msg_Context;
      Report_End_Group;
   end Warning_Msg_Scan;

   pragma Unreferenced (Warning_Msg_Scan);

   function Pop_Context return Context_Acc
   is
      Ctxt : Context_Acc;
   begin
      Ctxt := Current_Context;
      Source_File := Ctxt.File;
      Pos := Ctxt.Pos;
      Token_Pos := Ctxt.Pos;

      Current_Context := Ctxt.Prev;
      if Current_Context = null then
         Current_Kind := Kind_Nil;
         Source := null;
      else
         Current_Kind := Current_Context.Kind;
         Source := Get_File_Source (Source_File);
      end if;
      return Ctxt;
   end Pop_Context;

   procedure Free_Context is new Ada.Unchecked_Deallocation
     (Name => Context_Acc, Object => Context_Type);

   procedure Push_File (File : Source_File_Entry)
   is
      Ctxt : Context_Acc;
   begin
      --  FILE must be a real file.
      pragma Assert (File /= No_Source_File_Entry);

      Ctxt := new Context_Type'(Kind => Kind_File,
                                Prev => Current_Context,
                                File => Source_File,
                                Pos => Pos,
                                File_Length => Get_File_Length (File),
                                Line_Number => 1,
                                Line_Pos => Source_Ptr_Org);

      Source_File := File;
      Source := Get_File_Source (File);
      Pos := Source'First;

      Current_Kind := Kind_File;
      Current_Context := Ctxt;
      Token_Pos := Pos;
   end Push_File;

   procedure Set_File (File : Source_File_Entry) is
   begin
      --  Can be called only when not in use.
      pragma Assert (Current_Context = null);
      Cond_Index := 0;
      Source_File := No_Source_File_Entry;
      Pos := Source_Ptr_Bad;
      Translate_Off := False;

      Push_File (File);
   end Set_File;

   procedure Close_File
   is
      Ctxt : Context_Acc;
   begin
      Ctxt := Pop_Context;
      Free_Context (Ctxt);
      pragma Assert (Current_Context = null);
   end Close_File;

   function Save_Token return Token_Element
   is
      Res : Token_Element;
   begin
      Res := (Token => Current_Token,
              Spaces => False,
              Loc => Get_Token_Location,
              Data => Token_Data_Type'(Kind => Token_Data_None));

      case Current_Token is
         when Tok_Identifier
           | Tok_System =>
            Res.Data := (Kind => Token_Data_Id,
                         Id => Current_Identifier);
         when Tok_Number_32
           | Tok_Dec_Number =>
            Res.Data := (Kind => Token_Data_Number_32,
                         Number_32 => Current_Number_Lo);
         when Tok_String_Literal =>
            Res.Data := (Kind => Token_Data_String,
                         Str_Id => Current_String,
                         Str_Len => Current_String_Len);
         when Tok_Pp_String_Arg
           | Tok_Pp_String_End =>
            Res.Data := (Kind => Token_Data_Pp_String,
                         Pp_End => Current_Pp_Str_End,
                         Pp_Arg => Current_Pp_Str_Arg);
         when Tok_Pp_Macro =>
            Res.Data := (Kind => Token_Data_Macro,
                         Macro_Id => Current_Identifier,
                         Macro => Current_Macro);
         when others =>
            null;
      end case;

      return Res;
   end Save_Token;

   --  Called when a newline is scanned from a source file to save the position
   --  of the next line.
   procedure Scan_File_Newline is
   begin
      Current_Context.Line_Number := Current_Context.Line_Number + 1;
      Current_Context.Line_Pos := Pos;
      File_Add_Line_Number (Source_File, Current_Context.Line_Number, Pos);
   end Scan_File_Newline;

   procedure Skip_Newline (C : Character) is
   begin
      if (C = LF and then Source (Pos) = CR)
        or else (C = CR and then Source (Pos) = LF)
      then
         Pos := Pos + 1;
      end if;
   end Skip_Newline;

   procedure Current_String_Append (C : Character) is
   begin
      Str_Table.Append_String8_Char (C);
      Current_String_Len := Current_String_Len + 1;
   end Current_String_Append;

   function Scan_File_Eof return Boolean
   is
      Ctxt : Context_Acc;
   begin
      if Pos < Current_Context.File_Length then
         Error_Msg_Scan ("bad character in file");
         return False;
      end if;

      pragma Assert (Current_Kind = Kind_File);

      if Current_Context.Prev = null then
         --  No more context.
         if Cond_Index /= 0 then
            --  FIXME: should display the corresponding `ifdef or `else.
            Error_Msg_Scan ("missing `endif");
         end if;

         Current_Token := Tok_Eof;
         return True;
      else
         --  Pop context.
         Ctxt := Pop_Context;
         Free_Context (Ctxt);

         return False;
      end if;
   end Scan_File_Eof;

   function Scan_Directive_Identifier return Name_Id
   is
      Buffer : String (1 .. Max_Name_Length);
      Length : Natural;
      C : Character;
   begin
      C := Source (Pos);
      case C is
         when 'a' .. 'z'
           | 'A' .. 'Z'
           | '_' =>
            null;
         when others =>
            Error_Msg_Scan
              ("directive or macro should start with a letter or a '_'");
      end case;

      --  scan directive identifier.
      Length := 0;
      loop
         Length := Length + 1;
         Buffer (Length) := C;
         Pos := Pos + 1;
         C := Source (Pos);
         case C is
            when '0' .. '9'
              | 'A' .. 'Z'
              | 'a' .. 'z'
              | '_' =>
               null;
            when others =>
               exit;
         end case;
      end loop;
      return Get_Identifier (Buffer (1 .. Length));
   end Scan_Directive_Identifier;

   procedure Skip_Blanks is
   begin
      while Source (Pos) = ' ' or Source (Pos) = HT loop
         Pos := Pos + 1;
      end loop;
   end Skip_Blanks;

   procedure Scan_Endif;
   procedure Scan_Else;

   procedure Scan_Cond_Disable
   is
      Level : Natural;
   begin
      pragma Assert (not Scan_Ignore);
      Scan_Ignore := True;

      --  Eat tokens until `else or `endif, handle nested `ifdef/`ifndef.
      Level := 0;
      loop
         Scan;
         case Current_Token is
            when Tok_Pp_Endif =>
               if Level = 0 then
                  Scan_Ignore := False;
                  Scan_Endif;
                  return;
               end if;
               Level := Level - 1;
            when Tok_Pp_Ifdef
              | Tok_Pp_Ifndef =>
               Level := Level + 1;
            when Tok_Pp_Else =>
               if Level = 0 then
                  Scan_Else;
                  return;
               else
                  pragma Assert (Scan_Ignore);
                  null;
               end if;
            when Tok_Eof =>
               Error_Msg_Scan ("unexpected end of file during conditionnal");
               Scan_Ignore := False;
               return;
            when others =>
               null;
         end case;
      end loop;
   end Scan_Cond_Disable;

   --  Handle `ifdef/`ifndef.
   --  IS_IFDEF is true for `ifdef, false for `ifndef
   --  Advance to the next token.
   procedure Scan_Ifdef (Is_Ifdef : Boolean)
   is
      Id : Name_Id;
      Mac : Macro_Acc;
   begin
      pragma Assert (Current_Kind = Kind_File or Current_Kind = Kind_Macro);

      if Cond_Index > Cond_Else'Last then
         Error_Msg_Scan ("too many `ifdef/`ifndef nested");
      end if;
      Cond_Else (Cond_Index) := False;
      Cond_Index := Cond_Index + 1;

      --  Scan identifier.
      Scan;

      if Current_Token = Tok_Identifier then
         Id := Current_Identifier;
         Mac := Find_Macro (Id);
      else
         Error_Msg_Scan ("`ifdef/`ifndef must be followed by an identifier");
         Mac := null;
      end if;

      if (Mac = null and Is_Ifdef) or (Mac /= null and not Is_Ifdef) then
         --  Eat until `else or `endif.
         Scan_Cond_Disable;

         --  Skip `else/`endif.
         Scan;
      else
         --  Skip the identifier.
         Scan;
      end if;
   end Scan_Ifdef;

   procedure Scan_Endif is
   begin
      if Cond_Index = 0 then
         Error_Msg_Scan ("`endif without `ifdef/`ifndef");
      else
         Cond_Index := Cond_Index - 1;
      end if;
   end Scan_Endif;

   procedure Scan_Else is
   begin
      if Cond_Index = 0 then
         Error_Msg_Scan ("`else without `ifdef/`ifndef");
      elsif Cond_Else (Cond_Index - 1) then
         Error_Msg_Scan ("redondant `else");
      else
         Cond_Else (Cond_Index - 1) := True;
      end if;
      if Scan_Ignore then
         Scan_Ignore := False;
      else
         Scan_Cond_Disable;
      end if;
   end Scan_Else;

   --  Return True iff L and R are different.  Used to avoid spurious warning
   --  about macro redefinition.
   function Is_Same_Macro (L, R : Macro_Type) return Boolean is
   begin
      if (L.Args = null) /= (R.Args = null) then
         --  One has arguments but the other has not.
         return False;
      end if;

      if L.Toks = null and R.Toks = null then
         --  Both are empty.
         return True;
      end if;

      if L.Toks = null or R.Toks = null then
         --  One has text, but the other has not.
         return False;
      end if;

      if L.Toks'Length /= R.Toks'Length then
         --  Not the same number of tokens.
         return False;
      end if;

      for I in L.Toks'Range loop
         if L.Toks (I).Token /= R.Toks (I).Token then
            return False;
         end if;
         declare
            use Str_Table;
            Ldata : Token_Data_Type renames L.Toks (I).Data;
            Rdata : Token_Data_Type renames R.Toks (I).Data;
         begin
            if Ldata.Kind /= Rdata.Kind then
               return False;
            end if;
            case Ldata.Kind is
               when Token_Data_None =>
                  null;
               when Token_Data_Id =>
                  if Ldata.Id /= Rdata.Id then
                     return False;
                  end if;
               when Token_Data_Number_32 =>
                  if Ldata.Number_32 /= Rdata.Number_32 then
                     return False;
                  end if;
               when Token_Data_String =>
                  if Ldata.Str_Len /= Rdata.Str_Len then
                     return False;
                  end if;
                  for I in 1 .. Nat32 (Ldata.Str_Len) loop
                     if (Element_String8 (Ldata.Str_Id, I)
                           /= Element_String8 (Rdata.Str_Id, I))
                     then
                        return False;
                     end if;
                  end loop;
               when Token_Data_Arg =>
                  if Ldata.Arg_Idx /= Rdata.Arg_Idx then
                     return False;
                  end if;
               when Token_Data_Macro =>
                  if Ldata.Macro /= Rdata.Macro then
                     --  Direct comparaison or recursion ?
                     return False;
                  end if;
               when Token_Data_Pp_String =>
                  --  TODO
                  raise Program_Error;
            end case;
         end;
      end loop;

      return True;
   end Is_Same_Macro;

   --  1800-2017 22.5.1 `define
   --  Scan default text of formal argument.
   function Scan_Macro_Default_Text return Token_Array_Acc
   is
      use Token_Table;
      Toks_Table : Token_Table.Instance;
      Res : Token_Array_Acc;
   begin
      Init (Toks_Table, 128);
      loop
         case Current_Token is
            when Tok_Eol | Tok_Eof =>
               exit;
            when Tok_Comma | Tok_Right_Paren =>
               --  1800-2017 22.5.1 `define
               --  The default text may be explicitly specified to be empty
               --  by adding an = token after the formal argument name,
               --  followed by a comma (or a right parenthesis if it is the
               --  last argument in the argument list).
               exit;
            when Tok_Backslash_Eol =>
               --  1364-2005 19.3.1 `define
               --  The newline preceded by a backslash shall be replaced in
               --  the expanded macro with a newline (but without the
               --  preceding --  backslash character).
               Current_Token := Tok_Eol;
            when others =>
               null;
         end case;

         Append (Toks_Table, Save_Token);

         --  Next token.
         Scan;
      end loop;

      Res := new Token_Element_Array'
        (Toks_Table.Table (1 .. Last (Toks_Table)));
      Free (Toks_Table);

      return Res;
   end Scan_Macro_Default_Text;

   --  Handle `define.
   --  Save tokens.
   procedure Scan_Define
   is
      use Macro_Args_Table;
      use Token_Table;
      Prev_Mode : constant Scan_Mode_Type := Scan_Mode;
      Args_Table : Macro_Args_Table.Instance;
      Toks_Table : Token_Table.Instance;
      Id : Name_Id;
      Arg_Id : Name_Id;
      Default : Token_Array_Acc;
      Mac : Macro_Acc;
      Old : Macro_Type;
   begin
      Skip_Blanks;

      --  Scan macro name.
      Id := Scan_Directive_Identifier;

      if not Scan_Ignore then
         Mac := Find_Macro (Id);
         if Mac /= null then
            Old := Mac.all;
         else
            --  FIXME: detect `define of a directive name.
            Old.Id := Null_Identifier;
            declare
               Slot : constant Macro_Slot := Id_To_Macro_Slot (Id);
            begin
               Mac := new Macro_Type;
               Mac.Id := Id;
               Mac.Next := Macros_Table (Slot);
               Mac.Prev := null;
               if Macros_Table (Slot) /= null then
                  Macros_Table (Slot).Prev := Mac;
               end if;
               Macros_Table (Slot) := Mac;
            end;
         end if;

         --  Save location of the definition.
         Mac.File := Source_File;
         Mac.Pos := Token_Pos;

         Current_Macro := Mac;
      else
         Mac := null;
      end if;

      --  Scan the text of the macro; do not process directives.
      Scan_In_Define := True;
      Scan_Mode := Scan_Mode_Normal;

      if Scan_Ignore then
         loop
            --  Next token.
            Scan;

            case Current_Token is
               when Tok_Eol | Tok_Eof =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
      else
         if Source (Pos) = '(' then
            --  Macro has arguments.
            Init (Args_Table, 8);

            --  Skip '('.
            Pos := Pos + 1;
            Scan;

            if Current_Token = Tok_Right_Paren then
               Error_Msg_Scan ("empty list of macro arguments not allowed");
            else
               loop
                  if Current_Token = Tok_Identifier then
                     Arg_Id := Current_Identifier;

                     --  Skip identifier.
                     Scan;
                  else
                     Arg_Id := Null_Identifier;

                     Error_Msg_Scan ("macro argument must be an identifier");
                  end if;

                  if Current_Token = Tok_Equal then
                     --  Skip '='.
                     Scan;
                     Default := Scan_Macro_Default_Text;
                  else
                     Default := null;
                  end if;

                  Append (Args_Table, (Id => Arg_Id, Default => Default));

                  case Current_Token is
                     when Tok_Right_Paren =>
                        exit;
                     when Tok_Comma =>
                        --  Skip ','.
                        Scan;
                     when others =>
                        Error_Msg_Scan
                          ("missing ')' at end of macro arguments");
                        exit;
                  end case;
               end loop;
            end if;

            Mac.Args := new Macro_Args_Array'
              (Args_Table.Table (1 .. Last (Args_Table)));
            Free (Args_Table);
         else
            Mac.Args := null;
         end if;

         --  Eat spaces.
         Skip_Blanks;

         Init (Toks_Table, 128);
         loop
            --  For pp string...
            Current_Macro := Mac;

            --  Next token.
            Scan;

            case Current_Token is
               when Tok_Eol | Tok_Eof =>
                  exit;
               when Tok_Backslash_Eol =>
                  --  1364-2005 19.3.1 `define
                  --  The newline preceded by a backslash shall be replaced in
                  --  the expanded macro with a newline (but without the
                  --  preceding --  backslash character).
                  Current_Token := Tok_Eol;
               when Tok_Identifier =>
                  if Mac.Args /= null then
                     for I in Mac.Args'Range loop
                        if Mac.Args (I).Id = Current_Identifier then
                           Append
                             (Toks_Table,
                              Token_Element'(Token => Tok_Pp_Arg,
                                             Spaces => False,
                                             Loc => Get_Token_Location,
                                             Data => (Kind => Token_Data_Arg,
                                                      Arg_Idx => I)));
                           goto Done;
                        end if;
                     end loop;
                  end if;
               when Tok_Pp_Concat =>
                  if Last (Toks_Table) < Token_Table.First then
                     Error_Msg_Scan ("first `` is ignored");
                     goto Done;
                  end if;
                  --  Insert Tok_Pp_Concat before the previous token.
                  Append (Toks_Table, Toks_Table.Table (Last (Toks_Table)));
                  Toks_Table.Table (Last (Toks_Table) - 1) :=
                    Token_Element'(Token => Tok_Pp_Concat,
                                   Spaces => False,
                                   Loc => Get_Token_Location,
                                   Data => (Kind => Token_Data_None));
                  goto Done;
               when others =>
                  null;
            end case;

            Append (Toks_Table, Save_Token);
            << Done >> null;
         end loop;

         if Last (Toks_Table) >= Token_Table.First then
            Mac.Toks := new Token_Element_Array'
              (Toks_Table.Table (1 .. Last (Toks_Table)));
         else
            Mac.Toks := null;
         end if;
         Free (Toks_Table);

         --  Free the old macro (in case of redefinition).
         if Old.Id /= Null_Identifier then
            if not Is_Same_Macro (Old, Mac.all) then
               Warning_Msg_Scan
                 (Msgid_Warning, "redefinition of macro %i", (1 => +Id));
            end if;
            Free (Old.Toks);
            Free (Old.Args);
         end if;

         Current_Macro := Mac;
      end if;

      Scan_In_Define := False;
      Scan_Mode := Prev_Mode;
   end Scan_Define;

   procedure Scan_Undef
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Name => Macro_Acc, Object => Macro_Type);
      Id : Name_Id;
      Slot : Macro_Slot;
      Mac : Macro_Acc;
   begin
      pragma Assert (not Scan_Ignore);

      Skip_Blanks;

      --  Scan macro name.
      Id := Scan_Directive_Identifier;

      Slot := Id_To_Macro_Slot (Id);
      Mac := Macros_Table (Slot);
      loop
         if Mac = null then
            --  1364-2005 19.3.2 `undef
            --  An attempt to undefine a text macro that was not previously
            --  defined using a `define compiler directive can result in a
            --  warning.
            Warning_Msg_Scan
              (Msgid_Warning, "macro %i is not defined", (1 => +Id));
            exit;
         end if;
         if Mac.Id = Id then
            if Mac.Prev /= null then
               Mac.Prev.Next := Mac.Next;
            else
               Macros_Table (Slot) := Mac.Next;
            end if;
            if Mac.Next /= null then
               Mac.Next.Prev := Mac.Prev;
            end if;

            --  FIXME: add a reference counter in case of late-binding use.
            --  FIXME: free token streams.
            if False then
               Free (Mac);
            end if;

            exit;
         end if;
         Mac := Mac.Next;
      end loop;
   end Scan_Undef;

   Old_Macro_Contexts : Context_Acc := null;

   procedure Scan_From_Macro;

   procedure Save_Macro_Arg (Mac : Macro_Acc;
                             Args : Macro_Args_Arr_Acc;
                             Argn : in out Natural;
                             Toks_Table : in out Token_Table.Instance)
   is
      use Token_Table;
   begin
      if Argn > Args'Last then
         --  Emit error message only once per macro.
         if Argn = Args'Last + 1 then
            Error_Msg_Scan ("extra macro arguments ignored");
         end if;
      else
         if Last (Toks_Table) < Token_Table.First then
            --  No token, use default.
            if Mac.Args (Argn).Default = null then
               Error_Msg_Scan ("no argument for macro parameter %i",
                               (1 => +Mac.Args (Argn).Id));
            end if;
            Args (Argn).Default := Mac.Args (Argn).Default;
         else
            Args (Argn).Default := new Token_Element_Array'
              (Toks_Table.Table (1 .. Last (Toks_Table)));

            Set_Last (Toks_Table, 0);
         end if;
      end if;

      Argn := Argn + 1;
   end Save_Macro_Arg;

   procedure Scan_Macro (Mac : Macro_Acc)
   is
      Args : Macro_Args_Arr_Acc;
      Ctxt : Context_Acc;
   begin
      --  Scan arguments.
      if Mac.Args /= null then
         --  Skip macro identifier
         Scan;

         if Current_Token /= Tok_Left_Paren then
            Error_Msg_Scan ("macro used without arguments (ignored)");
            return;
         end if;

         declare
            use Token_Table;
            Toks_Table : Token_Table.Instance;
            Level : Natural;
            Argn : Natural;
         begin
            Args := new Macro_Args_Array (1 .. Mac.Args'Last);
            Init (Toks_Table, 128);

            Level := 0;
            Argn := 1;

            loop
               Scan;

               case Current_Token is
                  when Tok_Right_Paren =>
                     if Level = 0 then
                        Save_Macro_Arg (Mac, Args, Argn, Toks_Table);
                        exit;
                     else
                        Level := Level - 1;
                     end if;
                  when Tok_Comma =>
                     if Level = 0 then
                        Save_Macro_Arg (Mac, Args, Argn, Toks_Table);
                        goto Skip;
                     end if;
                  when Tok_Left_Paren | Tok_Left_Brack | Tok_Left_Curly =>
                     Level := Level + 1;
                  when Tok_Right_Brack | Tok_Right_Curly =>
                     if Level > 0 then
                        Level := Level - 1;
                     else
                        Error_Msg_Scan
                          ("un-paired ']' or '}' in macro argument");
                     end if;
                  when Tok_Eof =>
                     Error_Msg_Scan ("premature EOF in macro argument");
                     exit;
                  when others =>
                     null;
               end case;
               Append (Toks_Table, Save_Token);

               << Skip >> null;
            end loop;

            --  Remaning default arguments (if any).
            while Argn <= Args'Last loop
               Save_Macro_Arg (Mac, Args, Argn, Toks_Table);
            end loop;

            Free (Toks_Table);
         end;
      else
         Args := null;
      end if;

      if Mac.Toks = null then
         --  Macro is empty.
         --  But Scan_Macro must always set the next token.  So scan one token.

         if Args /= null then
            --  Skip ')'.
            Scan;

            --  FIXME: free args.
         else
            --  Skip macro identifier.
            Scan;
         end if;

         return;
      end if;

      --  FIXME: catch recursive macro expansion.

      --  Get a macro context.
      if Old_Macro_Contexts /= null then
         Ctxt := Old_Macro_Contexts;
         Old_Macro_Contexts := Ctxt.Prev;
      else
         Ctxt := new Context_Type (Kind_Macro);
      end if;

      Ctxt.all := Context_Type'(Kind => Kind_Macro,
                                Prev => Current_Context,
                                Pos => Pos,
                                File => Source_File,
                                Macro => Mac,
                                Macro_Idx => 0,
                                Macro_Args => Args,
                                Macro_Arg => 0,
                                Macro_Arg_Idx => 0);
      Current_Context := Ctxt;
      Current_Kind := Kind_Macro;
      Source_File := Mac.File;
      Pos := Mac.Pos;
      Token_Pos := Mac.Pos;
      Source := null; -- Get_File_Source (Source_File);

      Scan_From_Macro;
   end Scan_Macro;

   procedure Scan_File_Macro is
   begin
      if Scan_Ignore or Scan_In_Define then
         Current_Token := Tok_Pp_File;
      else
         declare
            Coord : constant Source_Coord_Type := Get_Source_Coord;
            Filename : constant String :=
              Name_Table.Image (Get_File_Name (Coord.File));
         begin
            --  TODO: create the string only once.
            Current_Token := Tok_String_Literal;
            Current_String := Str_Table.Create_String8;
            Current_String_Len := Filename'Length;
            for I in Filename'Range loop
               Str_Table.Append_String8_Char (Filename (I));
            end loop;
         end;
      end if;
   end Scan_File_Macro;

   procedure Scan_Line_Macro
   is
      Coord : Source_Coord_Type;
   begin
      if Scan_Ignore or Scan_In_Define then
         Current_Token := Tok_Pp_Line;
      else
         Coord := Get_Source_Coord;
         Current_Token := Tok_Dec_Number;
         Current_Number_Lo := (Val => Uns32 (Coord.Line), Zx => 0);
         Current_Number_Hi := (0, 0);
      end if;
   end Scan_Line_Macro;

   procedure Expand_Pp_String_Token (T : Token_Element) is
   begin
      case T.Token is
         when Tok_Identifier =>
            declare
               Len : constant Natural := Get_Name_Length (T.Data.Id);
               Ptr : constant Thin_String_Ptr := Get_Name_Ptr (T.Data.Id);
            begin
               for I in 1 .. Len loop
                  Str_Table.Append_String8_Char (Ptr (I));
               end loop;
               Current_String_Len := Current_String_Len + Len;
            end;
         when Toks_Keyword
           | Tok_Comma | Tok_Left_Paren | Tok_Right_Paren
           | Tok_Left_Brack | Tok_Right_Brack
           | Tok_Bit_And =>
            declare
               S : constant String := Tokens.Image (T.Token);
            begin
               for I in S'Range loop
                  Str_Table.Append_String8_Char (S (I));
               end loop;
               Current_String_Len := Current_String_Len + S'Length;
            end;
         when others =>
            Error_Msg_Scan ("cannot expand token %t in `"" string",
                            (1 => +T.Token));
      end case;
   end Expand_Pp_String_Token;

   --  Build a string from Tok_Pp_String* tokens in a macro.
   procedure Expand_Pp_String
   is
      Ctxt : constant Context_Acc := Current_Context;
      Mac : constant Macro_Acc := Ctxt.Macro;
      Tok : Token_Type;
      Idx : Natural;
      Sfe : Source_File_Entry;

      P : Source_Ptr;
      Epos : Source_Ptr;
      Source : File_Buffer_Acc;
      C : Character;
   begin
      Idx := Ctxt.Macro_Idx;

      Sfe := Location_To_File (Mac.Toks (Idx).Loc);
      Source := Get_File_Source (Sfe);

      Current_String := Str_Table.Create_String8;
      Current_String_Len := 0;

      loop
         Idx := Idx + 1;
         Tok := Mac.Toks (Idx).Token;

         if Tok = Tok_Pp_String_Arg or Tok = Tok_Pp_String_End then
            P := Location_File_To_Pos (Mac.Toks (Idx).Loc, Sfe);
            Epos := Mac.Toks (Idx).Data.Pp_End;

            while P <= Epos loop
               C := Source (P);
               if C = '`' then
                  pragma Assert (P + 1 <= Epos);
                  if Source (P + 1) = '`' then
                     --  Use of `` (concatenation) in pp-string is simply to
                     --  separate tokens and has no action.
                     P := P + 2;
                  else
                     pragma Assert (P + 3 <= Epos);
                     pragma Assert (Source (P + 1) = '\');
                     pragma Assert (Source (P + 2) = '`');
                     pragma Assert (Source (P + 3) = '"');
                     Current_String_Append ('"');
                     P := P + 4;
                  end if;
               else
                  Current_String_Append (C);
                  P := P + 1;
               end if;
            end loop;
         end if;

         case Tok is
            when Tok_Pp_String_Arg =>
               declare
                  Arg_Idx : constant Natural := Mac.Toks (Idx).Data.Pp_Arg;
                  Args : constant Token_Array_Acc :=
                    Ctxt.Macro_Args (Arg_Idx).Default;
               begin
                  if Args = null then
                     null;
                  else
                     for I in Args'Range loop
                        if I /= Args'First then
                           Current_String_Append (' ');
                        end if;
                        Expand_Pp_String_Token (Args (I));
                     end loop;
                  end if;
               end;
            when Tok_Pp_String_End =>
               exit;
            when others =>
               raise Internal_Error;
         end case;
      end loop;

      Str_Table.Append_String8_Char (NUL);
      Current_Token := Tok_String_Literal;

      --  Next token is the one after Tok_Pp_String_End.
      Ctxt.Macro_Idx := Idx;
   end Expand_Pp_String;

   --  Append name ID to the result of a concatenation.
   --  Subroutine of Expand_Pp_Concat.
   procedure Expand_Pp_Concat_Identifier (Buffer : in out String;
                                          Len : in out Natural;
                                          Id : Name_Id)
   is
      Ilen : constant Natural := Get_Name_Length (Id);
      Ptr : constant Thin_String_Ptr := Get_Name_Ptr (Id);
   begin
      if Len + Ilen > Buffer'Last then
         Error_Msg_Scan ("concatenated identifier is too long");
         return;
      end if;
      for I in 1 .. Ilen loop
         Buffer (Len + I) := Ptr (I);
      end loop;
      Len := Len + Ilen;
   end Expand_Pp_Concat_Identifier;

   --  Append token T to the result of a concatenation.
   --  Subroutine of Expand_Pp_Concat.
   procedure Expand_Pp_Concat_Token (Buffer : in out String;
                                     Len : in out Natural;
                                     T : Token_Element) is
   begin
      case T.Token is
         when Tok_Identifier =>
            Expand_Pp_Concat_Identifier (Buffer, Len, T.Data.Id);
         when Tok_Pp_Macro =>
            Expand_Pp_Concat_Identifier (Buffer, Len, T.Data.Macro_Id);
         when Toks_Keyword =>
            declare
               S : constant String := Image (T.Token);
            begin
               if Len + S'Length > Buffer'Last then
                  Error_Msg_Scan ("concatenated keyword is too long");
                  return;
               end if;
               for I in S'Range loop
                  Buffer (Len + I - S'First + 1) := S (I);
               end loop;
               Len := Len + S'Length;
            end;
         when others =>
            Error_Msg_Scan ("unhandled token %t in concatenation",
                            (1 => +T.Token));
      end case;
   end Expand_Pp_Concat_Token;

   --  Expand concatenation within a macro.
   procedure Expand_Pp_Concat
   is
      Ctxt : constant Context_Acc := Current_Context;
      Mac : constant Macro_Acc := Ctxt.Macro;
      First_Tok : Token_Type;
      Idx : Natural;

      Buffer : String (1 .. 256);
      Len : Natural;
      Todo : Natural;
   begin
      Idx := Ctxt.Macro_Idx;
      Len := 0;

      --  The resulting token is defined by the first token.
      First_Tok := Mac.Toks (Idx + 1).Token;

      --  Although the concatenation token `` appears between the two tokens
      --  that must be concatenated, it is stored in the macro definition in
      --  front of the two tokens, so that we know concatenation has to be
      --  performed without doing a lookup.

      --  Concatenate the next 2 tokens, unless the second one is also a
      --  concatenation.
      Todo := 2;

      while Todo > 0 loop
         --  Skip Tok_Pp_Concat
         Idx := Idx + 1;

         if Mac.Toks (Idx).Token = Tok_Pp_Arg then
            --  If an argument has to be concatenated, add the tokens from the
            --  argument.  Not sure how it is defined.
            declare
               Arg_Idx : constant Natural := Mac.Toks (Idx).Data.Arg_Idx;
               Args : constant Token_Array_Acc :=
                 Ctxt.Macro_Args (Arg_Idx).Default;
            begin
               if Args /= null then
                  for I in Args'Range loop
                     Expand_Pp_Concat_Token (Buffer, Len, Args (I));
                  end loop;
               end if;
            end;
         else
            --  Concatenate the token.
            Expand_Pp_Concat_Token (Buffer, Len, Mac.Toks (Idx));
         end if;

         --  Continue if next token is ``.
         if Idx < Mac.Toks'Last
           and then Mac.Toks (Idx + 1).Token = Tok_Pp_Concat
         then
            Idx := Idx + 1;
         else
            Todo := Todo - 1;
         end if;
      end loop;

      --  Set next token.
      Ctxt.Macro_Idx := Idx;

      --  So BUFFER contains the result of the concatenation, it must now be
      --  turned into a token.  Simply use the first token to deduce the nature
      --  of the concatenation.
      --  TODO: what if the result is a keyword ?
      case First_Tok is
         when Tok_Identifier
           | Tok_Pp_Arg =>
            Current_Identifier := Get_Identifier (Buffer (1 .. Len));
            Current_Token := Tok_Identifier;
         when Tok_Pp_Macro =>
            --  Macro-expansion of a concatenation.
            declare
               Mac : Macro_Acc;
            begin
               Current_Identifier := Get_Identifier (Buffer (1 .. Len));
               Mac := Find_Macro (Current_Identifier);
               if Mac = null then
                  Error_Msg_Scan
                    ("macro %i is not defined", (1 => +Current_Identifier));
               else
                  Scan_Macro (Mac);
               end if;
            end;
         when others =>
            Error_Msg_Scan ("unhandled: concatenation of %t",
                            (1 => +First_Tok));
            Current_Token := Tok_Identifier;
            Current_Identifier := Std_Names.Name_Name;
      end case;
   end Expand_Pp_Concat;

   procedure Expand_Pp_Macro (Mac : Macro_Acc; Id : Name_Id) is
   begin
      if Mac /= null then
         Scan_Macro (Mac);
      else
         Error_Msg_Scan ("macro %i is not defined", (1 => +Id));
         --  Next token
         Scan_From_Macro;
      end if;
   end Expand_Pp_Macro;

   procedure Expand_Pp_Macro_In_Macro (Data : Token_Data_Type)
   is
      Mac : Macro_Acc;
   begin
      --  Early binding: use the macro defined at the use
      --  point (if any).
      Mac := Data.Macro;

      if Mac = null then
         --  Late binding: use macro currently defined.
         Mac := Find_Macro (Data.Macro_Id);
      end if;

      Expand_Pp_Macro (Mac, Data.Macro_Id);
   end Expand_Pp_Macro_In_Macro;

   procedure Restore_Token (Tok : Token_Element) is
   begin
      Current_Token := Tok.Token;
      if Current_Token = Tok_New then
         --  The parser uses Current_Identifier for some keywords...
         Current_Identifier := Std_Names.Name_New;
      end if;
      Source_File := Location_To_File (Tok.Loc);
      Pos := Location_File_To_Pos (Tok.Loc, Source_File);
      Token_Pos := Pos;

      case Tok.Data.Kind is
         when Token_Data_None =>
            null;
         when Token_Data_Id =>
            Current_Identifier := Tok.Data.Id;
         when Token_Data_Number_32 =>
            Current_Number_Lo := Tok.Data.Number_32;
         when Token_Data_String =>
            Current_String := Tok.Data.Str_Id;
            Current_String_Len := Tok.Data.Str_Len;
         when Token_Data_Pp_String =>
            Current_Pp_Str_End := Tok.Data.Pp_End;
            Current_Pp_Str_Arg := Tok.Data.Pp_Arg;
         when Token_Data_Macro =>
            if not Scan_Ignore then
               Expand_Pp_Macro_In_Macro (Tok.Data);
            end if;
         when Token_Data_Arg =>
            if not Scan_Ignore then
               raise Internal_Error;
            end if;
      end case;
   end Restore_Token;

   procedure Scan_From_Macro
   is
      Ctxt : constant Context_Acc := Current_Context;
   begin
      if Ctxt.Macro_Arg /= 0 then
         --  Extracting tokens from an argument.

         --  Next token.
         Ctxt.Macro_Arg_Idx := Ctxt.Macro_Arg_Idx + 1;

         if Ctxt.Macro_Arg_Idx <= Ctxt.Macro_Args (Ctxt.Macro_Arg).Default'Last
         then
            Restore_Token
              (Ctxt.Macro_Args (Ctxt.Macro_Arg).Default (Ctxt.Macro_Arg_Idx));
            return;
         else
            --  End of argument.
            Ctxt.Macro_Arg := 0;
         end if;
      end if;

      declare
         Idx : constant Natural := Ctxt.Macro_Idx + 1;
         Mac : constant Macro_Acc := Ctxt.Macro;
      begin
         if Idx > Mac.Toks'Last then
            --  End of macro

            --  Free allocated data in the context.
            if Ctxt.Macro_Args /= null then
               Free (Ctxt.Macro_Args);
            end if;

            --  Free the context (just save it).
            declare
               Old_Ctxt : Context_Acc;
            begin
               Old_Ctxt := Pop_Context;

               Old_Ctxt.Prev := Old_Macro_Contexts;
               Old_Macro_Contexts := Old_Ctxt;
            end;

            --  Next token
            Scan;

            return;
         end if;

         Current_Context.Macro_Idx := Idx;

         --  If tokens are currently ignored (conditional compilation), just
         --  return it.
         if Scan_Ignore then
            Restore_Token (Mac.Toks (Idx));
            return;
         end if;

         case Mac.Toks (Idx).Token is
            when Tok_Pp_Arg =>
               --  Replace by argument.
               Ctxt.Macro_Arg := Mac.Toks (Idx).Data.Arg_Idx;
               Ctxt.Macro_Arg_Idx := 0;

               if Ctxt.Macro_Args (Ctxt.Macro_Arg).Default = null then
                  --  Error case:  missing argument.  Skip it.
                  Ctxt.Macro_Arg := 0;
               end if;

               --  Tail call.
               Scan_From_Macro;

            when Tok_Eol =>
               if Scan_In_Define or Flag_Scan_All then
                  Restore_Token (Mac.Toks (Idx));
               else
                  --  Next token (tail call).
                  Scan_From_Macro;
               end if;

            when Tok_Pp_Macro =>
               if Scan_In_Define then
                  Restore_Token (Mac.Toks (Idx));
               else
                  Expand_Pp_Macro_In_Macro (Mac.Toks (Idx).Data);
               end if;

            when Tok_Pp_Line =>
               Scan_Line_Macro;
            when Tok_Pp_File =>
               Scan_File_Macro;

            when Tok_Pp_String_Start =>
               --  What if Scan_In_Define is set ?
               Expand_Pp_String;

            when Tok_Pp_Concat =>
               --  What if Scan_In_Define is set ?
               Expand_Pp_Concat;

               if Current_Token = Tok_Pp_Macro then
                  declare
                     Mac : Macro_Acc;
                  begin
                     Mac := Find_Macro (Current_Identifier);
                     Expand_Pp_Macro (Mac, Current_Identifier);
                  end;
               end if;

            when Tok_Pp_Ifdef =>
               Scan_Ifdef (True);
            when Tok_Pp_Ifndef =>
               Scan_Ifdef (False);
            when Tok_Pp_Else =>
               if Scan_Ignore then
                  Scan_Else;
               else
                  Scan_Else;
                  Scan_From_Macro;
               end if;
            when Tok_Pp_Endif =>
               if Scan_Ignore then
                  Scan_Endif;
               else
                  Scan_Endif;
                  Scan_From_Macro;
               end if;
            when others =>
               Restore_Token (Mac.Toks (Idx));
         end case;
      end;
   end Scan_From_Macro;

   --  1800-2017 22.8 `default_tdnettype
   --  default_nettype_compiler_directive ::=
   --    `default_nettype default_nettype_value
   --
   --  default_nettype_value ::=
   --      wire | tri | tri0 | tri1 | wand | triand
   --    | wor | trior | trireg | uwire | none
   procedure Scan_Default_Nettype
   is
      use Std_Names;
      Id : Name_Id;
   begin
      Skip_Blanks;

      --  Scan macro name.
      Id := Scan_Directive_Identifier;

      case Id is
         when Name_Wire
           | Name_Tri
           | Name_Tri0
           | Name_Tri1
           | Name_Wand
           | Name_Triand
           | Name_Wor
           | Name_Trior
           | Name_Trireg
           | Name_Uwire =>
            null;
         when Name_None =>
            null;
         when others =>
            Error_Msg_Scan ("uncorrect default_nettype value %i", (1 => +Id));
      end case;
   end Scan_Default_Nettype;

   package Pathes is new Tables
     (Table_Index_Type => Integer,
      Table_Component_Type => Name_Id,
      Table_Low_Bound => 1,
      Table_Initial => 4);

   procedure Init_Pathes is
   begin
      Pathes.Append (Files_Map.Get_Home_Directory);

      Bignum_Table.Init (Bn_Table, 64);
   end Init_Pathes;

   procedure Add_Incdir (Dir : String)
   is
      use GNAT.OS_Lib;
      Id : Name_Id;
   begin
      if Dir'Length = 0 then
         return;
      end if;
      if Dir (Dir'Last) /= Directory_Separator then
         Id := Get_Identifier (Dir & Directory_Separator);
      else
         Id := Get_Identifier (Dir);
      end if;

      --  FIXME: add only if not yet present ?
      Pathes.Append (Id);
   end Add_Incdir;

   --  1800-2017 22.4 `include
   procedure Scan_Include
   is
      Buffer : String (1 .. Max_Name_Length);
      Length : Natural;
      C : Character;
      Fe : Source_File_Entry;
      Id : Name_Id;
      File_Id : Name_Id;
   begin
      Skip_Blanks;

      C := Source (Pos);
      if C /= '"' then
         Error_Msg_Scan ("filename is expected after `include");
      end if;

      Pos := Pos + 1;
      Length := 0;
      loop
         C := Source (Pos);
         exit when C < ' ';
         exit when C = '"';
         Length := Length + 1;
         Buffer (Length) := C;
         Pos := Pos + 1;
      end loop;
      if C /= '"' then
         Error_Msg_Scan ("missing '""' after filename");
      else
         Pos := Pos + 1;
      end if;

      Id := Get_Identifier (Buffer (1 .. Length));
      File_Id := Id;

      --  First, look in current directory.
      Fe := Read_Source_File (Get_Directory_Name (Source_File), File_Id);

      --  Then search in include path.
      if Fe = No_Source_File_Entry then
         for I in Pathes.First .. Pathes.Last loop
            Fe := Read_Source_File (Pathes.Table (I), Id);
            exit when Fe /= No_Source_File_Entry;
         end loop;
      end if;

      if Fe = No_Source_File_Entry then
         Error_Msg_Scan
           ("cannot include file """ & Name_Table.Image (Id) & """");
      else
         Push_File (Fe);
      end if;
   end Scan_Include;

   procedure Scan_Line_Directive is
   begin
      --  TODO.
      loop
         case Source (Pos) is
            when CR | LF =>
               exit;
            when Files_Map.EOT =>
               exit;
            when others =>
               Pos := Pos + 1;
         end case;
      end loop;
   end Scan_Line_Directive;

   function Scan_Pp_String_Find_Arg (Init_Pos : Source_Ptr) return Integer
   is
      Len : constant Natural := Natural (Pos - Init_Pos);
      Args : constant Macro_Args_Arr_Acc := Current_Macro.Args;
      Id : Name_Id;
      Ptr : Thin_String_Ptr;
   begin
      if Args = null then
         return -1;
      end if;
      for I in Args'Range loop
         Id := Args (I).Id;
         if Get_Name_Length (Id) = Len then
            Ptr := Get_Name_Ptr (Id);
            if File_Buffer (Ptr (1 .. Len)) = Source (Init_Pos .. Pos - 1)
            then
               return I;
            end if;
         end if;
      end loop;
      return -1;
   end Scan_Pp_String_Find_Arg;

   --  Scan after '`"'.
   --
   --  1800-2017 22.5.1 `define
   --  An `" overrides the usual lexical meaning of " and indicats that the
   --  expansion shall include the quotation mark, substitution of actual
   --  arguments, and expansions of embedded macros.  This allows string
   --  literals to be constructed from macro arguments.
   procedure Scan_From_Pp_String
   is
      C : Character;
   begin
      Token_Pos := Pos;

      loop
         --  Next character.
         C := Source (Pos);
         Pos := Pos + 1;

         case C is
            when 'A' .. 'Z'
              | 'a' .. 'z'
              | '_' =>
               if not Scan_Ignore then
                  declare
                     Arg_Pos : constant Source_Ptr := Pos - 1;
                     Idx : Integer;
                  begin
                     loop
                        C := Source (Pos);
                        exit when not (C in 'a' .. 'z'
                                         or C in 'A' .. 'Z'
                                         or C in '0' .. '9'
                                         or C = '_'
                                         or C = '$');
                        Pos := Pos + 1;
                     end loop;

                     --  Is it an argument ?
                     Idx := Scan_Pp_String_Find_Arg (Arg_Pos);
                     if Idx >= 0 then
                        Current_Pp_Str_End := Arg_Pos - 1;
                        Current_Pp_Str_Arg := Idx;
                        Current_Token := Tok_Pp_String_Arg;
                     return;
                     end if;
                  end;
               end if;
            when Files_Map.EOT =>
               Error_Msg_Scan ("non terminated string");
               Current_Kind := Kind_File;
               Current_Token := Tok_Pp_String_End;
               return;
            when LF | CR =>
               --  IEEE 1364-2005 2.6 Strings
               --  [...] and contained on a single line.
               Error_Msg_Scan ("multi-line strings are not allowed");
               Skip_Newline (C);
               Scan_File_Newline;
            when ASCII.NUL .. ASCII.ETX
              | ASCII.ENQ .. ASCII.BS
              | ASCII.VT
              | ASCII.FF
              | ASCII.SO .. ASCII.US =>
               --  FIXME: ref ?
               Error_Msg_Scan ("control character not allowed in strings");
               --  Continue as string ?
            when '`' =>
               C := Source (Pos);
               Pos := Pos + 1;
               if C = '"' then
                  Current_Pp_Str_End := Pos - 3;
                  Current_Token := Tok_Pp_String_End;
                  Current_Kind := Kind_File;
                  return;
               elsif C = '`' then
                  null;
               elsif C = '\' then
                  --  Only `\`" is allowed
                  if Source (Pos + 1) = '`' and then Source (Pos + 2) = '"'
                  then
                     Pos := Pos + 2;
                  else
                     Error_Msg_Scan ("`\`"" expected");
                  end if;
               else
                  Error_Msg_Scan ("`"", `\`"" or `` expected");
               end if;
            when others =>
               null;
         end case;
      end loop;
   end Scan_From_Pp_String;

   --  Scan after '`' (a directive or a macro).
   --  Return True if a token has to be returned.
   function Scan_Directive return Boolean
   is
      use Std_Names;
      C : Character;
   begin
      C := Source (Pos);
      case C is
         when 'a' .. 'z'
           | 'A' .. 'Z'
           | '_' =>
            null;
         when '"' =>
            --  Skip '"'.
            Pos := Pos + 1;

            if not Scan_In_Define then
               Error_Msg_Scan ("`"" allowed only in text macro");
            end if;
            Current_Token := Tok_Pp_String_Start;
            Current_Kind := Kind_Pp_String_In_File;
            return True;
         when '`' =>
            --  Skip the second '`'.
            Pos := Pos + 1;

            if not Scan_In_Define then
               Error_Msg_Scan ("`` allowed only in text macro");
            end if;
            Current_Token := Tok_Pp_Concat;
            return True;
         when others =>
            Error_Msg_Scan ("'`' must be immediately followed by a name");
      end case;

      --  scan directive identifier.
      Current_Identifier := Scan_Directive_Identifier;
      case Current_Identifier is
         when Name_Define =>
            if Scan_In_Define then
               Current_Token := Tok_Pp_Define;
               return True;
            else
               Scan_Define;
               if Flag_Scan_All then
                  Current_Token := Tok_Pp_Define;
                  return True;
               end if;
            end if;
         when Name_Ifdef =>
            if Scan_Ignore or Scan_In_Define then
               Current_Token := Tok_Pp_Ifdef;
            else
               Scan_Ifdef (True);
            end if;
            return True;
         when Name_Ifndef =>
            if Scan_Ignore or Scan_In_Define then
               Current_Token := Tok_Pp_Ifndef;
            else
               Scan_Ifdef (False);
            end if;
            return True;
         when Name_Else =>
            if Scan_Ignore or Scan_In_Define then
               Current_Token := Tok_Pp_Else;
               return True;
            else
               Scan_Else;
            end if;
         when Name_Endif =>
            if Scan_Ignore or Scan_In_Define then
               Current_Token := Tok_Pp_Endif;
               return True;
            else
               Scan_Endif;
            end if;
         when Name_Include =>
            if not Scan_Ignore then
               Scan_Include;
            end if;
            if Scan_Ignore or Flag_Scan_All then
               Current_Token := Tok_Pp_Include;
               return True;
            end if;
         when Name_Timescale =>
            Current_Token := Tok_Pp_Timescale;
            return True;
         when Name_Undef =>
            if Scan_Ignore or Scan_In_Define then
               Current_Token := Tok_Pp_Undef;
               return True;
            else
               Scan_Undef;
            end if;
         when Name_Uu_File_Uu =>
            Scan_File_Macro;
            return True;
         when Name_Uu_Line_Uu =>
            Scan_Line_Macro;
            return True;
         when Name_Line =>
            Scan_Line_Directive;

         when Name_Celldefine
           | Name_Endcelldefine =>
            --  Ignore.
            return False;

         when Name_Resetall =>
            return False;

         when Name_Default_Nettype =>
            Scan_Default_Nettype;
            return False;

         when others =>
            if Scan_Ignore then
               return False;
            end if;

            declare
               Mac : Macro_Acc;
            begin
               Mac := Find_Macro (Current_Identifier);

               if Scan_In_Define then
                  Current_Token := Tok_Pp_Macro;
                  Current_Macro := Mac;
                  return True;
               elsif Mac = null then
                  Error_Msg_Scan
                    ("macro %i is not defined", (1 => +Current_Identifier));
                  --  FIXME: scan next token ?
                  return False;
               else
                  Scan_Macro (Mac);
                  return True;
               end if;
            end;
      end case;
      return False;
   end Scan_Directive;

   --  Advance scanner till the first non-space character.
   procedure Skip_Spaces is
   begin
      while Source (Pos) = ' ' or Source (Pos) = HT loop
         Pos := Pos + 1;
      end loop;
   end Skip_Spaces;

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

   --  Scan an identifier within a comment.  Only lower case letters are
   --  allowed.
   procedure Scan_Comment_Identifier (Id : out Name_Id; Create : Boolean)
   is
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
      Token_Pos := Pos;
      Len := 0;
      loop
         C := Source (Pos);
         case C is
            when 'a' .. 'z'
               | 'A' .. 'Z'
               | '_' =>
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

   --  Skip until new_line after translate_on/translate_off.
   procedure Scan_Pragma_EOL (Id : Name_Id) is
   begin
      --  Expect new line.
      Skip_Spaces;

      if not Is_EOL (Source (Pos)) then
         Warning_Msg_Scan
           (Warnid_Pragma, "garbage ignored after '%i'", (1 => +Id));
         loop
            Pos := Pos + 1;
            exit when Is_EOL (Source (Pos));
         end loop;
      end if;
   end Scan_Pragma_EOL;

   procedure Scan_Pragma_Translate_Off is
   begin
      --  'pragma translate_off' has just been scanned.
      Scan_Pragma_EOL (Std_Names.Name_Translate_Off);

      if Translate_Off then
         Warning_Msg_Scan (Warnid_Pragma, "nested 'translate_off' ignored");
         return;
      end if;

      Current_Token := Tok_Translate_Off;
   end Scan_Pragma_Translate_Off;

   procedure Scan_Pragma_Translate_On is
   begin
      --  'pragma translate_off' has just been scanned.
      Scan_Pragma_EOL (Std_Names.Name_Translate_Off);

      if not Translate_Off then
         Warning_Msg_Scan
           (Warnid_Pragma,
            "'translate_on' without coresponding 'translate_off'");
         return;
      end if;

      Current_Token := Tok_Translate_On;
   end Scan_Pragma_Translate_On;

   procedure Scan_Translate_Off
   is
      Prev_Ignore : constant Boolean := Scan_Ignore;
   begin
      pragma Assert (not Translate_Off);
      Translate_Off := True;
      Scan_Ignore := True;

      --  Recursive scan until 'translate_on' is scanned.
      loop
         Scan;
         case Current_Token is
            when Tok_Translate_On =>
               Translate_Off := False;
               exit;
            when Tok_Eof =>
               Warning_Msg_Scan (Warnid_Pragma,
                                 "unterminated 'translate_off'");
               Translate_Off := False;
               exit;
            when others =>
               null;
         end case;
      end loop;

      --  The scanner is now at the EOL of the translate_on or at the EOF.
      --  Continue scanning.
      Scan_Ignore := Prev_Ignore;

      --  Assume tokens skipped were just a comment.  Anyway, the next token
      --  will be scanned.
      Current_Token := Tok_Line_Comment;
   end Scan_Translate_Off;

   --  Handle pragma comment if it is one.
   function Scan_Comment_Pragma return Boolean
   is
      use Std_Names;
      Id : Name_Id;
      Prev_Pos : Source_Ptr;
   begin
      --  A pragma comment starts with 'synthesis'.
      Scan_Comment_Identifier (Id, False);
      if Id /= Std_Names.Name_Synthesis then
         return False;
      end if;

      --  Directly handle translate_on/off.
      Prev_Pos := Pos;
      Scan_Comment_Identifier (Id, True);
      case Id is
         when Null_Identifier =>
            Warning_Msg_Scan
              (Warnid_Pragma, "incomplete pragma directive ignored");
         when Name_Translate =>
            Scan_Comment_Identifier (Id, False);
            case Id is
               when Name_On =>
                  Scan_Pragma_Translate_On;
               when Name_Off =>
                  Scan_Pragma_Translate_Off;
               when others =>
                  Warning_Msg_Scan
                    (Warnid_Pragma,
                     "pragma translate must be followed by 'on' or 'off'");
            end case;
         when Name_Translate_Off
           |  Name_Synthesis_Off =>
            Scan_Pragma_Translate_Off;
         when Name_Translate_On
           |  Name_Synthesis_On =>
            Scan_Pragma_Translate_On;
         when others =>
            --  Not translate on/off, rewind.
            Pos := Prev_Pos;
            return True;
      end case;
      return False;
   end Scan_Comment_Pragma;

   --  Return True if the current token should be returned.
   function Scan_Line_Comment return Boolean
   is
      C : Character;
   begin
      Current_Token := Tok_Line_Comment;

      --  Skip the '//'
      Pos := Pos + 1;

      if Flag_Pragma_Comment then
         if Scan_Comment_Pragma then
            Current_Token := Tok_Pragma_Comment;
            Scan_In := Scan_In_Line_Comment;
            return True;
         end if;
      end if;

      loop
         C := Source (Pos);
         case C is
            when '\' =>
               if Scan_In_Define then
                  C := Source (Pos + 1);
                  if C = CR or C = LF then
                     --  Rollback.  Scanned as line_comment, backslash_eol.
                     Pos := Pos - 1;
                     exit;
                  end if;
               end if;
            when CR | LF =>
               exit;
            when Files_Map.EOT =>
               exit;
            when others =>
               null;
         end case;
         Pos := Pos + 1;
      end loop;

      case Current_Token is
         when Tok_Line_Comment =>
            return Flag_Scan_All;
         when Tok_Translate_Off =>
            Scan_Translate_Off;
            --  Have to scan the next token
            return False;
         when Tok_Translate_On =>
            --  Return the translate_on to the Scan_Translate_Off function.
            return True;
         when others =>
            --  Another pragma.
            return True;
      end case;
   end Scan_Line_Comment;

   procedure Scan_Block_Comment
   is
      C : Character;
   begin
      --  Skip '/*'
      Pos := Pos + 1;

      --  Handle pragma.
      if Flag_Pragma_Comment then
         --  FIXME: This is not complete as it needs to deal with end of
         --   comment.
         if Scan_Comment_Pragma then
            Current_Token := Tok_Pragma_Comment;
            Scan_In := Scan_In_Block_Comment;
            return;
         end if;
      end if;

      loop
         C := Source (Pos);
         Pos := Pos + 1;
         case C is
            when '*' =>
               if Source (Pos) = '/' then
                  Pos := Pos + 1;
                  return;
               end if;
            when CR | LF =>
               Skip_Newline (C);
               Scan_File_Newline;
            when Files_Map.EOT =>
               Error_Msg_Scan ("EOT in block comment");
               return;
            when others =>
               null;
         end case;
      end loop;
   end Scan_Block_Comment;

   --  Verilog-AMS 2.4.0 2.6.2 Real numbers
   --  Scale.
   procedure Scan_Scale_Number
   is
      C : constant Character := Source (Pos + 1);
   begin
      case C is
         when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '$' | '_' =>
            --  Humm, an identifier.
            --  Do not scan as a scale number.
            Current_Token := Tok_Real_Number;
         when others =>
            --  Scan as a scale number
            Pos := Pos + 1;
            Current_Token := Tok_Scale_Number;
      end case;
   end Scan_Scale_Number;

   --  Current character is '.'.
   procedure Scan_Real_Number (V : Uns32)
   is
      Res : Fp64;
      D : Fp64;
      C : Character;
      Is_Neg : Boolean;
      Exp : Integer;
   begin
      Res := Fp64 (V);

      C := Source (Pos);
      if C = '.' then
         D := 0.1;
         Pos := Pos + 1;
         C := Source (Pos);
         if C not in '0' .. '9' then
            Error_Msg_Scan ("digit expected after '.'");
         end if;
         loop
            exit when C not in '0' .. '9';
            Res := Res + D * Fp64 (Character'Pos (C) - Character'Pos ('0'));
            D := D / 10.0;
            Pos := Pos + 1;
            C := Source (Pos);
         end loop;
      end if;
      if C = 'e' or C = 'E' then
         Pos := Pos + 1;
         C := Source (Pos);
         if C = '-' or C = '+' then
            Is_Neg := C = '-';
            Pos := Pos + 1;
            C := Source (Pos);
         else
            Is_Neg := False;
         end if;
         if C not in '0' .. '9' then
            Error_Msg_Scan ("real number exponent requires at leat one digit");
         end if;
         Exp := 0;
         while C in '0' .. '9' loop
            Exp := Exp * 10 + (Character'Pos (C) - Character'Pos ('0'));
            Pos := Pos + 1;
            C := Source (Pos);
         end loop;
         if Is_Neg then
            Res := Res / 10.0 ** Exp;
         else
            Res := Res * 10.0 ** Exp;
         end if;
      elsif Flag_AMS then
         --  See table 2.1 Scaled Symbols and notation
         case C is
            when 'T' =>
               Current_Number_Lo.Val := 12;
            when 'G' =>
               Current_Number_Lo.Val := 9;
            when 'M' =>
               Current_Number_Lo.Val := 6;
            when 'K' | 'k' =>
               Current_Number_Lo.Val := 3;
            when 'm' =>
               Current_Number_Lo.Val := To_Uns32 (-3);
            when 'u' =>
               Current_Number_Lo.Val := To_Uns32 (-6);
            when 'n' =>
               Current_Number_Lo.Val := To_Uns32 (-9);
            when 'p' =>
               Current_Number_Lo.Val := To_Uns32 (-12);
            when 'f' =>
               Current_Number_Lo.Val := To_Uns32 (-15);
            when 'a' =>
               Current_Number_Lo.Val := To_Uns32 (-18);
            when others =>
               Current_Number_Lo.Val := 0;
         end case;
         if Current_Number_Lo.Val /= 0 then
            Current_Real := Res;
            Scan_Scale_Number;
            return;
         end if;
      end if;
      Current_Real := Res;
      Current_Token := Tok_Real_Number;
   end Scan_Real_Number;

   procedure Scan_Big_Decimal_Number (V0 : Uns64)
   is
      Res : Bn_Index;
      V : Uns32;
      C : Character;
      Width : Width_Type;
   begin
      Res := Bignum_Table.Next (Bn_Table);
      Bignum_Table.Append (Bn_Table, (Val => Uns64_Lo (V0), Zx => 0));
      Bignum_Table.Append (Bn_Table, (Val => Uns64_Hi (V0), Zx => 0));
      Width := 64;

      Pos := Pos + 1;
      loop
         C := Source (Pos);
         if C in '0' .. '9' then
            V := Character'Pos (C) - Character'Pos ('0');
            Compute_Mul_Add_Clean
              (To_Logvec_Ptr (Bn_Table.Table (Res)'Address), Width, 10, V);
            if V /= 0 then
               Bignum_Table.Append (Bn_Table, (Val => V, Zx => 0));
               Width := Width + 32;
            end if;
         elsif C = '_' then
            if Source (Pos - 1) = '_' then
               Error_Msg_Scan ("'_' not allowed after a '_' in a number");
            end if;
         elsif C = '.' then
            --  TODO
            raise Program_Error;
         else
            exit;
         end if;
         Pos := Pos + 1;
      end loop;
      if Source (Pos) = '_' then
         Error_Msg_Scan ("trailing '_' not allowed in a number");
         Pos := Pos + 1;
      end if;
      if Width = 64 then
         Current_Number_Lo := Bn_Table.Table (Res);
         Current_Number_Hi := Bn_Table.Table (Res + 1);
         Bignum_Table.Set_Last (Bn_Table, Res - 1);
         Current_Token := Tok_Dec_Number;
      else
         Current_Bignum := Res;
         Current_Number_Len := Natural (Width);
         Current_Token := Tok_Dec_Bignum;
      end if;
   end Scan_Big_Decimal_Number;

   procedure Scan_Unsigned_Number
   is
      V : Uns64;
      C : Character;
      Orig_Pos : Source_Ptr;
   begin
      V := 0;
      Pos := Pos - 1;
      Orig_Pos := Pos;
      loop
         C := Source (Pos);
         if C in '0' .. '9' then
            --  FIXME: handle overflow.
            V := V * 10 + Character'Pos (C) - Character'Pos ('0');
            if Uns64_Hi (V) /= 0 then
               Scan_Big_Decimal_Number (V);
               return;
            end if;
         elsif C = '_' then
            if Pos = Orig_Pos then
               Error_Msg_Scan
                 ("'_' must not be the first character of a number");
            elsif Source (Pos - 1) = '_' then
               Error_Msg_Scan
                 ("'_' not allowed after a '_' in a number");
            end if;
         else
            exit;
         end if;
         Pos := Pos + 1;
      end loop;
      if Source (Pos) = '_' then
         Error_Msg_Scan ("trailing '_' not allowed in a number");
         Pos := Pos + 1;
      end if;
      Current_Number_Lo := (Val => Uns32 (V), Zx => 0);
      Current_Number_Hi := (Val => 0, Zx => 0);
      Current_Token := Tok_Dec_Number;
   end Scan_Unsigned_Number;

   procedure Scan_Number is
   begin
      Scan_Unsigned_Number;

      case Source (Pos) is
         when '.' | 'e' | 'E' =>
            pragma Assert (Current_Token = Tok_Dec_Number);
            Scan_Real_Number (Current_Number_Lo.Val);
         when others =>
            null;
      end case;

      if Flags.Std in Systemverilog_Standard then
         case Source (Pos) is
            when 's' | 'm' | 'u' | 'n' | 'p' | 'f' =>
               if Current_Token = Tok_Dec_Number then
                  Current_Real := Fp64 (Current_Number_Lo.Val);
               end if;
               Current_Token := Tok_Time_Literal;
            when others =>
               null;
         end case;
      end if;
   end Scan_Number;

   procedure Scan_Based_Number_Head is
   begin
      Pos := Pos - 1;
      if Source (Pos) = '_' then
         Error_Msg_Scan ("based number cannot start with a '_'");
         Pos := Pos + 1;
      end if;
   end Scan_Based_Number_Head;

   subtype Base_Natural is Natural range 1 .. 4;
   procedure Scan_Based_Number_Body (Start_Pos : Source_Ptr;
                                     Base_Log : Base_Natural)
   is
      C : Character;
      Va : Uns32;
      Zx : Uns32;
      Shift : Natural;
      Pos1 : Source_Ptr;
--      Bn : Bignum;
      Off : Natural; --  Offset;
      W : Logic_32;
   begin
      Current_Number_Len := 0;
      Off := 0;

      --  Note: POS1 points just after the number.
      Pos1 := Pos;
      Shift := 0;
      W := (0, 0);
      loop
         Pos1 := Pos1 - 1;
         C := Source (Pos1);
         Current_Number_Len := Current_Number_Len + Base_Log;
         case C is
            when '0' .. '9' =>
               Va := Character'Pos (C) - Character'Pos ('0');
               Zx := 0;
            when 'a' .. 'f' =>
               Va := Character'Pos (C) - Character'Pos ('a') + 10;
               Zx := 0;
            when 'A' .. 'F' =>
               Va := Character'Pos (C) - Character'Pos ('A') + 10;
               Zx := 0;
            when 'z' | 'Z' | '?' =>
               Va := 0;
               Zx := (2 ** Base_Log) - 1;
            when 'x' | 'X' =>
               Va := (2 ** Base_Log) - 1;
               Zx := Va;
            when '_' =>
               --  Oops, not a digit.
               Current_Number_Len := Current_Number_Len - Base_Log;

               if Pos1 = Pos then
                  Error_Msg_Scan ("trailing '_' not allowed in a number");
                  exit;
               elsif Source (Pos1 - 1) = '_' then
                  Error_Msg_Scan ("'_' not allowed after a '_' in a number");
               end if;
               goto Next;
            when others =>
               raise Internal_Error;
         end case;
         W.Val := W.Val or Shift_Left (Va, Shift);
         W.Zx := W.Zx or Shift_Left (Zx, Shift);
         Shift := Shift + Base_Log;
         if Shift >= 32 then
            if Off = 0 then
               Current_Number_Lo := W;
            elsif Off = 1 then
               Current_Number_Hi := W;
            elsif Off = 2 then
               --  Create the bignum.
               Current_Bignum := Bignum_Table.Next (Bn_Table);
               Bignum_Table.Append (Bn_Table, Current_Number_Lo);
               Bignum_Table.Append (Bn_Table, Current_Number_Hi);
               Bignum_Table.Append (Bn_Table, W);
            else
               Bignum_Table.Append (Bn_Table, W);
            end if;
            Off := Off + 1;
            Shift := Shift - 32;

            W.Val := Shift_Right (Va, Base_Log - Shift);
            W.Zx := Shift_Right (Zx, Base_Log - Shift);
         end if;
         << Next >> null;
         exit when Pos1 = Start_Pos;
      end loop;
      if Off = 0 then
         Current_Number_Lo := W;
         Current_Token := Tok_Number_32;
      elsif Off = 1 then
         if Shift = 0 then
            Current_Token := Tok_Number_32;
         else
            Current_Number_Hi := W;
            Current_Token := Tok_Number_64;
         end if;
      elsif Off = 2 and Shift = 0 then
         Current_Token := Tok_Number_64;
      else
         if Off = 2 then
            Current_Bignum := Bignum_Table.Next (Bn_Table);
            Bignum_Table.Append (Bn_Table, Current_Number_Lo);
            Bignum_Table.Append (Bn_Table, Current_Number_Hi);
         end if;
         Bignum_Table.Append (Bn_Table, W);
         Current_Token := Tok_Bignum;
      end if;
   end Scan_Based_Number_Body;

   procedure Scan_Hexa_Number
   is
      C : Character;
      Orig_Pos : Source_Ptr;
   begin
      Scan_Based_Number_Head;

      Current_Number_Lo := (0, 0);

      --  Compute length.
      Orig_Pos := Pos;
      loop
         C := Source (Pos);
         case C is
            when '0' .. '9'
              | 'a' .. 'f'
              | 'A' .. 'F'
              | 'x' | 'X'
              | 'z' | 'Z'
              | '?'
              | '_' =>
               null;
            when others =>
               if Pos = Orig_Pos then
                  Error_Msg_Scan ("hexadecimal number expected");
                  Current_Token := Tok_Number_32;
                  return;
               end if;
               exit;
         end case;
         Pos := Pos + 1;
      end loop;

      Scan_Based_Number_Body (Orig_Pos, 4);
   end Scan_Hexa_Number;

   procedure Scan_Octal_Number
   is
      C : Character;
      Orig_Pos : Source_Ptr;
   begin
      Scan_Based_Number_Head;

      Current_Number_Lo := (0, 0);

      --  Compute length.
      Orig_Pos := Pos;
      loop
         C := Source (Pos);
         case C is
            when '0' .. '7'
              | 'x' | 'X'
              | 'z' | 'Z'
              | '?'
              | '_' =>
               null;
            when others =>
               if Pos = Orig_Pos then
                  Error_Msg_Scan ("octal number expected");
                  Current_Token := Tok_Number_32;
                  return;
               end if;
               exit;
         end case;
         Pos := Pos + 1;
      end loop;

      Scan_Based_Number_Body (Orig_Pos, 3);
   end Scan_Octal_Number;

   procedure Scan_Binary_Number
   is
      C : Character;
      Orig_Pos : Source_Ptr;
   begin
      Scan_Based_Number_Head;

      Current_Number_Lo := (0, 0);

      --  Compute length.
      Orig_Pos := Pos;
      loop
         C := Source (Pos);
         case C is
            when '0' .. '1'
              | 'x' | 'X'
              | 'z' | 'Z'
              | '?'
              | '_' =>
               null;
            when others =>
               if Pos = Orig_Pos then
                  Error_Msg_Scan ("binary number expected");
                  Current_Token := Tok_Number_32;
                  return;
               end if;
               exit;
         end case;
         Pos := Pos + 1;
      end loop;

      Scan_Based_Number_Body (Orig_Pos, 1);
   end Scan_Binary_Number;

   procedure Scan_Decimal_Number is
   begin
      Scan_Based_Number_Head;
      Current_Number_Len := 1;

      case Source (Pos) is
         when '0' .. '9' =>
            Pos := Pos + 1;
            Scan_Unsigned_Number;
            return;
         when 'x' | 'X' =>
            Current_Number_Lo := (Val => 1, Zx => 1);
         when 'z' | 'Z' | '?' =>
            Current_Number_Lo := (Val => 0, Zx => 1);
         when others =>
            Current_Number_Lo := (0, 0);
            Error_Msg_Scan ("digit expected after decimal base");
            Current_Token := Tok_Number_32;
            return;
      end case;

      Pos := Pos + 1;
      while Source (Pos) = '_' loop
         Pos := Pos + 1;
      end loop;

      Current_Token := Tok_Number_32;
   end Scan_Decimal_Number;

   --     procedure Scan_Based_Number
--     is
--        C : Character;
--     begin
--        --  Skip spaces.
--        --  FIXME: newlines.
--        loop
--           C := Source (Pos);
--           exit when (C /= ' ' and C /= HT);
--           Pos := Pos + 1;
--        end loop;
--        if C = '_' then
--           Error_Msg_Scan ("based number cannot start with a '_'");
--           Pos := Pos + 1;
--           C := Source (Pos);
--        end if;
--        loop
--           case C is
--              when 'x' | 'X' =>
--                 null;
--              when 'z' | 'Z' | '?' =>
--                 null;
--              when '0' | '1' =>
--                 null;
--              when '2' .. '7' =>
--                 null;
--              when '8' .. '9' =>
--                 null;
--              when 'a' .. 'f' =>
--                 null;
--              when 'A' .. 'F' =>
--                 null;
--              when '_' =>
--                 if Source (Pos - 1) = '_' then
--                    Error_Msg_Scan ("too many '_'");
--                 end if;
--              when others =>
--                 exit;
--           end case;
--           Pos := Pos + 1;
--           C := Source (Pos);
--        end loop;
--     end Scan_Based_Number;

   procedure Scan_String
   is
      C : Character;
   begin
      --  FIXME: Scan_String;
      Current_String := Str_Table.Create_String8;
      Current_String_Len := 0;
      loop
         C := Source (Pos);
         if C = '"' then
            --  Skip the final quote.
            Pos := Pos + 1;
            --  Append a NUL.
            Str_Table.Append_String8_Char (NUL);
            return;
         elsif C < ' ' then
            case C is
               when Files_Map.EOT =>
                  Error_Msg_Scan ("non terminated string");
                  return;
               when HT =>
                  --  Allowed (?)
                  null;
               when LF | CR =>
                  --  IEEE 1364-2005 2.6 Strings
                  --  [...] and contained on a single line.
                  Error_Msg_Scan ("multi-line strings are not allowed");
                  return;
               when others =>
                  --  FIXME: ref ?
                  Error_Msg_Scan ("control character not allowed in strings");
                  --  Continue as string ?
            end case;
         elsif C = '\' then
            --  IEEE 1364-2005 2.6.3 Special characters in strings
            Pos := Pos + 1;
            C := Source (Pos);
            case C is
               when 'n' =>
                  C := LF;
               when 't' =>
                  C := HT;
               when '\' | '"' =>
                  --  As is.
                  null;
               when '0' .. '7' =>
                  declare
                     Val : Natural;
                  begin
                     Val := 0;
                     for I in 1 .. 3 loop
                        Val := Val * 8
                          + Character'Pos (C) - Character'Pos ('0');
                        Pos := Pos + 1;
                        C := Source (Pos);
                        exit when C not in '0' .. '7';
                     end loop;
                     Pos := Pos - 1;
                     if Val > 8#377# then
                        Error_Msg_Scan ("octal character is too large");
                        Val := 8#377#;
                     end if;
                     C := Character'Val (Val);
                  end;
               when CR | LF =>
                  Skip_Newline (C);
                  Scan_File_Newline;
                  goto Continue;
               when others =>
                  Warning_Msg_Scan
                    (Msgid_Warning,
                     "character %i is not special and doesn't need '\'",
                     (1 => +Get_Identifier (C)));
            end case;
         else
            --  Normal case.
            null;
         end if;
         Current_String_Append (C);
         << Continue >> null;
         Pos := Pos + 1;
      end loop;
   end Scan_String;

   procedure Error_Scan_Udp is
   begin
      Error_Msg_Scan ("bad character in udp body");
   end Error_Scan_Udp;

   procedure Scan_Udp
   is
      C : Character;
   begin
      loop
         Token_Pos := Pos;

         C := Source (Pos);
         Pos := Pos + 1;
         case C is
            when Files_Map.EOT =>
               case Current_Kind is
                  when Kind_File =>
                     if Scan_File_Eof then
                        return;
                     end if;
                  when others =>
                     Error_Msg_Scan ("unexpected EOT");
               end case;
            when LF | CR =>
               Skip_Newline (C);
               Scan_File_Newline;
               if Flag_Scan_All then
                  Current_Token := Tok_Eol;
                  return;
               end if;
            when ' ' | HT =>
               null;
            when '`' =>
               if Scan_Directive then
                  return;
               end if;
            when '0' =>
               Current_Token := Tok_Udp_0;
               return;
            when '1' =>
               Current_Token := Tok_Udp_1;
               return;
            when 'x' | 'X' =>
               Current_Token := Tok_Udp_X;
               return;
            when '?' =>
               Current_Token := Tok_Udp_Qm;
               return;
            when 'b' | 'B' =>
               Current_Token := Tok_Udp_B;
               return;
            when 'r' | 'R' =>
               Current_Token := Tok_Udp_R;
               return;
            when 'f' | 'F' =>
               Current_Token := Tok_Udp_F;
               return;
            when 'p' | 'P' =>
               Current_Token := Tok_Udp_P;
               return;
            when 'n' | 'N' =>
               Current_Token := Tok_Udp_N;
               return;
            when '*' =>
               Current_Token := Tok_Udp_Star;
               return;
            when '-' =>
               Current_Token := Tok_Udp_Dash;
               return;
            when '(' =>
               Current_Token := Tok_Left_Paren;
               return;
            when ')' =>
               Current_Token := Tok_Right_Paren;
               return;
            when ':' =>
               Current_Token := Tok_Colon;
               return;
            when ';' =>
               Current_Token := Tok_Semicolon;
               return;
            when '/' =>
               if Source (Pos) = '/' then
                  if Scan_Line_Comment then
                     --  Can this happen ?  Maybe for Flag_Scan_All, but a
                     --  pragma is not expected here.
                     return;
                  end if;
               elsif Source (Pos) = '*' then
                  Scan_Block_Comment;
               else
                  Error_Scan_Udp;
               end if;
            when 'e' =>
               if Source (Pos) = 'n'
                 and then Source (Pos + 1) = 'd'
                 and then Source (Pos + 2) = 't'
                 and then Source (Pos + 3) = 'a'
                 and then Source (Pos + 4) = 'b'
                 and then Source (Pos + 5) = 'l'
                 and then Source (Pos + 6) = 'e'
               then
                  Pos := Pos + 7;
                  Current_Token := Tok_Endtable;
                  return;
               else
                  Error_Scan_Udp;
               end if;
            when others =>
               Error_Scan_Udp;
         end case;
      end loop;
   end Scan_Udp;

   --  Convert an identifier to a token (handle keywords).
   --  FIXME: improve speed (use table, check limit with the standard).
   function Identifier_To_Verilog return Token_Type
   is
      use Std_Names;
   begin
      case Current_Identifier is
         when Name_Always =>
            return Tok_Always;
         when Name_And =>
            return Tok_And;
         when Name_Assign =>
            return Tok_Assign;
         when Name_Begin =>
            return Tok_Begin;
         when Name_Buf =>
            return Tok_Buf;
         when Name_Bufif0 =>
            return Tok_Bufif0;
         when Name_Bufif1 =>
            return Tok_Bufif1;
         when Name_Case =>
            return Tok_Case;
         when Name_Casex =>
            return Tok_Casex;
         when Name_Casez =>
            return Tok_Casez;
         when Name_Cmos =>
            return Tok_Cmos;
         when Name_Deassign =>
            return Tok_Deassign;
         when Name_Default =>
            return Tok_Default;
         when Name_Defparam =>
            return Tok_Defparam;
         when Name_Disable =>
            return Tok_Disable;
         when Name_Edge =>
            return Tok_Edge;
         when Name_Else =>
            return Tok_Else;
         when Name_End =>
            return Tok_End;
         when Name_Endcase =>
            return Tok_Endcase;
         when Name_Endmodule =>
            return Tok_Endmodule;
         when Name_Endfunction =>
            return Tok_Endfunction;
         when Name_Endprimitive =>
            return Tok_Endprimitive;
         when Name_Endspecify =>
            return Tok_Endspecify;
         when Name_Endtable =>
            return Tok_Endtable;
         when Name_Endtask =>
            return Tok_Endtask;
         when Name_Event =>
            return Tok_Event;
         when Name_For =>
            return Tok_For;
         when Name_Force =>
            return Tok_Force;
         when Name_Forever =>
            return Tok_Forever;
         when Name_Fork =>
            return Tok_Fork;
         when Name_Function =>
            return Tok_Function;
         when Name_Highz0 =>
            return Tok_Highz0;
         when Name_Highz1 =>
            return Tok_Highz1;
         when Name_If =>
            return Tok_If;
         when Name_Ifnone =>
            return Tok_Ifnone;
         when Name_Initial =>
            return Tok_Initial;
         when Name_Inout =>
            return Tok_Inout;
         when Name_Input =>
            return Tok_Input;
         when Name_Integer =>
            return Tok_Integer;
         when Name_Join =>
            return Tok_Join;
         when Name_Large =>
            return Tok_Large;
         when Name_Macromodule =>
            return Tok_Macromodule;
         when Name_Medium =>
            return Tok_Medium;
         when Name_Module =>
            return Tok_Module;
         when Name_Nand =>
            return Tok_Nand;
         when Name_Negedge =>
            return Tok_Negedge;
         when Name_Nmos =>
            return Tok_Nmos;
         when Name_Nor =>
            return Tok_Nor;
         when Name_Not =>
            return Tok_Not;
         when Name_Notif0 =>
            return Tok_Notif0;
         when Name_Notif1 =>
            return Tok_Notif1;
         when Name_Or =>
            return Tok_Or;
         when Name_Output =>
            return Tok_Output;
         when Name_Parameter =>
            return Tok_Parameter;
         when Name_Pmos =>
            return Tok_Pmos;
         when Name_Posedge =>
            return Tok_Posedge;
         when Name_Primitive =>
            return Tok_Primitive;
         when Name_Pull0 =>
            return Tok_Pull0;
         when Name_Pull1 =>
            return Tok_Pull1;
         when Name_Pullup =>
            return Tok_Pullup;
         when Name_Pulldown =>
            return Tok_Pulldown;
         when Name_Rcmos =>
            return Tok_Rcmos;
         when Name_Real =>
            return Tok_Real;
         when Name_Realtime =>
            return Tok_Realtime;
         when Name_Reg =>
            return Tok_Reg;
         when Name_Release =>
            return Tok_Release;
         when Name_Repeat =>
            return Tok_Repeat;
         when Name_Rnmos =>
            return Tok_Rnmos;
         when Name_Rpmos =>
            return Tok_Rpmos;
         when Name_Rtran =>
            return Tok_Rtran;
         when Name_Rtranif0 =>
            return Tok_Rtranif0;
         when Name_Rtranif1 =>
            return Tok_Rtranif1;
         when Name_Scalared =>
            return Tok_Scalared;
         when Name_Small =>
            return Tok_Small;
         when Name_Specify =>
            return Tok_Specify;
         when Name_Specparam =>
            return Tok_Specparam;
         when Name_Strong0 =>
            return Tok_Strong0;
         when Name_Strong1 =>
            return Tok_Strong1;
         when Name_Supply0 =>
            return Tok_Supply0;
         when Name_Supply1 =>
            return Tok_Supply1;
         when Name_Tablex =>
            return Tok_Table;
         when Name_Task =>
            return Tok_Task;
         when Name_Time =>
            return Tok_Time;
         when Name_Tran =>
            return Tok_Tran;
         when Name_Tranif0 =>
            return Tok_Tranif0;
         when Name_Tranif1 =>
            return Tok_Tranif1;
         when Name_Tri =>
            return Tok_Tri;
         when Name_Tri0 =>
            return Tok_Tri0;
         when Name_Tri1 =>
            return Tok_Tri1;
         when Name_Triand =>
            return Tok_Triand;
         when Name_Trior =>
            return Tok_Trior;
         when Name_Trireg =>
            return Tok_Trireg;
         when Name_Vectored =>
            return Tok_Vectored;
         when Name_Wait =>
            return Tok_Wait;
         when Name_Wand =>
            return Tok_Wand;
         when Name_Weak0 =>
            return Tok_Weak0;
         when Name_Weak1 =>
            return Tok_Weak1;
         when Name_While =>
            return Tok_While;
         when Name_Wire =>
            return Tok_Wire;
         when Name_Wor =>
            return Tok_Wor;
         when Name_Xnor =>
            return Tok_Xnor;
         when Name_Xor =>
            return Tok_Xor;
         when others =>
            null;
      end case;

      if Keywords_Std >= Verilog_2001 then
         case Current_Identifier is
            when Name_Automatic =>
               return Tok_Automatic;
            when Name_Generate =>
               return Tok_Generate;
            when Name_Endgenerate =>
               return Tok_Endgenerate;
            when Name_Genvar =>
               return Tok_Genvar;
            when Name_Localparam =>
               return Tok_Localparam;
            when Name_Signed =>
               return Tok_Signed;
            when Name_Unsigned =>
               return Tok_Unsigned;

            when others =>
               null;
         end case;
      end if;

      if Keywords_Std >= Verilog_Sv_3_0 then
         case Current_Identifier is
            when Name_Always_Comb =>
               return Tok_Always_Comb;
            when Name_Always_Ff =>
               return Tok_Always_Ff;
            when Name_Always_Latch =>
               return Tok_Always_Latch;
            when Name_Assert =>
               return Tok_Assert;
            when Name_Bit =>
               return Tok_Bit;
            when Name_Break =>
               return Tok_Break;
            when Name_Byte =>
               return Tok_Byte;
            when Name_Const =>
               return Tok_Const;
            when Name_Continue =>
               return Tok_Continue;
            when Name_Do =>
               return Tok_Do;
            when Name_Enum =>
               return Tok_Enum;
            when Name_Export =>
               return Tok_Export;
            when Name_Extern =>
               return Tok_Extern;
            when Name_Iff =>
               return Tok_Iff;
            when Name_Import =>
               return Tok_Import;
            when Name_Int =>
               return Tok_Int;
            when Name_Interface =>
               return Tok_Interface;
            when Name_Endinterface =>
               return Tok_Endinterface;
            when Name_Logic =>
               return Tok_Logic;
            when Name_Longint =>
               return Tok_Longint;
            when Name_Modport =>
               return Tok_Modport;
            when Name_Packed =>
               return Tok_Packed;
            when Name_Return =>
               return Tok_Return;
            when Name_Shortint =>
               return Tok_Shortint;
            when Name_Shortreal =>
               return Tok_Shortreal;
            when Name_Static =>
               return Tok_Static;
            when Name_Struct =>
               return Tok_Struct;
            when Name_Timeprecision =>
               return Tok_Timeprecision;
            when Name_Timeunit =>
               return Tok_Timeunit;
            when Name_Typedef =>
               return Tok_Typedef;
            when Name_Union =>
               return Tok_Union;
            when Name_Var =>
               return Tok_Var;
            when Name_Void =>
               return Tok_Void;

            when Name_Unique =>
               return Tok_Unique;
            when Name_Unique0 =>
               return Tok_Unique0;
            when Name_Priority =>
               return Tok_Priority;
            when others =>
               null;
         end case;
      end if;

      if Keywords_Std >= Verilog_Sv_3_1 then
         case Current_Identifier is
            when Name_Assume =>
               return Tok_Assume;
            when Name_Chandle =>
               return Tok_Chandle;
            when Name_Class =>
               return Tok_Class;
            when Name_Endclass =>
               return Tok_Endclass;
            when Name_Clocking =>
               return Tok_Clocking;
            when Name_Endclocking =>
               return Tok_Endclocking;
            when Name_Constraint =>
               return Tok_Constraint;
            when Name_Context =>
               return Tok_Context;
            when Name_Extends =>
               return Tok_Extends;
            when Name_Foreach =>
               return Tok_Foreach;
            when Name_Final =>
               return Tok_Final;
            when Name_Inside =>
               return Tok_Inside;
            when Name_Join_None =>
               return Tok_Join_None;
            when Name_Join_Any =>
               return Tok_Join_Any;
            when Name_Local =>
               return Tok_Local;
            when Name_New =>
               return Tok_New;
            when Name_Null =>
               return Tok_Null;
            when Name_Package =>
               return Tok_Package;
            when Name_Endpackage =>
               return Tok_Endpackage;
            when Name_Program =>
               return Tok_Program;
            when Name_Endprogram =>
               return Tok_Endprogram;
            when Name_Protected =>
               return Tok_Protected;
            when Name_Property =>
               return Tok_Property;
            when Name_Pure =>
               return Tok_Pure;
            when Name_Rand =>
               return Tok_Rand;
            when Name_Randc =>
               return Tok_Randc;
            when Name_Ref =>
               return Tok_Ref;
            when Name_Super =>
               return Tok_Super;
            when Name_String =>
               return Tok_String;
            when Name_This =>
               return Tok_This;
            when Name_Throughout =>
               return Tok_Throughout;
            when Name_Type =>
               return Tok_Type;
            when Name_Virtual =>
               return Tok_Virtual;
            when Name_With =>
               return Tok_With;
            when others =>
               null;
         end case;
      end if;

      if Keywords_Std >= Verilog_Sv2009 then
         case Current_Identifier is
            when Name_Implies =>
               return Tok_Implies;
            when Name_S_Until =>
               return Tok_S_Until;
            when Name_S_Until_With =>
               return Tok_S_Until_With;
            when Name_Until =>
               return Tok_Until;
            when Name_Until_With =>
               return Tok_Until_With;
            when others =>
               null;
         end case;
      end if;

      if Flag_AMS then
         case Current_Identifier is
            when Name_Analog =>
               return Tok_Analog;
            when Name_Discipline =>
               return Tok_Discipline;
            when Name_Enddiscipline =>
               return Tok_Enddiscipline;
            when Name_Nature =>
               return Tok_Nature;
            when Name_Endnature =>
               return Tok_Endnature;
            when Name_Potential =>
               return Tok_Potential;
            when Name_Flow =>
               return Tok_Flow;
            when Name_Domain =>
               return Tok_Domain;
            when Name_Discrete =>
               return Tok_Discrete;
            when Name_Continuous =>
               return Tok_Continuous;
            when Name_Abstol =>
               return Tok_Abstol;
            when Name_Access =>
               return Tok_Access;
            when Name_Ddt_Nature =>
               return Tok_Ddt_Nature;
            when Name_Idt_Nature =>
               return Tok_Idt_Nature;
            when Name_Branch =>
               return Tok_Branch;
            when Name_From =>
               return Tok_From;
            when Name_Exclude =>
               return Tok_Exclude;
            when Name_Ddt =>
               return Tok_Ddt;
            when Name_Idt =>
               return Tok_Idt;
            when Name_White_Noise =>
               return Tok_White_Noise;
            when Name_Units =>
               return Tok_Units;
            when others =>
               null;
         end case;
      end if;

      return Tok_Identifier;
   end Identifier_To_Verilog;

   procedure Identifier_To_BSV (Name : String)
   is
      use Std_Names;
   begin
      case Current_Identifier is
         when Name_Module =>
            Current_Token := Tok_Module;
         when Name_Endmodule =>
            Current_Token := Tok_Endmodule;
         when Name_Package =>
            Current_Token := Tok_Package;
         when Name_Endpackage =>
            Current_Token := Tok_Endpackage;
         when Name_Interface =>
            Current_Token := Tok_Interface;
         when Name_Endinterface =>
            Current_Token := Tok_Endinterface;
         when Name_Numeric =>
            Current_Token := Tok_Numeric;
         when Name_Type =>
            Current_Token := Tok_Type;
         when Name_Method =>
            Current_Token := Tok_Method;
         when Name_Endmethod =>
            Current_Token := Tok_Endmethod;
         when Name_Bit =>
            Current_Token := Tok_Bit;
         when Name_Rule =>
            Current_Token := Tok_Rule;
         when Name_Endrule =>
            Current_Token := Tok_Endrule;
         when Name_Return =>
            Current_Token := Tok_Return;
         when Name_Import =>
            Current_Token := Tok_Import;
         when Name_If =>
            Current_Token := Tok_If;
         when Name_Else =>
            Current_Token := Tok_Else;
         when Name_Function =>
            Current_Token := Tok_Function;
         when Name_Endfunction =>
            Current_Token := Tok_Endfunction;
         when Name_Parameter =>
            Current_Token := Tok_Parameter;
         when Name_Action =>
            Current_Token := Tok_Action;
         when Name_Endaction =>
            Current_Token := Tok_Endaction;
         when Name_Instance =>
            Current_Token := Tok_Instance;
         when Name_Endinstance =>
            Current_Token := Tok_Endinstance;
         when Name_Begin =>
            Current_Token := Tok_Begin;
         when Name_End =>
            Current_Token := Tok_End;
         when Name_Case =>
            Current_Token := Tok_Case;
         when Name_Endcase =>
            Current_Token := Tok_Endcase;
         when Name_Matches =>
            Current_Token := Tok_Matches;
         when Name_Default =>
            Current_Token := Tok_Default;
         when Name_Tagged =>
            Current_Token := Tok_Tagged;
         when Name_Union =>
            Current_Token := Tok_Union;
         when Name_Struct =>
            Current_Token := Tok_Struct;
         when Name_Enum =>
            Current_Token := Tok_Enum;
         when Name_Typedef =>
            Current_Token := Tok_Typedef;
         when Name_Provisos =>
            Current_Token := Tok_Provisos;
         when Name_Void =>
            Current_Token := Tok_Void;
         when Name_Deriving =>
            Current_Token := Tok_Deriving;
         when Name_For =>
            Current_Token := Tok_For;
         when Name_While =>
            Current_Token := Tok_While;
         when Name_Par =>
            Current_Token := Tok_Par;
         when Name_Endpar =>
            Current_Token := Tok_Endpar;
         when Name_Seq =>
            Current_Token := Tok_Seq;
         when Name_Endseq =>
            Current_Token := Tok_Endseq;
         when Name_Break =>
            Current_Token := Tok_Break;
         when Name_Continue =>
            Current_Token := Tok_Continue;
         when Name_Valueof =>
            Current_Token := Tok_Valueof;
         when Name_uValueof =>
            Current_Token := Tok_Valueof;
         when Name_Repeat =>
            Current_Token := Tok_Repeat;
         when Name_Rules =>
            Current_Token := Tok_Rules;
         when Name_Endrules =>
            Current_Token := Tok_Endrules;
         when Name_Let =>
            Current_Token := Tok_Let;
         when Name_Default_Clock =>
            Current_Token := Tok_Default_Clock;
         when Name_Default_Reset =>
            Current_Token := Tok_Default_Reset;
         when Name_Input_Clock =>
            Current_Token := Tok_Input_Clock;
         when Name_Input_Reset =>
            Current_Token := Tok_Input_Reset;
         when Name_Output_Clock =>
            Current_Token := Tok_Output_Clock;
         when Name_Output_Reset =>
            Current_Token := Tok_Output_Reset;
         when Name_Enable =>
            Current_Token := Tok_Enable;
         when Name_Ready =>
            Current_Token := Tok_Ready;
         when Name_Clocked_By =>
            Current_Token := Tok_Clocked_By;
         when Name_Reset_By =>
            Current_Token := Tok_Reset_By;
         when Name_Ancestor =>
            Current_Token := Tok_Ancestor;
         when Name_Same_Family =>
            Current_Token := Tok_Same_Family;
         when Name_Schedule =>
            Current_Token := Tok_Schedule;
         when Name_uCF =>
            Current_Token := Tok_CF;
         when Name_uSB =>
            Current_Token := Tok_SB;
         when Name_uSBR =>
            Current_Token := Tok_SBR;
         when Name_uC =>
            Current_Token := Tok_C;
         when Name_Path =>
            Current_Token := Tok_Path;
         when Name_Inout =>
            Current_Token := Tok_Inout;
         when Name_Ifc_Inout =>
            Current_Token := Tok_Ifc_Inout;
         when Name_Port =>
            Current_Token := Tok_Port;
         when Name_Typeclass =>
            Current_Token := Tok_Typeclass;
         when Name_Endtypeclass =>
            Current_Token := Tok_Endtypeclass;
         when Name_Dependencies =>
            Current_Token := Tok_Dependencies;
         when Name_Determines =>
            Current_Token := Tok_Determines;
         when others =>
            if Name (Name'First) in 'A' .. 'Z' then
               Current_Token := Tok_Uidentifier;
            else
               Current_Token := Tok_Lidentifier;
            end if;
      end case;
   end Identifier_To_BSV;

   procedure Scan_Identifier
   is
      Buffer : String (1 .. Max_Name_Length);
      Length : Natural;
      C : Character;
   begin
      Current_Dollar_In_Id := False;

      Length := 0;
      C := Source (Pos - 1);
      loop
         Length := Length + 1;
         Buffer (Length) := C;

         C := Source (Pos);
         exit when not (C in 'a' .. 'z'
                          or C in 'A' .. 'Z'
                          or C in '0' .. '9'
                          or C = '_'
                          or C = '$');

         if C = '$' then
            Current_Dollar_In_Id := True;
         end if;

         Pos := Pos + 1;
      end loop;
      if Scan_Ignore then
         --  Do not waste ressources scanning and allocating
         --  the identifier.
         Current_Token := Tok_Identifier;
         return;
      end if;
      Current_Identifier := Name_Table.Get_Identifier (Buffer (1 .. Length));
      case Language is
         when Language_Verilog =>
            Current_Token := Identifier_To_Verilog;
         when Language_BSV =>
            Identifier_To_BSV (Buffer (1 .. Length));
      end case;
   end Scan_Identifier;

   procedure No_Alphanum (Msg : String) is
   begin
      case Source (Pos) is
         when '0' .. '9'
           | 'a' .. 'z'
           | 'A' .. 'Z'
           | '_'
           | '$' =>
            Error_Msg_Scan
              ("alphanum character not allowed after '" & Msg & "'");
         when others =>
            null;
      end case;
   end No_Alphanum;

   procedure Scan_From_Source
   is
      C : Character;
   begin
      loop
         Token_Pos := Pos;

         C := Source (Pos);
         Pos := Pos + 1;
         case C is
            when ASCII.NUL .. ASCII.ETX
              | ASCII.ENQ .. ASCII.BS
              | ASCII.VT
              | ASCII.FF
              | ASCII.SO .. ASCII.US =>
               Error_Msg_Scan ("unexpected control character ^"
                                 & Character'Val (Character'Pos (C) + 64));
            when ASCII.DEL .. Character'Val (255) =>
               Error_Msg_Scan ("unexpected 8 bit character");
            when Files_Map.EOT =>
               if Scan_File_Eof then
                  return;
               end if;
               if Flag_Scan_All then
                  Current_Token := Tok_Pp_Endinclude;
                  return;
               end if;
            when LF | CR =>
               if Scan_In = Scan_In_Line_Comment then
                  Current_Token := Tok_Pragma_End_Comment;
                  Scan_In := Scan_In_Text;
                  return;
               end if;

               Skip_Newline (C);
               Scan_File_Newline;

               if Scan_In_Define or Flag_Scan_All then
                  Current_Token := Tok_Eol;
                  return;
               end if;
            when ' ' | HT =>
               --  Skip spaces.
               null;
            when '!' =>
               if Source (Pos) = '=' then
                  Pos := Pos + 1;
                  if Source (Pos) = '=' then
                     Pos := Pos + 1;
                     Current_Token := Tok_Case_Ne;
                  else
                     Current_Token := Tok_Logic_Ne;
                  end if;
               else
                  Current_Token := Tok_Logic_Neg;
               end if;
               exit;
            when '"' =>
               Scan_String;
               Current_Token := Tok_String_Literal;
               exit;
            when '#' =>
               if Std in Systemverilog_Standard then
                  if Source (Pos) = '#' then
                     Pos := Pos + 1;
                     if Source (Pos) = '[' then
                        Pos := Pos + 1;
                        if Source (Pos) = '+'
                          and then Source (Pos + 1) = ']'
                        then
                           Pos := Pos + 2;
                           Current_Token := Tok_Sharp_Plus_Concat;
                        elsif Source (Pos) = '*'
                          and then Source (Pos + 1) = ']'
                        then
                           Pos := Pos + 2;
                           Current_Token := Tok_Sharp_Star_Concat;
                        else
                           Current_Token := Tok_Sharp_Bracket;
                        end if;
                     else
                        Current_Token := Tok_Sharp_Sharp;
                     end if;
                  elsif Source (Pos) = '-'
                    and then Source (Pos + 1) = '#'
                  then
                     Pos := Pos + 2;
                     Current_Token := Tok_Sharp_Minus_Sharp;
                  elsif Source (Pos) = '='
                    and then Source (Pos + 1) = '#'
                  then
                     Pos := Pos + 2;
                     Current_Token := Tok_Sharp_Equal_Sharp;
                  else
                     Current_Token := Tok_Sharp;
                  end if;
               else
                  Current_Token := Tok_Sharp;
               end if;
               exit;
            when '&' =>
               if Source (Pos) = '&' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Logic_And;
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_And_Asgn;
               else
                  Current_Token := Tok_Bit_And;
               end if;
               exit;
            when ''' =>
               C := Source (Pos);
               Pos := Pos + 1;
               case C is
                  when 'h' | 'H' =>
                     Current_Token := Tok_Base_Hex;
                     Scan_Mode := Scan_Mode_Hexa;
                     return;  --  As Scan_Mode was set.
                  when 'b' | 'B' =>
                     Current_Token := Tok_Base_Bin;
                     Scan_Mode := Scan_Mode_Binary;
                     return;  --  As Scan_Mode was set.
                  when 'o' | 'O' =>
                     Current_Token := Tok_Base_Oct;
                     Scan_Mode := Scan_Mode_Octal;
                     return;  --  As Scan_Mode was set.
                  when 'd' | 'D' =>
                     Current_Token := Tok_Base_Dec;
                     Scan_Mode := Scan_Mode_Dec;
                     return;  --  As Scan_Mode was set.
                  when 's' | 'S' =>
                     C := Source (Pos);
                     Pos := Pos + 1;
                     case C is
                        when 'h' | 'H' =>
                           Current_Token := Tok_Base_Signed_Hex;
                           Scan_Mode := Scan_Mode_Hexa;
                           return;  --  As Scan_Mode was set.
                        when 'b' | 'B' =>
                           Current_Token := Tok_Base_Signed_Bin;
                           Scan_Mode := Scan_Mode_Binary;
                           return;  --  As Scan_Mode was set.
                        when 'o' | 'O' =>
                           Current_Token := Tok_Base_Signed_Oct;
                           Scan_Mode := Scan_Mode_Octal;
                           return;  --  As Scan_Mode was set.
                        when 'd' | 'D' =>
                           Current_Token := Tok_Base_Signed_Dec;
                           Scan_Mode := Scan_Mode_Dec;
                           return;  --  As Scan_Mode was set.
                        when others =>
                           Error_Msg_Scan
                             ("base specification (b, o, h or d) expected"
                                & " after s");
                           Pos := Pos - 1;
                           Current_Token := Tok_Base_Signed_Hex;
                           Scan_Mode := Scan_Mode_Hexa;
                           return;  --  As Scan_Mode was set.
                     end case;
                  when '0' =>
                     if Flags.Std in Systemverilog_Standard or Is_BSV then
                        Current_Token := Tok_Unbased_0;
                        No_Alphanum ("'0");
                        exit;
                     end if;
                  when '1' =>
                     if Flags.Std in Systemverilog_Standard or Is_BSV then
                        Current_Token := Tok_Unbased_1;
                        No_Alphanum ("'1");
                        exit;
                     end if;
                  when 'x' | 'X' =>
                     if Flags.Std in Systemverilog_Standard then
                        Current_Token := Tok_Unbased_X;
                        No_Alphanum ("'X");
                        exit;
                     end if;
                  when 'z' | 'Z' =>
                     if Flags.Std in Systemverilog_Standard then
                        Current_Token := Tok_Unbased_Z;
                        No_Alphanum ("'Z");
                        exit;
                     end if;
                  when '{' =>
                     if Flags.Std in Systemverilog_Standard then
                        Current_Token := Tok_Tick_Curly;
                        exit;
                     end if;
                  when others =>
                     null;
               end case;
               if Flags.Std in Systemverilog_Standard then
                  Pos := Pos - 1;
                  Current_Token := Tok_Tick;
                  exit;
               else
                  Error_Msg_Scan
                    ("base specification (b, o, h or d) expected");
                  Pos := Pos - 1;
                  Current_Token := Tok_Base_Hex;
                  Scan_Mode := Scan_Mode_Hexa;
                  return;  --  As Scan_Mode was set.
               end if;
            when '(' =>
               if Source (Pos) = '*' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Paren_Star;
               else
                  Current_Token := Tok_Left_Paren;
               end if;
               exit;
            when ')' =>
               Current_Token := Tok_Right_Paren;
               exit;
            when '*' =>
               if Scan_In = Scan_In_Block_Comment
                 and then Source (Pos + 1) = '/'
               then
                  Current_Token := Tok_Pragma_End_Comment;
                  Scan_In := Scan_In_Text;
                  return;
               end if;

               Current_Token := Tok_Star;
               case Source (Pos) is
                  when '>' =>
                     Pos := Pos + 1;
                     Current_Token := Tok_Full_Conn;
                  when ')' =>
                     Pos := Pos + 1;
                     Current_Token := Tok_Star_Paren;
                  when '=' =>
                     if Std in Systemverilog_Standard then
                        Pos := Pos + 1;
                        Current_Token := Tok_Mul_Asgn;
                     end if;
                  when '*' =>
                     --  Verilog 2001.
                     Pos := Pos + 1;
                     Current_Token := Tok_Star_Star;
                  when others =>
                     null;
               end case;
               exit;
            when '+' =>
               if Source (Pos) = ':' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Plus_Colon;
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '+'
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Plus_Plus;
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Plus_Asgn;
               else
                  Current_Token := Tok_Plus;
               end if;
               exit;
            when '%' =>
               if Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Mod_Asgn;
               else
                  Current_Token := Tok_Modulo;
               end if;
               exit;
            when ',' =>
               Current_Token := Tok_Comma;
               exit;
            when '-' =>
               if Source (Pos) = ':' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Minus_Colon;
               elsif Source (Pos) = '>' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Trigger;
               elsif Std in Systemverilog_Standard
                 and then Source (Pos) = '-'
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Minus_Minus;
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Minus_Asgn;
               else
                  Current_Token := Tok_Minus;
               end if;
               exit;
            when '.' =>
               if Std in Systemverilog_Standard
                 and Source (Pos) = '*'
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Dot_Star;
               else
                  Current_Token := Tok_Dot;
               end if;
               exit;
            when '0' .. '9' =>
               case Scan_Mode is
                  when Scan_Mode_Normal =>
                     Scan_Number;
                  when Scan_Mode_Dec =>
                     Scan_Decimal_Number;
                  when Scan_Mode_Hexa =>
                     Scan_Hexa_Number;
                  when Scan_Mode_Octal =>
                     Scan_Octal_Number;
                  when Scan_Mode_Binary =>
                     Scan_Binary_Number;
               end case;
               exit;
            when ':' =>
               if Source (Pos) = ':' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Colon_Colon;
               else
                  Current_Token := Tok_Colon;
               end if;
               exit;
            when ';' =>
               Current_Token := Tok_Semicolon;
               exit;
            when '<' =>
               Current_Token := Tok_Less;
               case Source (Pos) is
                  when '=' =>
                     Pos := Pos + 1;
                     Current_Token := Tok_Less_Equal;
                  when '<' =>
                     Pos := Pos + 1;
                     if Source (Pos) = '<' then
                        --  Verilog 2001.
                        Pos := Pos + 1;
                        Current_Token := Tok_Left_Ashift;
                     elsif (Std in Systemverilog_Standard)
                       and then Source (Pos) = '='
                     then
                        Pos := Pos + 1;
                        Current_Token := Tok_Shl_Asgn;
                     else
                        Current_Token := Tok_Left_Lshift;
                     end if;
                  when '+' =>
                     if Flag_AMS then
                        Pos := Pos + 1;
                        Current_Token := Tok_Less_Plus;
                     end if;
                  when '-' =>
                     if Is_BSV then
                        Pos := Pos + 1;
                        Current_Token := Tok_Less_Minus;
                     end if;
                  when others =>
                     null;
               end case;
               exit;
            when '=' =>
               Current_Token := Tok_Equal;
               case Source (Pos) is
                  when '=' =>
                     Pos := Pos + 1;
                     if Source (Pos) = '=' then
                        Pos := Pos + 1;
                        Current_Token := Tok_Case_Eq;
                     else
                        Current_Token := Tok_Logic_Eq;
                     end if;
                  when '>' =>
                     Pos := Pos + 1;
                     Current_Token := Tok_Par_Conn;
                  when others =>
                     null;
               end case;
               exit;
            when '>' =>
               Current_Token := Tok_Greater;
               case Source (Pos) is
                  when '=' =>
                     Pos := Pos + 1;
                     Current_Token := Tok_Greater_Equal;
                  when '>' =>
                     Pos := Pos + 1;
                     if Source (Pos) = '>' then
                        --  Verilog 2001.
                        Pos := Pos + 1;
                        Current_Token := Tok_Right_Ashift;
                     elsif Std in Systemverilog_Standard
                       and then Source (Pos) = '='
                     then
                        Pos := Pos + 1;
                        Current_Token := Tok_Shr_Asgn;
                     else
                        Current_Token := Tok_Right_Lshift;
                     end if;
                  when others =>
                     null;
               end case;
               exit;
            when '?' =>
               case Scan_Mode is
                  when Scan_Mode_Normal =>
                     Current_Token := Tok_Question;
                  when Scan_Mode_Hexa =>
                     Scan_Hexa_Number;
                  when Scan_Mode_Dec =>
                     Scan_Decimal_Number;
                  when Scan_Mode_Octal =>
                     Scan_Octal_Number;
                  when Scan_Mode_Binary =>
                     Scan_Binary_Number;
               end case;
               exit;
            when '@' =>
               Current_Token := Tok_At;
               exit;
            when '$' =>
               declare
                  Buffer : String (1 .. Max_Name_Length);
                  Length : Natural;
               begin
                  Length := 0;
                  loop
                     C := Source (Pos);
                     exit when not (C in 'a' .. 'z'
                                      or C in 'A' .. 'Z'
                                      or C in '0' .. '9'
                                      or C = '_'
                                      or C = '$');
                     Length := Length + 1;
                     Buffer (Length) := C;
                     Pos := Pos + 1;
                  end loop;
                  if Length = 0 then
                     if Std in Systemverilog_Standard then
                        Current_Token := Tok_Dollar;
                     else
                        Error_Msg_Scan ("incorrect system name");
                        Current_Token := Tok_System;
                     end if;
                  else
                     Current_Token := Tok_System;
                  end if;
                  if not Scan_Ignore then
                     --  Do not waste ressources scanning and allocating
                     --  the identifier.
                     Current_Identifier :=
                       Get_Identifier (Buffer (1 .. Length));
                  end if;
                  exit;
               end;
            when 'a' .. 'z'
              | 'A' .. 'Z'
              | '_' =>
               case Scan_Mode is
                  when Scan_Mode_Normal =>
                     Scan_Identifier;
                  when Scan_Mode_Dec =>
                     Scan_Decimal_Number;
                  when Scan_Mode_Hexa =>
                     Scan_Hexa_Number;
                  when Scan_Mode_Octal =>
                     Scan_Octal_Number;
                  when Scan_Mode_Binary =>
                     Scan_Binary_Number;
               end case;
               exit;
            when '\' =>
               C := Source (Pos);
               Pos := Pos + 1;
               if C = CR or C = LF then
                  Skip_Newline (C);
                  Scan_File_Newline;

                  if Scan_In_Define then
                     Current_Token := Tok_Backslash_Eol;
                     return;
                  end if;

                  Error_Msg_Scan ("continuation line allowed only in macro");
                  --  Skip it silently.

               else
                  --  Extended identifier.
                  declare
                     Buffer : String (1 .. Max_Name_Length);
                     Length : Natural;
                  begin
                     --  Put the initial '\' in the name to have a separate
                     --  namespace from normal identifiers.
                     Length := 1;
                     Buffer (Length) := '\';
                     loop
                        exit when C = ' ' or C = CR or C = LF;
                        if C < ' ' or C > Character'Val (127) then
                           Error_Msg_Scan
                             ("bad character in extended identifier");
                           exit;
                        end if;
                        Length := Length + 1;
                        Buffer (Length) := C;

                        C := Source (Pos);
                        Pos := Pos + 1;
                     end loop;
                     if Length = 1 then
                        Error_Msg_Scan ("empty extended identifier");
                     end if;
                     if not Scan_Ignore then
                        --  Do not waste ressources scanning and allocating
                        --  the identifier if ignoring tokens.
                        Current_Identifier :=
                          Name_Table.Get_Identifier (Buffer (1 .. Length));
                     end if;
                     Current_Token := Tok_Identifier;
                     exit;
                  end;
               end if;
            when '[' =>
               if Std in Systemverilog_Standard then
                  if Source (Pos) = '*' then
                     Pos := Pos + 1;
                     if Source (Pos) = ']' then
                        Pos := Pos + 1;
                        Current_Token := Tok_Brack_Star_Brack;
                     else
                        Current_Token := Tok_Brack_Star;
                     end if;
                  elsif Source (Pos) = '+'
                    and then Source (Pos + 1) = ']'
                  then
                     Pos := Pos + 2;
                     Current_Token := Tok_Brack_Plus_Brack;
                  else
                     Current_Token := Tok_Left_Brack;
                  end if;
               else
                  Current_Token := Tok_Left_Brack;
               end if;
               exit;
            when ']' =>
               Current_Token := Tok_Right_Brack;
               exit;
            when '^' =>
               if Source (Pos) = '~' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Bit_Xnor;
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Xor_Asgn;
               else
                  Current_Token := Tok_Bit_Xor;
               end if;
               exit;
            when '`' =>
               if Scan_Directive then
                  return;
               end if;
            when '/' =>
               if Source (Pos) = '/' then
                  if Scan_Line_Comment then
                     return;
                  end if;
                  --  Continue scanning.
               elsif Source (Pos) = '*' then
                  Scan_Block_Comment;
                  if Flag_Scan_All then
                     Current_Token := Tok_Block_Comment;
                     return;
                  end if;
                  --  Continue scanning.
               elsif Std in Systemverilog_Standard
                 and Source (Pos) = '='
               then
                  Pos := Pos + 1;
                  Current_Token := Tok_Div_Asgn;
                  exit;
               else
                  Current_Token := Tok_Slash;
                  exit;
               end if;
            when '{' =>
               Current_Token := Tok_Left_Curly;
               exit;
            when '|' =>
               if Source (Pos) = '|' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Logic_Or;
               elsif Std in Systemverilog_Standard then
                  if Source (Pos) = '=' then
                     Pos := Pos + 1;
                     if Source (Pos) = '>' then
                        Pos := Pos + 1;
                        Current_Token := Tok_Bar_Double_Arrow;
                     else
                        Current_Token := Tok_Or_Asgn;
                     end if;
                  elsif Source (Pos) = '-'
                    and then Source (Pos + 1) = '>'
                  then
                     Pos := Pos + 2;
                     Current_Token := Tok_Bar_Double_Arrow;
                  else
                     Current_Token := Tok_Bit_Or;
                  end if;
               else
                  Current_Token := Tok_Bit_Or;
               end if;
               exit;
            when '}' =>
               Current_Token := Tok_Right_Curly;
               exit;
            when '~' =>
               if Source (Pos) = '^' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Bit_Nxor;
               elsif Source (Pos) = '|' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Red_Nor;
               elsif Source (Pos) = '&' then
                  Pos := Pos + 1;
                  Current_Token := Tok_Red_Nand;
               else
                  Current_Token := Tok_Bit_Neg;
               end if;
               exit;
         end case;
      end loop;

      Scan_Mode := Scan_Mode_Normal;
   end Scan_From_Source;

   procedure Scan is
   begin
      case Current_Kind is
         when Kind_Nil =>
            raise Internal_Error;
         when Kind_File =>
            Scan_From_Source;
         when Kind_Pp_String_In_File =>
            Scan_From_Pp_String;
         when Kind_Macro =>
            Scan_From_Macro;
      end case;
   end Scan;

   function Image_Current_Token return String is
   begin
      if Current_Token in Toks_Literals then
         case Toks_Literals (Current_Token) is
            when Tok_Dec_Number =>
               declare
                  Res : constant String :=
                    Uns32'Image (Current_Number_Lo.Val);
               begin
                  return Res (2 .. Res'Last);
               end;
            when Tok_System =>
               return '$' & Name_Table.Image (Current_Identifier);
            when Tok_Identifier =>
               return Name_Table.Image (Current_Identifier);
            when Tok_String_Literal =>
               declare
                  Str : String (1 .. Current_String_Len);
               begin
                  for I in Str'Range loop
                     --  FIXME: escape non-ASCII characters.
                     Str (I) :=
                       Str_Table.Char_String8 (Current_String, Pos32 (I));
                  end loop;
                  return '"' & Str & '"';
               end;
            when Tok_Number_32 =>
               return "NUMBER_32";
            when Tok_Number_64 =>
               return "NUMBER_64";
            when Tok_Dec_Bignum =>
               return "DEC_BIGNUM";
            when Tok_Bignum =>
               return "BIG_NUMBER";
            when Tok_Real_Number =>
               return "REAL_NUMBER";
            when Tok_Scale_Number =>
               return "SCALE_NUMBER";
            when Tok_Time_Literal =>
               return "TIME_LITERAL";
            when Tok_Fixed_Time_Literal =>
               return "FIXED_TIME_LITERAL";
         end case;
      else
         return Image (Current_Token);
      end if;
   end Image_Current_Token;
end Verilog.Scans;
