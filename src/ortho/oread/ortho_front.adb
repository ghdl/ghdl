--  Ortho code compiler.
--  Copyright (C) 2005 Tristan Gingold
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
with Ada.Unchecked_Deallocation;
with Ortho_Nodes; use Ortho_Nodes;
with Ortho_Ident; use Ortho_Ident;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces; use Interfaces;
with Ada.Exceptions;
--with GNAT.Debug_Pools;

--  TODO:
--  uncomplete type: check for type redefinition

package body Ortho_Front is
   --  If true, emit line number before each statement.
   --  If flase, keep line number indication in the source file.
   Flag_Renumber : Boolean := True;

   procedure Init is
   begin
      null;
   end Init;

   function Decode_Option (Opt : String_Acc; Arg : String_Acc) return Natural
   is
      pragma Unreferenced (Arg);
   begin
      if Opt.all = "-r" or Opt.all = "--ghdl-r" then
         Flag_Renumber := True;
         return 1;
      else
         return 0;
      end if;
   end Decode_Option;

   --  File buffer.
   File_Name : String_Acc;
   Buf : String (1 .. 2048 + 1);
   Buf_Len : Natural;
   Pos : Natural;
   Lineno : Natural;

   Fd : File_Descriptor;

   Error : exception;

   procedure Puterr (Msg : String)
   is
      L : Integer;
      pragma Unreferenced (L);
   begin
      L := Write (Standerr, Msg'Address, Msg'Length);
   end Puterr;

   procedure Puterr (N : Natural)
   is
      Str : constant String := Natural'Image (N);
   begin
      Puterr (Str (Str'First + 1 .. Str'Last));
   end Puterr;

   procedure Newline_Err is
   begin
      Puterr ((1 => LF));
   end Newline_Err;

   procedure Scan_Error (Msg : String) is
   begin
      Puterr (File_Name.all);
      Puterr (":");
      Puterr (Lineno);
      Puterr (": ");
      Puterr (Msg);
      Newline_Err;
      raise Error;
   end Scan_Error;

   procedure Parse_Error (Msg : String);
   pragma No_Return (Parse_Error);

   procedure Parse_Error (Msg : String) is
   begin
      Puterr (File_Name.all);
      Puterr (":");
      Puterr (Lineno);
      Puterr (": ");
      Puterr (Msg);
      Newline_Err;
      raise Error;
   end Parse_Error;


--    Uniq_Num : Natural := 0;

--    function Get_Uniq_Id return O_Ident
--    is
--       Str : String (1 .. 8);
--       V : Natural;
--    begin
--       V := Uniq_Num;
--       Uniq_Num := Uniq_Num + 1;
--       Str (1) := 'L';
--       Str (2) := '.';
--       for I in reverse 3 .. Str'Last loop
--          Str (I) := Character'Val ((V mod 10) + Character'Pos('0'));
--          V := V / 10;
--       end loop;
--       return Get_Identifier (Str);
--    end Get_Uniq_Id;

   --  Get the next character.
   --  Return NUL on end of file.
   function Get_Char return Character
   is
      Res : Character;
   begin
      if Buf (Pos) = NUL then
         --  Read line.
         Buf_Len := Read (Fd, Buf'Address, Buf'Length - 1);
         if Buf_Len = 0 then
            --  End of file.
            return NUL;
         end if;
         Pos := 1;
         Buf (Buf_Len + 1) := NUL;
      end if;

      Res := Buf (Pos);
      Pos := Pos + 1;
      return Res;
   end Get_Char;

   procedure Unget_Char is
   begin
      if Pos = Buf'First then
         raise Program_Error;
      end if;
      Pos := Pos - 1;
   end Unget_Char;

   type Token_Type is
      (Tok_Eof,
       Tok_Line_Number, Tok_File_Name, Tok_Comment,
       Tok_Ident, Tok_Num, Tok_String, Tok_Float_Num,
       Tok_Plus, Tok_Minus,
       Tok_Star, Tok_Div, Tok_Mod, Tok_Rem,
       Tok_Sharp,
       Tok_Not, Tok_Abs,
       Tok_Or, Tok_And, Tok_Xor,
       Tok_Equal, Tok_Not_Equal,
       Tok_Greater, Tok_Greater_Eq,
       Tok_Less, Tok_Less_Eq,
       Tok_Colon, Tok_Semicolon,
       Tok_Comma, Tok_Dot, Tok_Tick, Tok_Arob, Tok_Elipsis,
       Tok_Assign,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Left_Brace, Tok_Right_Brace,
       Tok_Left_Brack, Tok_Right_Brack,
       Tok_Unsigned, Tok_Signed, Tok_Float,
       Tok_Array, Tok_Subarray,
       Tok_Access,
       Tok_Record, Tok_Subrecord, Tok_Union,
       Tok_Boolean, Tok_Enum,
       Tok_If, Tok_Then, Tok_Else, Tok_Elsif,
       Tok_Loop, Tok_Exit, Tok_Next,
       Tok_Is, Tok_Of, Tok_All,
       Tok_Return,
       Tok_Type,
       Tok_External, Tok_Private, Tok_Public, Tok_Local,
       Tok_Procedure, Tok_Function,
       Tok_Constant, Tok_Var,
       Tok_Declare, Tok_Begin, Tok_End,
       Tok_Case, Tok_When, Tok_Default, Tok_Arrow,
       Tok_Null);

   type Hash_Type is new Unsigned_32;

   type Name_Type;
   type Name_Acc is access Name_Type;

   --  Symbol table.
   type Syment_Type;
   type Syment_Acc is access Syment_Type;
   type Syment_type is record
      --  The hash for the symbol.
      Hash : Hash_Type;
      --  Identification of the symbol.
      Ident : O_Ident;
      --  Next symbol with the same collision.
      Next : Syment_Acc;
      --  Meaning of the symbol.
      Name : Name_Acc;
   end record;

   --  Well known identifiers (used for attributes).
   Id_Address : Syment_Acc;
   Id_Unchecked_Address : Syment_Acc;
   Id_Subprg_Addr : Syment_Acc;
   Id_Conv : Syment_Acc;
   Id_Sizeof : Syment_Acc;
   Id_Record_Sizeof : Syment_Acc;
   Id_Alignof : Syment_Acc;
   Id_Alloca : Syment_Acc;
   Id_Offsetof : Syment_Acc;

   Token_Number : Unsigned_64;
   Token_Float : IEEE_Float_64;
   Token_Ident : String (1 .. 2048);
   Token_Idlen : Natural;
   Token_Hash : Hash_Type;
   Token_Sym : Syment_Acc;

   --  The symbol table.
   type Syment_Acc_Array is array (Hash_Type range <>) of Syment_Acc;
   type Syment_Acc_Map (Max : Hash_Type) is record
      Map : Syment_Acc_Array (0 .. Max);
   end record;
   type Syment_Acc_Map_Acc is access Syment_Acc_Map;

   --  Prime numbers for the number of buckets in the hash map.
   Hash_Primes : constant array (Natural range <>) of Hash_Type :=
     (389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613,
      393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843,
      50331653, 100663319, 201326611, 402653189, 805306457, 1610612741);

   --  Number of entries in the hash table.
   Nbr_Syment : Natural := 0;

   --  Maximum number of entries before resizing the hash table.
   Max_Syment : Natural := 512;  --  Could be less or more.

   --  Current prime number in Hash_Primes.
   Cur_Prime_Idx : Natural := 0;

   Symtable : Syment_Acc_Map_Acc;

   type Node_Kind is (Decl_Keyword, Decl_Type, Decl_Param,
                      Node_Function, Node_Procedure, Node_Object, Node_Field,
                      Node_Lit,
                      Type_Boolean, Type_Enum,
                      Type_Unsigned, Type_Signed, Type_Float,
                      Type_Array, Type_Subarray, Type_Subrecord,
                      Type_Access, Type_Record, Type_Union);
   subtype Nodes_Subprogram is Node_Kind range Node_Function .. Node_Procedure;

   type Node (<>);
   type Node_Acc is access Node;

   type Node_Array is array (Natural range <>) of Node_Acc;

   type Node_Map (Len : Natural) is record
      Map : Node_Array (1 .. Len);
   end record;
   type Node_Map_Acc is access Node_Map;

   type Node_Array_Acc is access Node_Array;

   type Node (Kind : Node_Kind) is record
      case Kind is
         when Decl_Keyword =>
            --  Keyword.
            --  A keyword is not a declaration since the identifier has only
            --  one meaning (the keyword).
            Keyword : Token_Type;
         when Decl_Type
           | Decl_Param
           | Node_Function
           | Node_Procedure
           | Node_Object
           | Node_Lit =>
            --  Declarations
            Decl_Storage : O_Storage;
            --  For constants: True iff fully defined.
            Decl_Defined : Boolean;
            --  All declarations but NODE_PROCEDURE have a type.
            Decl_Dtype : Node_Acc;
            case Kind is
               when Decl_Type =>
                  --  Type declaration.
                  null;
               when Decl_Param =>
                  --  Parameter identifier.
                  Param_Name : Syment_Acc;
                  --  Parameter ortho node.
                  Param_Node : O_Dnode;
                  --  Next parameter of the parameters list.
                  Param_Next : Node_Acc;
               when Node_Procedure
                 | Node_Function =>
                  --  Subprogram symbol name.
                  Subprg_Name : Syment_Acc;
                  --  List of parameters.
                  Subprg_Params : Node_Acc;
                  --  Subprogram ortho node.
                  Subprg_Node : O_Dnode;
               when Node_Object =>
                  --  Name of the object (constant, variable).
                  Obj_Name : O_Ident;
                  --  Ortho node of the object.
                  Obj_Node : O_Dnode;
               when Node_Lit =>
                  --  Name of the literal.
                  Lit_Name : O_Ident;
                  --  Enum literal
                  Lit_Cnode : O_Cnode;
                  --  Next literal for the type.
                  Lit_Next : Node_Acc;
               when others =>
                  null;
            end case;
         when Node_Field =>
            --  Record field.
            Field_Pos : Natural;  --  From 1 to N.
            Field_Ident : Syment_Acc;
            Field_Fnode : O_Fnode;
            Field_Type : Node_Acc;
            Field_Next : Node_Acc;
            --  Next entry in the field map (if the map exists).
            Field_Hash_Next : Node_Acc;
         when Type_Signed
           | Type_Unsigned
           | Type_Float
           | Type_Array
           | Type_Subarray
           | Type_Record
           | Type_Subrecord
           | Type_Union
           | Type_Access
           | Type_Boolean
           | Type_Enum =>
            --  Ortho node type.
            Type_Onode : O_Tnode;
            case Kind is
               when Type_Array =>
                  Array_Index : Node_Acc;
                  Array_Element : Node_Acc;
               when Type_Subarray =>
                  Subarray_Base : Node_Acc;
                  Subarray_El : Node_Acc;
               when Type_Access =>
                  Access_Dtype : Node_Acc;
               when Type_Record
                 | Type_Union =>
                  --  Simply linked list of fields.  Works well unless the
                  --  number of fields is too high.
                  Record_Union_Fields : Node_Array_Acc;
                  --  Hash map of fields (the key is the hash of the ident).
                  Record_Union_Map : Node_Map_Acc;
               when Type_Subrecord =>
                  Subrecord_Base : Node_Acc;
                  Subrecord_Fields : Node_Array_Acc;
               when Type_Enum
                 | Type_Boolean =>
                  Enum_Lits : Node_Acc;
               when Type_Float =>
                  null;
               when others =>
                  null;
            end case;
      end case;
   end record;

   type Scope_Type;
   type Scope_Acc is access Scope_Type;

   type Name_Type is record
      --  Current interpretation of the symbol.
      Inter : Node_Acc;
      --  Next declaration in the current scope.
      Next : Syment_Acc;
      --  Interpretation in a previous scope.
      Up : Name_Acc;
      --  Current scope.
      Scope : Scope_Acc;
   end record;

   type Scope_Type is record
      --  Simply linked list of names.
      Names : Syment_Acc;
      --  Previous scope.
      Prev : Scope_Acc;
   end record;

   --  Return the current declaration for symbol SYM.
   function Get_Decl (Sym : Syment_Acc) return Node_Acc;
   pragma Inline (Get_Decl);

   procedure Scan_Char (C : Character)
   is
      R : Character;
   begin

      if C = '\' then
         R := Get_Char;
         case R is
            when 'n' =>
               R := LF;
            when 'r' =>
               R := CR;
            when ''' =>
               R := ''';
            when '"' => -- "
               R := '"'; -- "
            when others =>
               Scan_Error ("bad character sequence \" & R);
         end case;
      else
         R := C;
      end if;
      Token_Idlen := Token_Idlen + 1;
      Token_Ident (Token_Idlen) := R;
   end Scan_Char;

   function Get_Hash (Str : String) return Hash_Type
   is
      Res : Hash_Type;
   begin
      Res := 0;
      for I in Str'Range loop
         Res := Res * 31 + Character'Pos (Str (I));
      end loop;
      return Res;
   end Get_Hash;

   --  Previous token.
   Tok_Previous : Token_Type;

   function To_Digit (C : Character) return Integer is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when others =>
            return -1;
      end case;
   end To_Digit;

   function Is_Digit (C : Character) return Boolean is
   begin
      case C is
         when '0' .. '9'
           | 'A' .. 'F'
           | 'a' .. 'f' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Digit;

   function Scan_Hex_Number return Token_Type
   is
      C : Character;
      Exp : Integer;
      Exp_Neg : Boolean;
      After_Point : Natural;
   begin
      Token_Number := 0;
      C := Get_Char;
      if not Is_Digit (C) then
         Scan_Error ("digit expected after '0x'");
      end if;
      loop
         Token_Number := Token_Number * 16 + Unsigned_64 (To_Digit (C));
         C := Get_Char;
         exit when not Is_Digit (C);
      end loop;

      After_Point := 0;
      if C = '.' then
         loop
            C := Get_Char;
            exit when not Is_Digit (C);
            if Shift_Right (Token_Number, 60) = 0 then
               Token_Number := Token_Number * 16 + Unsigned_64 (To_Digit (C));
               After_Point := After_Point + 4;
            end if;
         end loop;

         Exp := 0;
         if C = 'p' or C = 'P' then
            -- A real number.
            C := Get_Char;
            Exp_Neg := False;
            if C = '-' then
               Exp_Neg := True;
               C := Get_Char;
            elsif C = '+' then
               C := Get_Char;
            end if;
            if not Is_Digit (C) then
               Scan_Error ("digit expected after 'p'");
            end if;
            loop
               Exp := Exp * 10 + To_Digit (C);
               C := Get_Char;
               exit when not Is_Digit (C);
            end loop;
            if Exp_Neg then
               Exp := -Exp;
            end if;
         end if;
         Exp := Exp - After_Point;
         Unget_Char;
         Token_Float :=
           IEEE_Float_64'Scaling (IEEE_Float_64 (Token_Number), Exp);
         return Tok_Float_Num;
      else
         Unget_Char;
         return Tok_Num;
      end if;
   end Scan_Hex_Number;

   function Scan_Fp_Number return Token_Type
   is
      After_Point : Integer;
      C : Character;
      Exp : Integer;
      Exp_Neg : Boolean;
   begin
      -- A real number.
      After_Point := 0;
      Token_Float := IEEE_Float_64 (Token_Number);
      loop
         C := Get_Char;
         exit when C not in '0' .. '9';
         Token_Float := Token_Float * 10.0 + IEEE_Float_64 (To_Digit (C));
         After_Point := After_Point + 1;
      end loop;
      if C = 'e' or C = 'E' then
         Exp := 0;
         C := Get_Char;
         Exp_Neg := False;
         if C = '-' then
            Exp_Neg := True;
            C := Get_Char;
         elsif C = '+' then
            C := Get_Char;
         elsif not Is_Digit (C) then
            Scan_Error ("digit expected");
         end if;
         while Is_Digit (C) loop
            Exp := Exp * 10 + To_Digit (C);
            C := Get_Char;
         end loop;
         if Exp_Neg then
            Exp := -Exp;
         end if;
         Exp := Exp - After_Point;
      else
         Exp := - After_Point;
      end if;
      Unget_Char;
      Token_Float := Token_Float * 10.0 ** Exp;
      if Token_Float > IEEE_Float_64'Last then
         Token_Float := IEEE_Float_64'Last;
      end if;
      return Tok_Float_Num;
   end Scan_Fp_Number;

   function Scan_Number (First_Char : Character) return Token_Type
   is
      C : Character;
      Base : Unsigned_64;
   begin
      C := First_Char;
      Token_Number := 0;

      --  Handle '0x' prefix.
      if C = '0' then
         --  '0' can be discarded.
         C := Get_Char;
         if C = 'x' or C = 'X' then
            return Scan_Hex_Number;
         elsif C = '.' then
            return Scan_Fp_Number;
         elsif not Is_Digit (C) then
            Unget_Char;
            return Tok_Num;
         end if;
      end if;

      loop
         Token_Number := Token_Number * 10 + Unsigned_64 (To_Digit (C));
         C := Get_Char;
         exit when not Is_Digit (C);
      end loop;
      if C = '#' then
         Base := Token_Number;
         Token_Number := 0;
         C := Get_Char;
         loop
            if C /= '_' then
               Token_Number :=
                 Token_Number * Base + Unsigned_64 (To_Digit (C));
            end if;
            C := Get_Char;
            exit when C = '#';
         end loop;
         return Tok_Num;
      end if;
      if C = '.' then
         return Scan_Fp_Number;
      else
         Unget_Char;
         return Tok_Num;
      end if;
   end Scan_Number;

   procedure Scan_Comment
   is
      C : Character;
   begin
      Token_Idlen := 0;
      loop
         C := Get_Char;
         exit when C = CR or C = LF;
         Token_Idlen := Token_Idlen + 1;
         Token_Ident (Token_Idlen) := C;
      end loop;
      Unget_Char;
   end Scan_Comment;

   function Get_Ident_Token return Token_Type
   is
      H : Hash_Type;
      S : Syment_Acc;
      N : Node_Acc;
   begin
      H := Token_Hash mod Symtable.Max;
      S := Symtable.Map (H);
      while S /= null loop
         if S.Hash = Token_Hash
           and then Is_Equal (S.Ident, Token_Ident (1 .. Token_Idlen))
         then
            --  This identifier is known.
            Token_Sym := S;

            --  It may be a keyword.
            if S.Name /= null then
               N := Get_Decl (S);
               if N.Kind = Decl_Keyword then
                  return N.Keyword;
               end if;
            end if;

            return Tok_Ident;
         end if;
         S := S.Next;
      end loop;

      Nbr_Syment := Nbr_Syment + 1;
      if Nbr_Syment >= Max_Syment
        and then Cur_Prime_Idx < Hash_Primes'Last
      then
         --  Resize.
         Cur_Prime_Idx := Cur_Prime_Idx + 1;
         Max_Syment := Max_Syment * 2;

         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Syment_Acc_Map, Syment_Acc_Map_Acc);
            New_Table : Syment_Acc_Map_Acc;
            Ns, Next_Ns : Syment_Acc;
            Nh : Hash_Type;
         begin
            New_Table := new Syment_Acc_Map (Hash_Primes (Cur_Prime_Idx));

            --  Fill the new hash table.
            for I in Symtable.Map'Range loop
               Ns := Symtable.Map (I);
               while Ns /= null loop
                  Next_Ns := Ns.Next;

                  Nh := Ns.Hash mod New_Table.Max;
                  Ns.Next := New_Table.Map (Nh);
                  New_Table.Map (Nh) := Ns;

                  Ns := Next_Ns;
               end loop;
            end loop;

            --  Replace the old one with the new one.
            Free (Symtable);
            Symtable := New_Table;
         end;

         --  Recompute H
         H := Token_Hash mod Symtable.Max;
      end if;

      Symtable.Map (H) := new Syment_Type'
        (Hash => Token_Hash,
         Ident => Get_Identifier (Token_Ident (1 .. Token_Idlen)),
         Next => Symtable.Map (H),
         Name => null);
      Token_Sym := Symtable.Map (H);
      return Tok_Ident;
   end Get_Ident_Token;

   --  Get the next token.
   function Get_Token return Token_Type
   is
      C : Character;
   begin
      loop

         C := Get_Char;
         << Again >> null;
         case C is
            when NUL =>
               return Tok_Eof;
            when ' ' | HT =>
               null;
            when LF =>
               Lineno := Lineno + 1;
               C := Get_Char;
               if C /= CR then
                  goto Again;
               end if;
            when CR =>
               Lineno := Lineno + 1;
               C := Get_Char;
               if C /= LF then
                  goto Again;
               end if;
            when '+' =>
               return Tok_Plus;
            when '-' =>
               C := Get_Char;
               if C = '-' then
                  C := Get_Char;
                  if C = '#' then
                     return Tok_Line_Number;
                  elsif C = 'F' then
                     Scan_Comment;
                     return Tok_File_Name;
                  elsif C = ' ' then
                     Scan_Comment;
                     return Tok_Comment;
                  else
                     Scan_Error ("bad comment");
                  end if;
               else
                  Unget_Char;
                  return Tok_Minus;
               end if;
            when '/' =>
               C := Get_Char;
               if C = '=' then
                  return Tok_Not_Equal;
               else
                  Unget_Char;
                  return Tok_Div;
               end if;
            when '*' =>
               return Tok_Star;
            when '#' =>
               return Tok_Sharp;
            when '=' =>
               C := Get_Char;
               if C = '>' then
                  return Tok_Arrow;
               else
                  Unget_Char;
                  return Tok_Equal;
               end if;
            when '>' =>
               C := Get_Char;
               if C = '=' then
                  return Tok_Greater_Eq;
               else
                  Unget_Char;
                  return Tok_Greater;
               end if;
            when '(' =>
               return Tok_Left_Paren;
            when ')' =>
               return Tok_Right_Paren;
            when '{' =>
               return Tok_Left_Brace;
            when '}' =>
               return Tok_Right_Brace;
            when '[' =>
               return Tok_Left_Brack;
            when ']' =>
               return Tok_Right_Brack;
            when '<' =>
               C := Get_Char;
               if C = '=' then
                  return Tok_Less_Eq;
               else
                  Unget_Char;
                  return Tok_Less;
               end if;
            when ':' =>
               C := Get_Char;
               if C = '=' then
                  return Tok_Assign;
               else
                  Unget_Char;
                  return Tok_Colon;
               end if;
            when '.' =>
               C := Get_Char;
               if C = '.' then
                  C := Get_Char;
                  if C = '.' then
                     return Tok_Elipsis;
                  else
                     Scan_Error ("'...' expected");
                  end if;
               else
                  Unget_Char;
                  return Tok_Dot;
               end if;
            when ';' =>
               return Tok_Semicolon;
            when ',' =>
               return Tok_Comma;
            when '@' =>
               return Tok_Arob;
            when ''' =>
               if Tok_Previous = Tok_Ident then
                  return Tok_Tick;
               else
                  Token_Number := Character'Pos (Get_Char);
                  C := Get_Char;
                  if C /= ''' then
                     Scan_Error ("ending single quote expected");
                  end if;
                  return Tok_Num;
               end if;
            when '"' => -- "
               --  Eat double quote.
               C := Get_Char;
               Token_Idlen := 0;
               loop
                  Scan_Char (C);
                  C := Get_Char;
                  exit when C = '"'; -- "
               end loop;
               return Tok_String;
            when '0' .. '9' =>
               return Scan_Number (C);
            when 'a' .. 'z'
              | 'A' .. 'Z'
              | '_' =>
               Token_Idlen := 0;
               Token_Hash := 0;
               loop
                  Token_Idlen := Token_Idlen + 1;
                  Token_Ident (Token_Idlen) := C;
                  Token_Hash := Token_Hash * 31 + Character'Pos (C);
                  C := Get_Char;
                  exit when (C < 'A' or C > 'Z')
                    and (C < 'a' or C > 'z')
                    and (C < '0' or C > '9')
                    and (C /= '_');
               end loop;
               Unget_Char;
               return Get_Ident_Token;
            when others =>
               Scan_Error ("Bad character:"
                           & Integer'Image (Character'Pos (C))
                           & C);
               return Tok_Eof;
         end case;
      end loop;
   end Get_Token;

   --  The current token.
   Tok : Token_Type;

   procedure Next_Token is
   begin
      Tok_Previous := Tok;
      Tok := Get_Token;
   end Next_Token;

   procedure Expect (T : Token_Type; Msg : String := "") is
   begin
      if Tok /= T then
         if Msg'Length = 0 then
            case T is
               when Tok_Left_Brace =>
                  Parse_Error ("'{' expected");
               when others =>
                  if Tok = Tok_Ident then
                     Parse_Error
                       (Token_Type'Image (T) & " expected, found '" &
                        Token_Ident (1 .. Token_Idlen) & "'");
                  else
                     Parse_Error (Token_Type'Image (T) & " expected, found "
                                  & Token_Type'Image (Tok));
                  end if;
            end case;
         else
            Parse_Error (Msg);
         end if;
      end if;
   end Expect;

   procedure Next_Expect (T : Token_Type; Msg : String := "") is
   begin
      Next_Token;
      Expect (T, Msg);
   end Next_Expect;

   --  Scopes and identifiers.


   --  Current scope.
   Scope : Scope_Acc := null;

   --  Add a declaration for symbol SYM in the current scope.
   --  INTER defines the meaning of the declaration.
   --  There must be at most one declaration for a symbol in the current scope,
   --  i.e. a symbol cannot be redefined.
   procedure Add_Decl (Sym : Syment_Acc; Inter : Node_Acc);

   --  Return TRUE iff SYM is already defined in the current scope.
   function Is_Defined (Sym : Syment_Acc) return Boolean;

   --  Create new scope.
   procedure Push_Scope;

   --  Close the current scope.  Symbols defined in the scope regain their
   --  previous declaration.
   procedure Pop_Scope;


   procedure Push_Scope
   is
      Nscope : Scope_Acc;
   begin
      Nscope := new Scope_Type'(Names => null, Prev => Scope);
      Scope := Nscope;
   end Push_Scope;

   procedure Pop_Scope
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Name_Type, Name => Name_Acc);

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Scope_Type, Name => Scope_Acc);

      Sym : Syment_Acc;
      N_Sym : Syment_Acc;
      Name : Name_Acc;
      Old_Scope : Scope_Acc;
   begin
      Sym := Scope.Names;
      while Sym /= null loop
         Name := Sym.Name;
         --  Check.
         if Name.Scope /= Scope then
            raise Program_Error;
         end if;

         --  Set the interpretation of this symbol.
         Sym.Name := Name.Up;

         N_Sym := Name.Next;

         Free (Name);
         Sym := N_Sym;
      end loop;

      --  Free scope.
      Old_Scope := Scope;
      Scope := Scope.Prev;
      Free (Old_Scope);
   end Pop_Scope;

   function Is_Defined (Sym : Syment_Acc) return Boolean is
   begin
      if Sym.Name /= null
        and then Sym.Name.Scope = Scope
      then
         return True;
      else
         return False;
      end if;
   end Is_Defined;

   function New_Symbol (Str : String) return Syment_Acc
   is
      Ent : Syment_Acc;
      H : Hash_Type;
   begin
      Ent := new Syment_Type'(Hash => Get_Hash (Str),
                              Ident => Get_Identifier (Str),
                              Next => null,
                              Name => null);
      H := Ent.Hash mod Symtable.Max;
      Ent.Next := Symtable.Map (H);
      Symtable.Map (H) := Ent;

      Nbr_Syment := Nbr_Syment + 1;

      --  This function doesn't handle resizing, as it is called only for
      --  keywords during initialization.  Be sure to use a big enough initial
      --  size for the hash table.
      pragma Assert (Nbr_Syment < Max_Syment);

      return Ent;
   end New_Symbol;

   procedure Add_Keyword (Str : String; Token : Token_Type)
   is
      Kw : String (Str'Range);
      Ent : Syment_Acc;
   begin
      --  Convert to uppercase.
      for I in Str'Range loop
         pragma Assert (Str (I) in 'a' .. 'z');
         Kw (I) := Character'Val
           (Character'Pos ('A')
                 + Character'Pos (Str (I)) - Character'Pos ('a'));
      end loop;

      Ent := New_Symbol (Kw);
      if Ent.Name /= null
        or else Scope /= null
      then
         --  Redefinition of a keyword.
         raise Program_Error;
      end if;
      Ent.Name := new Name_Type'(Inter => new Node'(Kind => Decl_Keyword,
                                                    Keyword => Token),
                                 Next => null,
                                 Up => null,
                                 Scope => null);
   end Add_Keyword;

   procedure Add_Decl (Sym : Syment_Acc; Inter : Node_Acc)
   is
      Name : Name_Acc;
      Prev : Node_Acc;
   begin
      Name := Sym.Name;
      if Name /= null and then Name.Scope = Scope then
         Prev := Name.Inter;
         if Prev.Kind = Inter.Kind
           and then Prev.Kind /= Node_Field
           and then Prev.Decl_Dtype = Inter.Decl_Dtype
           and then Prev.Decl_Storage = O_Storage_External
           and then Inter.Decl_Storage = O_Storage_Public
         then
            --  Redefinition
            Name.Inter := Inter;
            return;
         end if;
         Parse_Error ("redefinition of " & Get_String (Sym.Ident));
      end if;
      Name := new Name_Type'(Inter => Inter,
                             Next => Scope.Names,
                             Up => Sym.Name,
                             Scope => Scope);
      Sym.Name := Name;
      Scope.Names := Sym;
   end Add_Decl;

   function Get_Decl (Sym : Syment_Acc) return Node_Acc is
   begin
      if Sym.Name = null then
         Parse_Error ("undefined identifier " & Get_String (Sym.Ident));
      else
         return Sym.Name.Inter;
      end if;
   end Get_Decl;

   function Parse_Constant_Value (Atype : Node_Acc) return O_Cnode;
   function Parse_Address (Prefix : Node_Acc) return O_Enode;
   function Parse_Constant_Address (Prefix : Node_Acc) return O_Cnode;
   procedure Parse_Declaration;
   procedure Parse_Compound_Statement;

   function Parse_Type return Node_Acc;

   --  Return the index of FIELD in map MAP.
   function Field_Map_Index (Map : Node_Map_Acc; Sym : Syment_Acc)
                            return Natural is
   begin
      return 1 + Natural (Sym.Hash mod Hash_Type (Map.Len));
   end Field_Map_Index;

   --  Grammar:
   --      { ident : type ; }
   --    end
   function Parse_Fields return Node_Array_Acc
   is
      F_Type : Node_Acc;
      F : Syment_Acc;
      First_Field : Node_Acc;
      Last_Field : Node_Acc;
      Field : Node_Acc;
      Num : Natural;
      Res : Node_Array_Acc;
   begin
      Push_Scope;

      Last_Field := null;
      First_Field := null;
      Num := 0;
      loop
         exit when Tok = Tok_End;
         exit when Tok = Tok_Right_Paren;

         if Tok /= Tok_Ident then
            Parse_Error ("field name expected");
         end if;

         Num := Num + 1;

         F := Token_Sym;
         Next_Expect (Tok_Colon, "':' expected");
         Next_Token;
         F_Type := Parse_Type;
         Field := new Node'(Kind => Node_Field,
                            Field_Pos => Num,
                            Field_Ident => F,
                            Field_Fnode => O_Fnode_Null,
                            Field_Type => F_Type,
                            Field_Next => null,
                            Field_Hash_Next => null);

         --  Check fields are uniq.
         Add_Decl (F, Field);

         --  Append field
         if Last_Field = null then
            First_Field := Field;
         else
            Last_Field.Field_Next := Field;
         end if;
         Last_Field := Field;

         Expect (Tok_Semicolon, "';' expected");
         Next_Token;
      end loop;

      Pop_Scope;

      Res := new Node_Array(1 .. Num);
      for I in Res'Range loop
         Res (I) := First_Field;
         First_Field := First_Field.Field_Next;
      end loop;

      return Res;
   end Parse_Fields;

   procedure Parse_Fields (Aggr_Type : Node_Acc;
                           Constr : in out O_Element_List)
   is
      Fields : Node_Array_Acc;
      Field : Node_Acc;
   begin
      Fields := Parse_Fields;
      Expect (Tok_End, "end expected");
      Aggr_Type.Record_Union_Fields := Fields;

      for I in Fields'Range loop
         Field := Fields (I);
         case Aggr_Type.Kind is
            when Type_Record =>
               New_Record_Field (Constr, Field.Field_Fnode,
                                 Field.Field_Ident.Ident,
                                 Field.Field_Type.Type_Onode);
            when Type_Union =>
               New_Union_Field (Constr, Field.Field_Fnode,
                                Field.Field_Ident.Ident,
                                Field.Field_Type.Type_Onode);
            when others =>
               raise Program_Error;
         end case;
      end loop;

      --  Create a map if there are a lot of fields.
      if Fields'Last > 16 then
         declare
            Map : Node_Map_Acc;
            Idx : Natural;
         begin
            Map := new Node_Map'(Len => Fields'Last / 3,
                                 Map => (others => null));
            Aggr_Type.Record_Union_Map := Map;
            for I in Fields'Range loop
               Field := Fields (I);
               Idx := Field_Map_Index (Map, Field.Field_Ident);
               Field.Field_Hash_Next := Map.Map (Idx);
               Map.Map (Idx) := Field;
            end loop;
         end;
      end if;
   end Parse_Fields;

   procedure Parse_Record_Type (Def : Node_Acc)
   is
      Constr : O_Element_List;
   begin
      if Def.Type_Onode = O_Tnode_Null then
         Start_Record_Type (Constr);
      else
         Start_Uncomplete_Record_Type (Def.Type_Onode, Constr);
      end if;
      Parse_Fields (Def, Constr);
      Next_Expect (Tok_Record, "end record expected");
      Finish_Record_Type (Constr, Def.Type_Onode);
   end Parse_Record_Type;

   procedure Parse_Subrecord_Type (Def : Node_Acc)
   is
      Base : Node_Acc;
      Constr : O_Element_Sublist;
      Fields : Node_Array_Acc;
      Field : Node_Acc;
   begin
      Base := Parse_Type;
      if Base.Kind /= Type_Record then
         Parse_Error ("subrecord base type must be a record type");
      end if;
      Def.Subrecord_Base := Base;
      Expect (Tok_Left_Paren);
      Next_Token;

      Fields := Parse_Fields;
      Def.Subrecord_Fields := Fields;
      Expect (Tok_Right_Paren);

      Start_Record_Subtype (Base.Type_Onode, Constr);
      for I in Fields'Range loop
         Field := Fields (I);
         New_Subrecord_Field (Constr, Field.Field_Fnode,
                              Field.Field_Type.Type_Onode);
      end loop;
      Finish_Record_Subtype (Constr, Def.Type_Onode);
   end Parse_Subrecord_Type;

   procedure Parse_Union_Type (Def : Node_Acc)
   is
      Constr : O_Element_List;
   begin
      Start_Union_Type (Constr);
      Parse_Fields (Def, Constr);
      Next_Expect (Tok_Union, "end union expected");
      Finish_Union_Type (Constr, Def.Type_Onode);
   end Parse_Union_Type;

   function Parse_Type return Node_Acc
   is
      Res : Node_Acc;
      T : Token_Type;
   begin
      T := Tok;
      case T is
         when Tok_Unsigned
           | Tok_Signed =>
            Next_Expect (Tok_Left_Paren, "'(' expected");
            Next_Expect (Tok_Num, "number expected");
            case T is
               when Tok_Unsigned =>
                  Res := new Node'
                    (Kind => Type_Unsigned,
                     Type_Onode => New_Unsigned_Type (Natural
                                                      (Token_Number)));
               when Tok_Signed =>
                  Res := new Node'
                     (Kind => Type_Signed,
                      Type_Onode => New_Signed_Type (Natural
                                                     (Token_Number)));
               when others =>
                  raise Program_Error;
            end case;
            Next_Expect (Tok_Right_Paren, "')' expected");
         when Tok_Float =>
            Res := new Node'(Kind => Type_Float,
                             Type_Onode => New_Float_Type);
         when Tok_Array =>
            declare
               Index_Node : Node_Acc;
               El_Node : Node_Acc;
            begin
               Next_Expect (Tok_Left_Brack, "'[' expected");
               Next_Token;
               Index_Node := Parse_Type;
               Expect (Tok_Right_Brack, "']' expected");
               Next_Expect (Tok_Of, "'OF' expected");
               Next_Token;
               El_Node := Parse_Type;
               Res := new Node'
                 (Kind => Type_Array,
                  Type_Onode => New_Array_Type (El_Node.Type_Onode,
                                                Index_Node.Type_Onode),
                  Array_Index => Index_Node,
                  Array_Element => El_Node);
            end;
            return Res;
         when Tok_Subarray =>
            --  Grammar:
            --    SUBARRAY type '[' len ']' [ OF eltype ]
            declare
               Base_Node : Node_Acc;
               Len : O_Cnode;
               El_Node : Node_Acc;
               Res_Type : O_Tnode;
            begin
               Next_Token;
               Base_Node := Parse_Type;
               if Base_Node.Kind /= Type_Array then
                  Parse_Error ("subarray base type is not an array type");
               end if;
               Expect (Tok_Left_Brack);
               Next_Token;
               Len := Parse_Constant_Value (Base_Node.Array_Index);
               Expect (Tok_Right_Brack);
               Next_Token;
               if Tok = Tok_Of then
                  Next_Token;
                  El_Node := Parse_Type;
                  --  TODO: check this is a subtype of the element
               else
                  El_Node := Base_Node.Array_Element;
                  --  TODO: check EL_NODE is constrained.
               end if;
               Res_Type := New_Array_Subtype
                 (Base_Node.Type_Onode, El_Node.Type_Onode, Len);
               Res := new Node' (Kind => Type_Subarray,
                                 Type_Onode => Res_Type,
                                 Subarray_Base => Base_Node,
                                 Subarray_El => El_Node);
               return Res;
            end;
         when Tok_Ident =>
            declare
               Inter : Node_Acc;
            begin
               Inter := Get_Decl (Token_Sym);
               if Inter = null then
                  Parse_Error ("undefined type name symbol "
                               & Get_String (Token_Sym.Ident));
               end if;
               if Inter.Kind /= Decl_Type then
                  Parse_Error ("type declarator expected");
               end if;
               Res := Inter.Decl_Dtype;
            end;
         when Tok_Access =>
            declare
               Dtype : Node_Acc;
            begin
               Next_Token;
               if Tok = Tok_Semicolon then
                  Res := new Node'
                    (Kind => Type_Access,
                     Type_Onode => New_Access_Type (O_Tnode_Null),
                     Access_Dtype => null);
               else
                  Dtype := Parse_Type;
                  Res := new Node'
                    (Kind => Type_Access,
                     Type_Onode => New_Access_Type (Dtype.Type_Onode),
                     Access_Dtype => Dtype);
               end if;
               return Res;
            end;
         when Tok_Record =>
            Next_Token;
            if Tok = Tok_Semicolon then
               --  Uncomplete record type.
               Res := new Node'(Kind => Type_Record,
                                Type_Onode => O_Tnode_Null,
                                Record_Union_Fields => null,
                                Record_Union_Map => null);
               New_Uncomplete_Record_Type (Res.Type_Onode);
               return Res;
            end if;

            Res := new Node'(Kind => Type_Record,
                             Type_Onode => O_Tnode_Null,
                             Record_Union_Fields => null,
                             Record_Union_Map => null);
            Parse_Record_Type (Res);
         when Tok_Subrecord =>
            Next_Token;
            Res := new Node'(Kind => Type_Subrecord,
                             Type_Onode => O_Tnode_Null,
                             Subrecord_Base => null,
                             Subrecord_Fields => null);
            Parse_Subrecord_Type (Res);
         when Tok_Union =>
            Next_Token;
            Res := new Node'(Kind => Type_Union,
                             Type_Onode => O_Tnode_Null,
                             Record_Union_Fields => null,
                             Record_Union_Map => null);
            Parse_Union_Type (Res);

         when Tok_Boolean =>
            declare
               False_Lit, True_Lit : Node_Acc;
            begin
               Res := new Node'(Kind => Type_Boolean,
                                Type_Onode => O_Tnode_Null,
                                Enum_Lits => null);
               Next_Expect (Tok_Left_Brace, "'{' expected");
               Next_Expect (Tok_Ident, "identifier expected");
               False_Lit := new Node'(Kind => Node_Lit,
                                      Decl_Dtype => Res,
                                      Decl_Storage => O_Storage_Public,
                                      Decl_Defined => False,
                                      Lit_Name => Token_Sym.Ident,
                                      Lit_Cnode => O_Cnode_Null,
                                      Lit_Next => null);
               Next_Expect (Tok_Comma, "',' expected");
               Next_Expect (Tok_Ident, "identifier expected");
               True_Lit := new Node'(Kind => Node_Lit,
                                     Decl_Dtype => Res,
                                     Decl_Storage => O_Storage_Public,
                                     Decl_Defined => False,
                                     Lit_Name => Token_Sym.Ident,
                                     Lit_Cnode => O_Cnode_Null,
                                     Lit_Next => null);
               Next_Expect (Tok_Right_Brace, "'}' expected");
               False_Lit.Lit_Next := True_Lit;
               Res.Enum_Lits := False_Lit;
               New_Boolean_Type (Res.Type_Onode,
                                 False_Lit.Lit_Name, False_Lit.Lit_Cnode,
                                 True_Lit.Lit_Name, True_Lit.Lit_Cnode);
            end;
         when Tok_Enum =>
            --  Grammar:
            --   ENUM { LIT1, LIT2, ... LITN }
            declare
               List : O_Enum_List;
               Lit : Node_Acc;
               Last_Lit : Node_Acc;
            begin
               Res := new Node'(Kind => Type_Enum,
                                Type_Onode => O_Tnode_Null,
                                Enum_Lits => null);
               Last_Lit := null;
               Push_Scope;
               Next_Expect (Tok_Left_Brace);
               Next_Token;
               --  FIXME: set a size to the enum.
               Start_Enum_Type (List, 8);
               loop
                  Expect (Tok_Ident);
                  Lit := new Node'(Kind => Node_Lit,
                                   Decl_Dtype => Res,
                                   Decl_Storage => O_Storage_Public,
                                   Decl_Defined => False,
                                   Lit_Name => Token_Sym.Ident,
                                   Lit_Cnode => O_Cnode_Null,
                                   Lit_Next => null);
                  Add_Decl (Token_Sym, Lit);
                  New_Enum_Literal (List, Lit.Lit_Name, Lit.Lit_Cnode);
                  if Last_Lit = null then
                     Res.Enum_Lits := Lit;
                  else
                     Last_Lit.Lit_Next := Lit;
                  end if;
                  Last_Lit := Lit;

                  Next_Token;
                  if Tok = Tok_Equal then
                     --  By compatibility, support '= N' after a literal.
                     Next_Expect (Tok_Num);
                     Next_Token;
                  end if;
                  exit when Tok = Tok_Right_Brace;
                  Expect (Tok_Comma);
                  Next_Token;
               end loop;
               Finish_Enum_Type (List, Res.Type_Onode);
               Pop_Scope;
            end;
         when others =>
            Parse_Error ("bad type " & Token_Type'Image (Tok));
            return null;
      end case;
      Next_Token;
      return Res;
   end Parse_Type;

   procedure Parse_Type_Completion (Decl : Node_Acc)
   is
   begin
      case Tok is
         when Tok_Record =>
            Next_Token;
            Parse_Record_Type (Decl.Decl_Dtype);
            Next_Token;
         when Tok_Access =>
            Next_Token;
            declare
               Dtype : Node_Acc;
            begin
               Dtype := Parse_Type;
               Decl.Decl_Dtype.Access_Dtype := Dtype;
               Finish_Access_Type (Decl.Decl_Dtype.Type_Onode,
                                   Dtype.Type_Onode);
            end;
         when others =>
            Parse_Error ("'access' or 'record' expected");
      end case;
   end Parse_Type_Completion;

--    procedure Parse_Declaration;

   procedure Parse_Expression (Expr_Type : Node_Acc;
                               Expr : out O_Enode;
                               Res_Type : out Node_Acc);
   procedure Parse_Name (Prefix : Node_Acc;
                         Name : out O_Lnode; N_Type : out Node_Acc);
   procedure Parse_Lvalue (N : in out O_Lnode; N_Type : in out Node_Acc);

   --  Expect: '('
   --  Let: next token.
   procedure Parse_Association (Constr : in out O_Assoc_List;
                                Decl : Node_Acc);

   function Find_Field_By_Name (Aggr_Type : Node_Acc) return Node_Acc
   is
      Map : Node_Map_Acc;
      Field : Node_Acc;
      Fields : Node_Array_Acc;
   begin
      case Aggr_Type.Kind is
         when Type_Record
           | Type_Union =>
            Map := Aggr_Type.Record_Union_Map;
            Fields := Aggr_Type.Record_Union_Fields;
         when Type_Subrecord =>
            Map := Aggr_Type.Subrecord_Base.Record_Union_Map;
            Fields := Aggr_Type.Subrecord_Fields;
         when others =>
            raise Program_Error;
      end case;

      if Map /= null then
         --  Look in the hash map if it is present.
         Field := Map.Map (Field_Map_Index (Map, Token_Sym));
         while Field /= null loop
            if Field.Field_Ident = Token_Sym then
               --  Get the field by position as the map is shared between
               --  a record and its subrecords.
               Field := Fields (Field.Field_Pos);
               exit;
            end if;
            Field := Field.Field_Hash_Next;
         end loop;
      else
         --  Linear look.
         Field := null;
         for I in Fields'Range loop
            if Fields (I).Field_Ident = Token_Sym then
               Field := Fields (I);
               exit;
            end if;
         end loop;
      end if;

      if Field = null then
         Parse_Error ("no such field name");
      end if;
      return Field;
   end Find_Field_By_Name;

   --  expect: offsetof id.
   function Parse_Offsetof (Atype : Node_Acc) return O_Cnode
   is
      Rec_Type : Node_Acc;
      Rec_Field : Node_Acc;
   begin
      Next_Expect (Tok_Left_Paren);
      Next_Expect (Tok_Ident);
      Rec_Type := Get_Decl (Token_Sym);
      if Rec_Type.Kind /= Decl_Type
        or else (Rec_Type.Decl_Dtype.Kind /= Type_Record
                   and then Rec_Type.Decl_Dtype.Kind /= Type_Subrecord)
      then
         Parse_Error ("record type name expected");
      end if;
      Next_Expect (Tok_Dot);
      Next_Expect (Tok_Ident);
      Rec_Field := Find_Field_By_Name (Rec_Type.Decl_Dtype);
      Next_Expect (Tok_Right_Paren);
      return New_Offsetof (Rec_Type.Decl_Dtype.Type_Onode,
                           Rec_Field.Field_Fnode,
                           Atype.Type_Onode);
   end Parse_Offsetof;

   function Parse_Type_Attribute return Node_Acc
   is
      Res : Node_Acc;
   begin
      Next_Expect (Tok_Left_Paren);
      Next_Token;
      if Tok /= Tok_Ident then
         Parse_Error ("type name expected");
      end if;
      Res := Get_Decl (Token_Sym).Decl_Dtype;
      Next_Expect (Tok_Right_Paren);
      return Res;
   end Parse_Type_Attribute;

   function Parse_Sizeof (Atype : Node_Acc) return O_Cnode
   is
      T : Node_Acc;
   begin
      T := Parse_Type_Attribute;
      return New_Sizeof (T.Type_Onode, Atype.Type_Onode);
   end Parse_Sizeof;

   function Parse_Record_Sizeof (Atype : Node_Acc) return O_Cnode
   is
      T : Node_Acc;
   begin
      T := Parse_Type_Attribute;
      return New_Record_Sizeof (T.Type_Onode, Atype.Type_Onode);
   end Parse_Record_Sizeof;

   function Parse_Alignof (Atype : Node_Acc) return O_Cnode
   is
      T : Node_Acc;
   begin
      T := Parse_Type_Attribute;
      return New_Alignof (T.Type_Onode, Atype.Type_Onode);
   end Parse_Alignof;

   function Parse_Minus_Num (Atype : Node_Acc) return O_Cnode
   is
      Res : O_Cnode;
      V : Integer_64;
   begin
      if Token_Number = Unsigned_64 (Integer_64'Last) + 1 then
         V := Integer_64'First;
      else
         V := -Integer_64 (Token_Number);
      end if;
      Res := New_Signed_Literal (Atype.Type_Onode, V);
      Next_Token;
      return Res;
   end Parse_Minus_Num;

   --  Parse a literal whose type is ATYPE.
   function Parse_Typed_Literal (Atype : Node_Acc) return O_Cnode
   is
      Res : O_Cnode;
   begin
      case Tok is
         when Tok_Num =>
            case Atype.Kind is
               when Type_Signed =>
                  Res := New_Signed_Literal
                    (Atype.Type_Onode, Integer_64 (Token_Number));
               when Type_Unsigned =>
                  Res := New_Unsigned_Literal
                    (Atype.Type_Onode, Token_Number);
               when others =>
                  Parse_Error ("bad type for integer literal");
            end case;
         when Tok_Minus =>
            Next_Token;
            case Tok is
               when Tok_Num =>
                  return Parse_Minus_Num (Atype);
               when Tok_Float_Num =>
                  Res := New_Float_Literal (Atype.Type_Onode, -Token_Float);
               when others =>
                  Parse_Error ("bad token after '-'");
            end case;
         when Tok_Float_Num =>
            Res := New_Float_Literal (Atype.Type_Onode, Token_Float);
         when Tok_Ident =>
            declare
               Pfx : Node_Acc;
               N : Node_Acc;
            begin
               --  Note: we don't use get_decl, since the name can be a literal
               --  name, which is not directly visible.
               if Token_Sym.Name /= null
                 and then Token_Sym.Name.Inter.Kind = Decl_Type
               then
                  --  A typed expression.
                  Pfx := Token_Sym.Name.Inter;
                  N := Pfx.Decl_Dtype;
                  if Atype /= null and then N /= Atype then
                     Parse_Error ("type mismatch");
                  end if;
                  Next_Expect (Tok_Tick);
                  Next_Token;
                  if Tok = Tok_Left_Brack then
                     Next_Token;
                     Res := Parse_Typed_Literal (N);
                     Expect (Tok_Right_Brack);
                  elsif Tok = Tok_Ident then
                     if Token_Sym = Id_Offsetof then
                        Res := Parse_Offsetof (N);
                     elsif Token_Sym = Id_Sizeof then
                        Res := Parse_Sizeof (N);
                     elsif Token_Sym = Id_Record_Sizeof then
                        Res := Parse_Record_Sizeof (N);
                     elsif Token_Sym = Id_Alignof then
                        Res := Parse_Alignof (N);
                     elsif Token_Sym = Id_Address
                       or Token_Sym = Id_Unchecked_Address
                       or Token_Sym = Id_Subprg_Addr
                     then
                        Res := Parse_Constant_Address (Pfx);
                     elsif Token_Sym = Id_Conv then
                        Next_Expect (Tok_Left_Paren);
                        Next_Token;
                        Res := Parse_Typed_Literal (N);
                        Expect (Tok_Right_Paren);
                     else
                        Parse_Error ("offsetof or sizeof attributes expected");
                     end if;
                  else
                     Parse_Error ("'[' or attribute expected");
                  end if;
               else
                  if Atype.Kind /= Type_Enum
                    and then Atype.Kind /= Type_Boolean
                  then
                     Parse_Error ("name allowed only for enumeration");
                  end if;
                  N := Atype.Enum_Lits;
                  while N /= null loop
                     if Is_Equal (N.Lit_Name, Token_Sym.Ident) then
                        Res := N.Lit_Cnode;
                        exit;
                     end if;
                     N := N.Lit_Next;
                  end loop;
                  if N = null then
                     Parse_Error ("no matching literal");
                     return O_Cnode_Null;
                  end if;
               end if;
            end;
         when Tok_Null =>
            Res := New_Null_Access (Atype.Type_Onode);
         when Tok_Default =>
            Res := New_Default_Value (Atype.Type_Onode);
         when others =>
            Parse_Error ("bad primary expression: " & Token_Type'Image (Tok));
            return O_Cnode_Null;
      end case;
      Next_Token;
      return Res;
   end Parse_Typed_Literal;

   --  expect: next token
   --  Parse an expression starting with NAME.
   procedure Parse_Named_Expression
     (Atype : Node_Acc; Name : Node_Acc; Stop_At_All : Boolean;
                                         Res : out O_Enode;
                                         Res_Type : out Node_Acc)
   is
   begin
      if Tok = Tok_Tick then
         Next_Token;
         if Tok = Tok_Left_Brack then
            --  Typed literal.
            Next_Token;
            Res := New_Lit (Parse_Typed_Literal (Name.Decl_Dtype));
            Res_Type := Name.Decl_Dtype;
            Expect (Tok_Right_Brack);
            Next_Token;
         elsif Tok = Tok_Left_Paren then
            --  Typed expression (used for comparaison operators)
            Next_Token;
            Parse_Expression (Name.Decl_Dtype, Res, Res_Type);
            Expect (Tok_Right_Paren);
            Next_Token;
         elsif Tok = Tok_Ident then
            --  Attribute.
            if Token_Sym = Id_Conv then
               declare
                  Ov : Boolean;
               begin
                  Next_Token;
                  if Tok = Tok_Sharp then
                     Ov := True;
                     Next_Token;
                  else
                     Ov := False;
                  end if;
                  Expect (Tok_Left_Paren);
                  Next_Token;
                  Parse_Expression (null, Res, Res_Type);
                  --  Discard Res_Type.
                  Expect (Tok_Right_Paren);
                  Next_Token;
                  Res_Type := Name.Decl_Dtype;
                  if Ov then
                     Res := New_Convert_Ov (Res, Res_Type.Type_Onode);
                  else
                     Res := New_Convert (Res, Res_Type.Type_Onode);
                  end if;
                  --  Fall-through.
               end;
            elsif Token_Sym = Id_Address
              or Token_Sym = Id_Unchecked_Address
              or Token_Sym = Id_Subprg_Addr
            then
               Res_Type := Name.Decl_Dtype;
               Res := Parse_Address (Name);
               --  Fall-through.
            elsif Token_Sym = Id_Sizeof then
               Res_Type := Name.Decl_Dtype;
               Res := New_Lit (Parse_Sizeof (Res_Type));
               Next_Token;
               return;
            elsif Token_Sym = Id_Record_Sizeof then
               Res_Type := Name.Decl_Dtype;
               Res := New_Lit (Parse_Record_Sizeof (Res_Type));
               Next_Token;
               return;
            elsif Token_Sym = Id_Alignof then
               Res_Type := Name.Decl_Dtype;
               Res := New_Lit (Parse_Alignof (Res_Type));
               Next_Token;
               return;
            elsif Token_Sym = Id_Alloca then
               Next_Expect (Tok_Left_Paren);
               Next_Token;
               Parse_Expression (null, Res, Res_Type);
               --  Discard Res_Type.
               Res_Type := Name.Decl_Dtype;
               Res := New_Alloca (Res_Type.Type_Onode, Res);
               Expect (Tok_Right_Paren);
               Next_Token;
               return;
            elsif Token_Sym = Id_Offsetof then
               Res_Type := Atype;
               Res := New_Lit (Parse_Offsetof (Res_Type));
               Next_Token;
               return;
            else
               Parse_Error ("unknown attribute name");
            end if;
            -- Fall-through.
         else
            Parse_Error ("typed expression expected");
         end if;
      elsif Tok = Tok_Left_Paren then
         if Name.Kind /= Node_Function then
            Parse_Error ("function name expected");
         end if;
         declare
            Constr : O_Assoc_List;
         begin
            Parse_Association (Constr, Name);
            Res := New_Function_Call (Constr);
            Res_Type := Name.Decl_Dtype;
            --  Fall-through.
         end;
      elsif Name.Kind = Node_Object
        or else Name.Kind = Decl_Param
      then
         --  Name.
         declare
            Lval : O_Lnode;
         begin
            Parse_Name (Name, Lval, Res_Type);
            Res := New_Value (Lval);
            if Atype /= null and then Res_Type /= Atype then
               Parse_Error ("type mismatch");
            end if;
         end;
      else
         Parse_Error ("bad ident expression: "
                      & Token_Type'Image (Tok));
      end if;

      -- Continue.
      --  R_TYPE and RES must be set.
      if Tok = Tok_Dot then
         if Stop_At_All then
            return;
         end if;
         Next_Token;
         if Tok = Tok_All then
            if Res_Type.Kind /= Type_Access then
               Parse_Error ("type of prefix is not an access");
            end if;
            declare
               N : O_Lnode;
            begin
               Next_Token;
               N := New_Access_Element (Res);
               Res_Type := Res_Type.Access_Dtype;
               Parse_Lvalue (N, Res_Type);
               Res := New_Value (N);
            end;
            return;
         else
            Parse_Error ("'.all' expected");
         end if;
      end if;
   end Parse_Named_Expression;

   procedure Parse_Primary_Expression (Atype : Node_Acc;
                                       Res : out O_Enode;
                                       Res_Type : out Node_Acc)
   is
   begin
      case Tok is
         when Tok_Num
           | Tok_Float_Num =>
            if Atype = null then
               Parse_Error ("numeric literal without type context");
            end if;
            Res_Type := Atype;
            Res := New_Lit (Parse_Typed_Literal (Atype));
         when Tok_Ident =>
            declare
               N : Node_Acc;
            begin
               N := Get_Decl (Token_Sym);
               Next_Token;
               Parse_Named_Expression (Atype, N, False, Res, Res_Type);
            end;
         when Tok_Left_Paren =>
            Next_Token;
            Parse_Expression (Atype, Res, Res_Type);
            Expect (Tok_Right_Paren);
            Next_Token;
         when others =>
            Parse_Error ("bad primary expression: " & Token_Type'Image (Tok));
      end case;
   end Parse_Primary_Expression;

   --  Parse '-' EXPR, 'not' EXPR, 'abs' EXPR or EXPR.
   procedure Parse_Unary_Expression (Atype : Node_Acc;
                                     Res : out O_Enode;
                                     Res_Type : out Node_Acc) is
   begin
      case Tok is
         when Tok_Minus =>
            Next_Token;
            if Tok = Tok_Num then
               if Atype = null then
                  Parse_Error ("numeric literal without type context");
               end if;
               Res := New_Lit (Parse_Minus_Num (Atype));
               Res_Type := Atype;
            else
               Parse_Unary_Expression (Atype, Res, Res_Type);
               Res := New_Monadic_Op (ON_Neg_Ov, Res);
            end if;
         when Tok_Not =>
            Next_Token;
            Parse_Unary_Expression (Atype, Res, Res_Type);
            Res := New_Monadic_Op (ON_Not, Res);
         when Tok_Abs =>
            Next_Token;
            Parse_Unary_Expression (Atype, Res, Res_Type);
            Res := New_Monadic_Op (ON_Abs_Ov, Res);
         when others =>
            Parse_Primary_Expression (Atype, Res, Res_Type);
      end case;
   end Parse_Unary_Expression;

   function Check_Sharp (Op_Ov : ON_Op_Kind) return ON_Op_Kind is
   begin
      Next_Expect (Tok_Sharp);
      Next_Token;
      return Op_Ov;
   end Check_Sharp;

   procedure Parse_Expression (Expr_Type : Node_Acc;
                               Expr : out O_Enode;
                               Res_Type : out Node_Acc)
   is
      Op_Type : Node_Acc;
      L : O_Enode;
      R : O_Enode;
      Op : ON_Op_Kind;
   begin
      if Expr_Type = null or else Expr_Type.Kind = Type_Boolean then
         --  The type of the expression isn't known, as this can be a
         --  comparaison operator.
         Op_Type := null;
      else
         Op_Type := Expr_Type;
      end if;
      Parse_Unary_Expression (Op_Type, L, Res_Type);
      case Tok is
         when Tok_Div =>
            Op := Check_Sharp (ON_Div_Ov);
         when Tok_Plus =>
            Op := Check_Sharp (ON_Add_Ov);
         when Tok_Minus =>
            Op := Check_Sharp (ON_Sub_Ov);
         when Tok_Star =>
            Op := Check_Sharp (ON_Mul_Ov);
         when Tok_Mod =>
            Op := Check_Sharp (ON_Mod_Ov);
         when Tok_Rem =>
            Op := Check_Sharp (ON_Rem_Ov);

         when Tok_Equal =>
            Op := ON_Eq;
         when Tok_Not_Equal =>
            Op := ON_Neq;
         when Tok_Greater =>
            Op := ON_Gt;
         when Tok_Greater_Eq =>
            Op := ON_Ge;
         when Tok_Less =>
            Op := ON_Lt;
         when Tok_Less_Eq =>
            Op := ON_Le;

         when Tok_Or =>
            Op := ON_Or;
            Next_Token;
         when Tok_And =>
            Op := ON_And;
            Next_Token;
         when Tok_Xor =>
            Op := ON_Xor;
            Next_Token;

         when others =>
            Expr := L;
            return;
      end case;
      if Op in ON_Compare_Op_Kind then
         Next_Token;
      end if;

      Parse_Unary_Expression (Res_Type, R, Res_Type);
      case Op is
         when ON_Dyadic_Op_Kind =>
            Expr := New_Dyadic_Op (Op, L, R);
         when ON_Compare_Op_Kind =>
            if Expr_Type = null then
               Parse_Error ("comparaison operator requires a type");
            end if;
            Expr := New_Compare_Op (Op, L, R, Expr_Type.Type_Onode);
            Res_Type := Expr_Type;
         when others =>
            raise Program_Error;
      end case;
   end Parse_Expression;

   procedure Check_Selected_Prefix (N_Type : Node_Acc) is
   begin
      case N_Type.Kind is
         when Type_Record
           | Type_Union
           | Type_Subrecord =>
            null;
         when others =>
            Parse_Error ("type of prefix is neither a record nor an union");
      end case;
   end Check_Selected_Prefix;

   --  Expect and leave: next token
   procedure Parse_Lvalue (N : in out O_Lnode; N_Type : in out Node_Acc) is
   begin
      loop
         case Tok is
            when Tok_Dot =>
               Next_Token;
               if Tok = Tok_All then
                  if N_Type.Kind /= Type_Access then
                     Parse_Error ("type of prefix is not an access");
                  end if;
                  N := New_Access_Element (New_Value (N));
                  N_Type := N_Type.Access_Dtype;
                  Next_Token;
               elsif Tok = Tok_Ident then
                  Check_Selected_Prefix (N_Type);
                  declare
                     Field : Node_Acc;
                  begin
                     Field := Find_Field_By_Name (N_Type);
                     N := New_Selected_Element (N, Field.Field_Fnode);
                     N_Type := Field.Field_Type;
                     Next_Token;
                  end;
               else
                  Parse_Error
                    ("'.' must be followed by 'all' or a field name");
               end if;
            when Tok_Left_Brack =>
               declare
                  V : O_Enode;
                  Bt : Node_Acc;
                  El_Type : Node_Acc;
                  Res_Type : Node_Acc;
               begin
                  Next_Token;
                  if N_Type.Kind = Type_Subarray then
                     Bt := N_Type.Subarray_Base;
                     El_Type := N_Type.Subarray_El;
                  else
                     Bt := N_Type;
                     El_Type := N_Type.Array_Element;
                  end if;
                  if Bt.Kind /= Type_Array then
                     Parse_Error ("type of prefix is not an array");
                  end if;
                  Parse_Expression (Bt.Array_Index, V, Res_Type);
                  if Tok = Tok_Elipsis then
                     N := New_Slice (N, Bt.Type_Onode, V);
                     Next_Token;
                  else
                     N := New_Indexed_Element (N, V);
                     N_Type := El_Type;
                  end if;
                  Expect (Tok_Right_Brack);
                  Next_Token;
               end;
            when others =>
               return;
         end case;
      end loop;
   end Parse_Lvalue;

   procedure Parse_Name (Prefix : Node_Acc;
                         Name : out O_Lnode; N_Type : out Node_Acc)
   is
   begin
      case Prefix.Kind is
         when Decl_Param =>
            Name := New_Obj (Prefix.Param_Node);
            N_Type := Prefix.Decl_Dtype;
         when Node_Object =>
            Name := New_Obj (Prefix.Obj_Node);
            N_Type := Prefix.Decl_Dtype;
         when Decl_Type =>
            declare
               Val : O_Enode;
            begin
               Parse_Named_Expression (null, Prefix, True, Val, N_Type);
               if N_Type /= Prefix.Decl_Dtype then
                  Parse_Error ("type doesn't match");
               end if;
               if Tok = Tok_Dot then
                  Next_Token;
                  if Tok = Tok_All then
                     if N_Type.Kind /= Type_Access then
                        Parse_Error ("type of prefix is not an access");
                     end if;
                     Name := New_Access_Element (Val);
                     N_Type := N_Type.Access_Dtype;
                     Next_Token;
                  else
                     Parse_Error ("'.all' expected");
                  end if;
               else
                  Parse_Error ("name expected");
               end if;
            end;
         when others =>
            Parse_Error ("invalid name");
      end case;
      Parse_Lvalue (Name, N_Type);
   end Parse_Name;

   --  Expect: '('
   --  Let: next token.
   procedure Parse_Association (Constr : in out O_Assoc_List; Decl : Node_Acc)
   is
      Param : Node_Acc;
      Expr : O_Enode;
      Expr_Type : Node_Acc;
   begin
      Start_Association (Constr, Decl.Subprg_Node);
      if Tok /= Tok_Left_Paren then
         Parse_Error ("'(' expected for a subprogram call");
      end if;
      Next_Token;
      Param := Decl.Subprg_Params;
      while Tok /= Tok_Right_Paren loop
         if Param = null then
            Parse_Error ("too many parameters");
         end if;
         Parse_Expression (Param.Decl_Dtype, Expr, Expr_Type);
         New_Association (Constr, Expr);
         Param := Param.Param_Next;
         exit when Tok /= Tok_Comma;
         Next_Token;
      end loop;
      if Param /= null then
         Parse_Error ("missing parameters");
      end if;
      if Tok /= Tok_Right_Paren then
         Parse_Error ("')' expected to finish a subprogram call, found "
                      & Token_Type'Image (Tok));
      end if;
      Next_Token;
   end Parse_Association;

   type Loop_Info;
   type Loop_Info_Acc is access Loop_Info;
   type Loop_Info is record
      Num : Natural;
      Blk : O_Snode;
      Prev : Loop_Info_Acc;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation
     (Name => Loop_Info_Acc, Object => Loop_Info);

   Loop_Stack : Loop_Info_Acc := null;

   function Find_Loop (N : Natural) return Loop_Info_Acc
   is
      Res : Loop_Info_Acc;
   begin
      Res := Loop_Stack;
      while Res /= null loop
         if Res.Num = N then
            return Res;
         end if;
         Res := Res.Prev;
      end loop;
      return null;
   end Find_Loop;

   Current_Subprg : Node_Acc := null;

   procedure Parse_Statement;

   --  Expect : next token
   --  Let: next token
   procedure Parse_Statements is
   begin
      loop
         exit when Tok = Tok_End;
         exit when Tok = Tok_Else;
         exit when Tok = Tok_When;
         Parse_Statement;
      end loop;
   end Parse_Statements;

   --  Expect : next token
   --  Let: next token
   procedure Parse_Statement is
   begin
      if Flag_Renumber then
         New_Debug_Line_Stmt (Lineno);
      end if;

      case Tok is
         when Tok_Comment =>
            Next_Token;

         when Tok_Declare =>
            Start_Declare_Stmt;
            Parse_Compound_Statement;
            Expect (Tok_Semicolon);
            Next_Token;
            Finish_Declare_Stmt;

         when Tok_Line_Number =>
            Next_Expect (Tok_Num);
            if Flag_Renumber = False then
               New_Debug_Line_Stmt (Natural (Token_Number));
            end if;
            Next_Token;

         when Tok_If =>
            declare
               If_Blk : O_If_Block;
               Cond : O_Enode;
               Cond_Type : Node_Acc;
            begin
               Next_Token;
               Parse_Expression (null, Cond, Cond_Type);
               Start_If_Stmt (If_Blk, Cond);
               Expect (Tok_Then);
               Next_Token;
               Parse_Statements;
               if Tok = Tok_Else then
                  Next_Token;
                  New_Else_Stmt (If_Blk);
                  Parse_Statements;
               end if;
               Finish_If_Stmt (If_Blk);
               Expect (Tok_End);
               Next_Expect (Tok_If);
               Next_Expect (Tok_Semicolon);
               Next_Token;
            end;

         when Tok_Loop =>
            --  Grammar:
            --    LOOP n:
            --      stmts
            --    END LOOP;
            declare
               Info : Loop_Info_Acc;
               Num : Natural;
            begin
               Next_Expect (Tok_Num);
               Num := Natural (Token_Number);
               if Find_Loop (Num) /= null then
                  Parse_Error ("loop label already defined");
               end if;
               Info := new Loop_Info;
               Info.Num := Num;
               Info.Prev := Loop_Stack;
               Loop_Stack := Info;
               Start_Loop_Stmt (Info.Blk);
               Next_Expect (Tok_Colon);
               Next_Token;
               Parse_Statements;
               Finish_Loop_Stmt (Info.Blk);
               Next_Expect (Tok_Loop);
               Next_Expect (Tok_Semicolon);
               Loop_Stack := Info.Prev;
               Free (Info);
               Next_Token;
            end;

         when Tok_Exit
           | Tok_Next =>
            --  Grammar:
            --    EXIT LOOP n;
            --    NEXT LOOP n;
            declare
               Label : Loop_Info_Acc;
               Etok : Token_Type;
            begin
               Etok := Tok;
               Next_Expect (Tok_Loop);
               Next_Expect (Tok_Num);
               Label := Find_Loop (Natural (Token_Number));
               if Label = null then
                  Parse_Error ("no such loop");
               end if;
               if Etok = Tok_Exit then
                  New_Exit_Stmt (Label.Blk);
               else
                  New_Next_Stmt (Label.Blk);
               end if;
               Next_Expect (Tok_Semicolon);
               Next_Token;
            end;

         when Tok_Return =>
            --  Grammar:
            --    RETURN;
            --    RETURN expr;
            declare
               Res : O_Enode;
               Res_Type : Node_Acc;
            begin
               Next_Token;
               if Tok /= Tok_Semicolon then
                  Parse_Expression (Current_Subprg.Decl_Dtype, Res, Res_Type);
                  New_Return_Stmt (Res);
                  if Tok /= Tok_Semicolon then
                     Parse_Error ("';' expected at end of return statement");
                  end if;
               else
                  New_Return_Stmt;
               end if;
               Next_Token;
            end;

         when Tok_Ident =>
            --  This is either a procedure call or an assignment.
            declare
               Inter : Node_Acc;
            begin
               Inter := Get_Decl (Token_Sym);
               Next_Token;
               if Tok = Tok_Left_Paren then
                  --  A procedure call.
                  declare
                     Constr : O_Assoc_List;
                  begin
                     Parse_Association (Constr, Inter);
                     New_Procedure_Call (Constr);
                     if Tok /= Tok_Semicolon then
                        Parse_Error ("';' expected after call");
                     end if;
                     Next_Token;
                     return;
                  end;
               else
                  --  An assignment.
                  declare
                     Name : O_Lnode;
                     Expr : O_Enode;
                     Expr_Type : Node_Acc;
                     N_Type : Node_Acc;
                  begin
                     Parse_Name (Inter, Name, N_Type);
                     if Tok /= Tok_Assign then
                        Parse_Error ("`:=' expected after a variable");
                     end if;
                     Next_Token;
                     Parse_Expression (N_Type, Expr, Expr_Type);
                     New_Assign_Stmt (Name, Expr);
                     if Tok /= Tok_Semicolon then
                        Parse_Error ("';' expected at end of assignment");
                     end if;
                     Next_Token;
                     return;
                  end;
               end if;
            end;

         when Tok_Case =>
            --  Grammar:
            --    CASE expr IS
            --      WHEN lit =>
            --      WHEN lit ... lit =>
            --      WHEN DEFAULT =>
            --    END CASE;
            declare
               Case_Blk : O_Case_Block;
               L : O_Cnode;
               Choice : O_Enode;
               Choice_Type : Node_Acc;
            begin
               Next_Token;
               Parse_Expression (null, Choice, Choice_Type);
               Start_Case_Stmt (Case_Blk, Choice);
               Expect (Tok_Is);
               Next_Token;
               loop
                  exit when Tok = Tok_End;
                  Expect (Tok_When);
                  Next_Token;
                  Start_Choice (Case_Blk);
                  loop
                     if Tok = Tok_Default then
                        New_Default_Choice (Case_Blk);
                        Next_Token;
                     else
                        L := Parse_Typed_Literal (Choice_Type);
                        if Tok = Tok_Elipsis then
                           Next_Token;
                           New_Range_Choice
                             (Case_Blk, L, Parse_Typed_Literal (Choice_Type));
                        else
                           New_Expr_Choice (Case_Blk, L);
                        end if;
                     end if;
                     exit when Tok = Tok_Arrow;
                     Expect (Tok_Comma);
                     Next_Token;
                  end loop;
                  --  Skip '=>'.
                  Next_Token;
                  Finish_Choice (Case_Blk);
                  Parse_Statements;
               end loop;
               Finish_Case_Stmt (Case_Blk);
               Expect (Tok_End);
               Next_Expect (Tok_Case);
               Next_Expect (Tok_Semicolon);
               Next_Token;
            end;
         when others =>
            Parse_Error ("bad statement: " & Token_Type'Image (Tok));
      end case;
   end Parse_Statement;

   procedure Parse_Compound_Statement is
   begin
      if Tok /= Tok_Declare then
         Parse_Error ("'declare' expected to start a statements block");
      end if;
      Next_Token;

      Push_Scope;

      --  Parse declarations.
      while Tok /= Tok_Begin loop
         Parse_Declaration;
      end loop;
      Next_Token;

      --  Parse statements.
      Parse_Statements;
      Expect (Tok_End);
      Next_Token;

      Pop_Scope;
   end Parse_Compound_Statement;

   --  Parse (P1 : T1; P2: T2; ...)
   function Parse_Parameter_List return Node_Acc
   is
      First, Last : Node_Acc;
      P : Node_Acc;
   begin
      Expect (Tok_Left_Paren);
      Next_Token;
      if Tok = Tok_Right_Paren then
         Next_Token;
         return null;
      end if;
      First := null;
      Last := null;
      loop
         Expect (Tok_Ident);
         P := new Node'(Kind => Decl_Param,
                        Decl_Dtype => null,
                        Decl_Storage => O_Storage_Public,
                        Decl_Defined => False,
                        Param_Node => O_Dnode_Null,
                        Param_Name => Token_Sym,
                        Param_Next => null);
         --  Link
         if Last = null then
            First := P;
         else
            Last.Param_Next := P;
         end if;
         Last := P;
         Next_Expect (Tok_Colon);
         Next_Token;
         P.Decl_Dtype := Parse_Type;
         exit when Tok = Tok_Right_Paren;
         Expect (Tok_Semicolon);
         Next_Token;
      end loop;
      Next_Token;
      return First;
   end Parse_Parameter_List;

   procedure Create_Interface_List (Constr : in out O_Inter_List;
                                    First_Inter : Node_Acc)
   is
      Inter : Node_Acc;
   begin
      Inter := First_Inter;
      while Inter /= null loop
         New_Interface_Decl (Constr, Inter.Param_Node, Inter.Param_Name.Ident,
                             Inter.Decl_Dtype.Type_Onode);
         Inter := Inter.Param_Next;
      end loop;
   end Create_Interface_List;

   procedure Check_Parameter_List (List : Node_Acc)
   is
      Param : Node_Acc;
   begin
      Next_Expect (Tok_Left_Paren);
      Next_Token;
      Param := List;
      while Tok /= Tok_Right_Paren loop
         if Param = null then
            Parse_Error ("subprogram redefined with more parameters");
         end if;
         Expect (Tok_Ident);
         if Token_Sym /= Param.Param_Name then
            Parse_Error ("subprogram redefined with different parameter name");
         end if;
         Next_Expect (Tok_Colon);
         Next_Token;
         if Parse_Type /= Param.Decl_Dtype then
            Parse_Error ("subprogram redefined with different parameter type");
         end if;
         Param := Param.Param_Next;
         exit when Tok = Tok_Right_Paren;
         Expect (Tok_Semicolon);
         Next_Token;
      end loop;
      Expect (Tok_Right_Paren);
      Next_Token;
      if Param /= null then
         Parse_Error ("subprogram redefined with less parameters");
      end if;
   end Check_Parameter_List;

   procedure Parse_Subprogram_Body (Subprg : Node_Acc)
   is
      Param : Node_Acc;
      Prev_Subprg : Node_Acc;
   begin
      Prev_Subprg := Current_Subprg;
      Current_Subprg := Subprg;

      Start_Subprogram_Body (Subprg.Subprg_Node);
      Push_Scope;

      --  Put parameters in the current scope.
      Param := Subprg.Subprg_Params;
      while Param /= null loop
         Add_Decl (Param.Param_Name, Param);
         Param := Param.Param_Next;
      end loop;

      Parse_Compound_Statement;

      Pop_Scope;
      Finish_Subprogram_Body;

      Current_Subprg := Prev_Subprg;
   end Parse_Subprogram_Body;

   procedure Parse_Function_Definition (Storage : O_Storage)
   is
      Constr : O_Inter_List;
      Sym : Syment_Acc;
      N : Node_Acc;
   begin
      Expect (Tok_Function);
      Next_Expect (Tok_Ident);
      Sym := Token_Sym;
      if Sym.Name /= null then
         N := Get_Decl (Sym);
         Check_Parameter_List (N.Subprg_Params);
         Expect (Tok_Return);
         Next_Expect (Tok_Ident);
         Next_Token;
      else
         N := new Node'(Kind => Node_Function,
                        Decl_Dtype => null,
                        Decl_Storage => Storage,
                        Decl_Defined => False,
                        Subprg_Node => O_Dnode_Null,
                        Subprg_Name => Sym,
                        Subprg_Params => null);
         Next_Token;
         N.Subprg_Params := Parse_Parameter_List;
         Expect (Tok_Return);
         Next_Token;
         N.Decl_Dtype := Parse_Type;

         Start_Function_Decl (Constr, N.Subprg_Name.Ident, Storage,
                              N.Decl_Dtype.Type_Onode);
         Create_Interface_List (Constr, N.Subprg_Params);
         Finish_Subprogram_Decl (Constr, N.Subprg_Node);

         Add_Decl (Sym, N);
      end if;

      if Tok = Tok_Declare then
         Parse_Subprogram_Body (N);
      end if;
   end Parse_Function_Definition;

   procedure Parse_Procedure_Definition (Storage : O_Storage)
   is
      Constr : O_Inter_List;
      Sym : Syment_Acc;
      N : Node_Acc;
   begin
      Expect (Tok_Procedure);
      Next_Expect (Tok_Ident);
      Sym := Token_Sym;
      if Sym.Name /= null then
         N := Get_Decl (Sym);
         Check_Parameter_List (N.Subprg_Params);
      else
         N := new Node'(Kind => Node_Procedure,
                        Decl_Dtype => null,
                        Decl_Storage => Storage,
                        Decl_Defined => False,
                        Subprg_Node => O_Dnode_Null,
                        Subprg_Name => Sym,
                        Subprg_Params => null);
         Next_Token;
         N.Subprg_Params := Parse_Parameter_List;

         Start_Procedure_Decl (Constr, N.Subprg_Name.Ident, Storage);
         Create_Interface_List (Constr, N.Subprg_Params);
         Finish_Subprogram_Decl (Constr, N.Subprg_Node);

         Add_Decl (Sym, N);
      end if;

      if Tok = Tok_Declare then
         Parse_Subprogram_Body (N);
      end if;
   end Parse_Procedure_Definition;

   function Parse_Address (Prefix : Node_Acc) return O_Enode
   is
      Pfx : Node_Acc;
      N : O_Lnode;
      N_Type : Node_Acc;
      Res : O_Enode;
      Attr : Syment_Acc;
      T : O_Tnode;
   begin
      Attr := Token_Sym;
      Next_Expect (Tok_Left_Paren);
      Next_Expect (Tok_Ident);
      Pfx := Get_Decl (Token_Sym);
      T := Prefix.Decl_Dtype.Type_Onode;
      if Attr = Id_Subprg_Addr then
         Expect (Tok_Ident);
         Pfx := Get_Decl (Token_Sym);
         if Pfx.Kind not in Nodes_Subprogram then
            Parse_Error ("subprogram identifier expected");
         end if;
         Res := New_Lit (New_Subprogram_Address (Pfx.Subprg_Node, T));
         Next_Token;
      else
         Next_Token;
         Parse_Name (Pfx, N, N_Type);
         if Attr = Id_Address then
            Res := New_Address (N, T);
         elsif Attr = Id_Unchecked_Address then
            Res := New_Unchecked_Address (N, T);
         else
            Parse_Error ("address attribute expected");
         end if;
      end if;
      Expect (Tok_Right_Paren);
      Next_Token;
      return Res;
   end Parse_Address;

   procedure Parse_Global_Name (Prefix : Node_Acc;
                                Name : out O_Gnode; N_Type : out Node_Acc)
   is
   begin
      case Prefix.Kind is
         when Node_Object =>
            Name := New_Global (Prefix.Obj_Node);
            N_Type := Prefix.Decl_Dtype;
         when others =>
            Parse_Error ("invalid name");
      end case;

      loop
         case Tok is
            when Tok_Dot =>
               Next_Token;
               if Tok = Tok_Ident then
                  Check_Selected_Prefix (N_Type);
                  declare
                     Field : Node_Acc;
                  begin
                     Field := Find_Field_By_Name (N_Type);
                     Name := New_Global_Selected_Element (Name,
                                                          Field.Field_Fnode);
                     N_Type := Field.Field_Type;
                     Next_Token;
                  end;
               else
                  Parse_Error ("'.' must be followed by a field name");
               end if;
            when others =>
               return;
         end case;
      end loop;
   end Parse_Global_Name;

   function Parse_Constant_Address (Prefix : Node_Acc) return O_Cnode
   is
      Pfx : Node_Acc;
      Res : O_Cnode;
      Attr : Syment_Acc;
      T : O_Tnode;
      N : O_Gnode;
      N_Type : Node_Acc;
   begin
      Attr := Token_Sym;
      Next_Expect (Tok_Left_Paren);
      Next_Expect (Tok_Ident);
      Pfx := Get_Decl (Token_Sym);
      T := Prefix.Decl_Dtype.Type_Onode;
      if Attr = Id_Subprg_Addr then
         Expect (Tok_Ident);
         Pfx := Get_Decl (Token_Sym);
         if Pfx.Kind not in Nodes_Subprogram then
            Parse_Error ("subprogram identifier expected");
         end if;
         Res := New_Subprogram_Address (Pfx.Subprg_Node, T);
         Next_Token;
      else
         Next_Token;
         Parse_Global_Name (Pfx, N, N_Type);
         if Attr = Id_Address then
            Res := New_Global_Address (N, T);
         elsif Attr = Id_Unchecked_Address then
            Res := New_Global_Unchecked_Address (N, T);
         else
            Parse_Error ("address attribute expected");
         end if;
      end if;
      Expect (Tok_Right_Paren);
      return Res;
   end Parse_Constant_Address;

   function Parse_Array_Aggregate (Aggr_Type : Node_Acc; El_Type : Node_Acc)
                                  return O_Cnode
   is
      Res : O_Cnode;
      Constr : O_Array_Aggr_List;
      Len : Unsigned_32;
   begin
      --  Parse '[' LEN ']'
      Expect (Tok_Left_Brack);
      Next_Token;
      Expect (Tok_Num);
      Len := Unsigned_32 (Token_Number);
      Next_Token;
      Expect (Tok_Right_Brack);
      Next_Token;

      Expect (Tok_Left_Brace);
      Next_Token;
      Start_Array_Aggr (Constr, Aggr_Type.Type_Onode, Len);
      for I in Unsigned_32 loop
         if Tok = Tok_Right_Brace then
            if I /= Len then
               Parse_Error ("bad number of aggregate element");
            end if;
            exit;
         end if;

         if I /= 0 then
            Expect (Tok_Comma);
            Next_Token;
         end if;
         New_Array_Aggr_El (Constr, Parse_Constant_Value (El_Type));
      end loop;
      Finish_Array_Aggr (Constr, Res);
      Next_Token;
      return Res;
   end Parse_Array_Aggregate;

   function Parse_Constant_Value (Atype : Node_Acc) return O_Cnode
   is
      Res : O_Cnode;
   begin
      case Atype.Kind is
         when Type_Subarray =>
            return Parse_Array_Aggregate
              (Atype, Atype.Subarray_Base.Array_Element);
         when Type_Array =>
            return Parse_Array_Aggregate (Atype, Atype.Array_Element);
         when Type_Unsigned
           | Type_Signed
           | Type_Enum
           | Type_Float
           | Type_Boolean
           | Type_Access =>
            --return Parse_Primary_Expression (Atype);
            return Parse_Typed_Literal (Atype);
         when Type_Record =>
            if Tok = Tok_Ident then
               --  Default value ?
               return Parse_Typed_Literal (Atype);
            end if;

            declare
               Constr : O_Record_Aggr_List;
               Fields : Node_Array_Acc;
            begin
               Expect (Tok_Left_Brace);
               Next_Token;
               Start_Record_Aggr (Constr, Atype.Type_Onode);
               Fields := Atype.Record_Union_Fields;
               for I in Fields'Range loop
                  if I /= 1 then
                     Expect (Tok_Comma);
                     Next_Token;
                  end if;
                  if Tok = Tok_Dot then
                     Next_Expect (Tok_Ident);
                     if Token_Sym /= Fields (I).Field_Ident then
                        Parse_Error ("bad field name");
                     end if;
                     Next_Expect (Tok_Equal);
                     Next_Token;
                  end if;
                  New_Record_Aggr_El
                    (Constr, Parse_Constant_Value (Fields (I).Field_Type));
               end loop;
               Finish_Record_Aggr (Constr, Res);
               Expect (Tok_Right_Brace);
               Next_Token;
               return Res;
            end;

         when Type_Union =>
            if Tok = Tok_Ident then
               --  Default value ?
               return Parse_Typed_Literal (Atype);
            end if;
            declare
               Field : Node_Acc;
            begin
               Expect (Tok_Left_Brace);
               Next_Token;
               Expect (Tok_Dot);
               Next_Expect (Tok_Ident);
               Field := Find_Field_By_Name (Atype);
               Next_Expect (Tok_Equal);
               Next_Token;
               Res := New_Union_Aggr
                 (Atype.Type_Onode, Field.Field_Fnode,
                  Parse_Constant_Value (Field.Field_Type));
               Expect (Tok_Right_Brace);
               Next_Token;
               return Res;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Parse_Constant_Value;

   procedure Parse_Constant_Declaration (Storage : O_Storage)
   is
      N : Node_Acc;
      Sym : Syment_Acc;
      Val : O_Cnode;
   begin
      Expect (Tok_Constant);
      Next_Expect (Tok_Ident);
      Sym := Token_Sym;
      N := new Node'(Kind => Node_Object,
                     Decl_Dtype => null,
                     Decl_Storage => Storage,
                     Decl_Defined => False,
                     Obj_Name => Sym.Ident,
                     Obj_Node => O_Dnode_Null);
      Next_Expect (Tok_Colon);
      Next_Token;
      N.Decl_Dtype := Parse_Type;
      New_Const_Decl (N.Obj_Node, Sym.Ident, Storage, N.Decl_Dtype.Type_Onode);
      Add_Decl (Sym, N);

      if Tok = Tok_Assign then
         N.Decl_Defined := True;
         Next_Token;

         Start_Init_Value (N.Obj_Node);
         Val := Parse_Constant_Value (N.Decl_Dtype);
         Finish_Init_Value (N.Obj_Node, Val);
      end if;
   end Parse_Constant_Declaration;

   --  Grammar:
   --    CONSTANT ident := value ;
   procedure Parse_Constant_Value_Declaration
   is
      N : Node_Acc;
      Val : O_Cnode;
   begin
      Next_Expect (Tok_Ident);
      N := Get_Decl (Token_Sym);
      if N.Kind /= Node_Object then
         Parse_Error ("name of a constant expected");
      end if;
      if N.Decl_Defined then
         Parse_Error ("constant already defined");
      else
         N.Decl_Defined := True;
      end if;
      --  FIXME: should check storage,
      --         should check the object is a constant,
      --         should check the object has no value.
      Next_Expect (Tok_Assign);
      Next_Token;
      Start_Init_Value (N.Obj_Node);
      Val := Parse_Constant_Value (N.Decl_Dtype);
      Finish_Init_Value (N.Obj_Node, Val);
   end Parse_Constant_Value_Declaration;

   procedure Parse_Var_Declaration (Storage : O_Storage)
   is
      N : Node_Acc;
      Sym : Syment_Acc;
   begin
      Expect (Tok_Var);
      Next_Expect (Tok_Ident);
      Sym := Token_Sym;
      N := new Node'(Kind => Node_Object,
                     Decl_Dtype => null,
                     Decl_Storage => Storage,
                     Decl_Defined => False,
                     Obj_Name => Sym.Ident,
                     Obj_Node => O_Dnode_Null);
      Next_Expect (Tok_Colon);
      Next_Token;
      N.Decl_Dtype := Parse_Type;
      New_Var_Decl (N.Obj_Node, Sym.Ident, Storage, N.Decl_Dtype.Type_Onode);
      Add_Decl (Sym, N);
   end Parse_Var_Declaration;

   procedure Parse_Stored_Decl (Storage : O_Storage)
   is
   begin
      Next_Token;
      if Tok = Tok_Function then
         Parse_Function_Definition (Storage);
      elsif Tok = Tok_Procedure then
         Parse_Procedure_Definition (Storage);
      elsif Tok = Tok_Constant then
         Parse_Constant_Declaration (Storage);
      elsif Tok = Tok_Var then
         Parse_Var_Declaration (Storage);
      else
         Parse_Error ("function or object declaration expected");
      end if;
   end Parse_Stored_Decl;

   procedure Parse_Declaration
   is
      Inter : Node_Acc;
      S : Syment_Acc;
   begin
      if Flag_Renumber then
         New_Debug_Line_Decl (Lineno);
      end if;

      case Tok is
         when Tok_Type =>
            Next_Token;
            if Tok /= Tok_Ident then
               Parse_Error ("identifier for type expected");
            end if;
            S := Token_Sym;
            Next_Expect (Tok_Is);
            Next_Token;
            if Is_Defined (S) then
               Parse_Type_Completion (Get_Decl (S));
            else
               Inter := new Node'(Kind => Decl_Type,
                                  Decl_Storage => O_Storage_Public,
                                  Decl_Defined => False,
                                  Decl_Dtype => Parse_Type);
               Add_Decl (S, Inter);
               New_Type_Decl (S.Ident, Inter.Decl_Dtype.Type_Onode);
            end if;
         when Tok_External =>
            Parse_Stored_Decl (O_Storage_External);
         when Tok_Private =>
            Parse_Stored_Decl (O_Storage_Private);
         when Tok_Public =>
            Parse_Stored_Decl (O_Storage_Public);
         when Tok_Local =>
            Parse_Stored_Decl (O_Storage_Local);
         when Tok_Constant =>
            Parse_Constant_Value_Declaration;
         when Tok_Comment =>
            New_Debug_Comment_Decl (Token_Ident (1 .. Token_Idlen));
            Next_Token;
            return;
         when Tok_File_Name =>
            if Flag_Renumber = False then
               New_Debug_Filename_Decl (Token_Ident (1 .. Token_Idlen));
            end if;
            Next_Token;
            return;
         when Tok_Line_Number =>
            Next_Expect (Tok_Num);
            if Flag_Renumber = False then
               New_Debug_Line_Decl (Natural (Token_Number));
            end if;
            Next_Token;
            return;
         when others =>
            Parse_Error ("declaration expected");
      end case;
      Expect (Tok_Semicolon);
      Next_Token;
   end Parse_Declaration;

--    procedure Put (Str : String)
--    is
--       L : Integer;
--    begin
--       L := Write (Standout, Str'Address, Str'Length);
--    end Put;

   function Parse (Filename : String_Acc) return Boolean is
   begin
      --  Create the symbol hash table.
      Symtable := new Syment_Acc_Map (Hash_Primes (Cur_Prime_Idx));

      --  Initialize symbol table.
      Add_Keyword ("type", Tok_Type);
      Add_Keyword ("return", Tok_Return);
      Add_Keyword ("if", Tok_If);
      Add_Keyword ("then", Tok_Then);
      Add_Keyword ("else", Tok_Else);
      Add_Keyword ("elsif", Tok_Elsif);
      Add_Keyword ("loop", Tok_Loop);
      Add_Keyword ("exit", Tok_Exit);
      Add_Keyword ("next", Tok_Next);
      Add_Keyword ("signed", Tok_Signed);
      Add_Keyword ("unsigned", Tok_Unsigned);
      Add_Keyword ("float", Tok_Float);
      Add_Keyword ("is", Tok_Is);
      Add_Keyword ("of", Tok_Of);
      Add_Keyword ("all", Tok_All);
      Add_Keyword ("not", Tok_Not);
      Add_Keyword ("abs", Tok_Abs);
      Add_Keyword ("or", Tok_Or);
      Add_Keyword ("and", Tok_And);
      Add_Keyword ("xor", Tok_Xor);
      Add_Keyword ("mod", Tok_Mod);
      Add_Keyword ("rem", Tok_Rem);
      Add_Keyword ("array", Tok_Array);
      Add_Keyword ("access", Tok_Access);
      Add_Keyword ("record", Tok_Record);
      Add_Keyword ("subrecord", Tok_Subrecord);
      Add_Keyword ("union", Tok_Union);
      Add_Keyword ("end", Tok_End);
      Add_Keyword ("boolean", Tok_Boolean);
      Add_Keyword ("enum", Tok_Enum);
      Add_Keyword ("external", Tok_External);
      Add_Keyword ("private", Tok_Private);
      Add_Keyword ("public", Tok_Public);
      Add_Keyword ("local", Tok_Local);
      Add_Keyword ("procedure", Tok_Procedure);
      Add_Keyword ("function", Tok_Function);
      Add_Keyword ("constant", Tok_Constant);
      Add_Keyword ("var", Tok_Var);
      Add_Keyword ("subarray", Tok_Subarray);
      Add_Keyword ("declare", Tok_Declare);
      Add_Keyword ("begin", Tok_Begin);
      Add_Keyword ("end", Tok_End);
      Add_Keyword ("null", Tok_Null);
      Add_Keyword ("case", Tok_Case);
      Add_Keyword ("when", Tok_When);
      Add_Keyword ("default", Tok_Default);

      Id_Address := New_Symbol ("address");
      Id_Unchecked_Address := New_Symbol ("unchecked_address");
      Id_Subprg_Addr := New_Symbol ("subprg_addr");
      Id_Conv := New_Symbol ("conv");
      Id_Sizeof := New_Symbol ("sizeof");
      Id_Record_Sizeof := New_Symbol ("record_sizeof");
      Id_Alignof := New_Symbol ("alignof");
      Id_Alloca := New_Symbol ("alloca");
      Id_Offsetof := New_Symbol ("offsetof");

      --  Initialize the scanner.
      Buf (1) := NUL;
      Pos := 1;
      Lineno := 1;
      if Filename = null then
         Fd := Standin;
         File_Name := new String'("*stdin*");
      else
         declare
            Name : String (1 .. Filename'Length + 1);
         begin
            Name (1 .. Filename'Length) := Filename.all;
            Name (Name'Last) := NUL;
            File_Name := Filename;
            Fd := Open_Read (Name'Address, Text);
            if Fd = Invalid_FD then
               Puterr ("cannot open '" & Filename.all & ''');
               Newline_Err;
               return False;
            end if;
         end;
      end if;

      New_Debug_Filename_Decl (File_Name.all);

      Push_Scope;
      Next_Token;
      while Tok /= Tok_Eof loop
         Parse_Declaration;
      end loop;
      Pop_Scope;

      if Fd /= Standin then
         Close (Fd);
      end if;
      return True;
   exception
      when Error =>
         return False;
      when E : others =>
         Puterr (Ada.Exceptions.Exception_Information (E));
         raise;
   end Parse;
end Ortho_Front;
