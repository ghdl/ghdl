--  Name table.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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

with System; use System;

package body Name_Table is
   --  Id of the first character (NUL).
   First_Character_Name_Id : constant Name_Id := 1;

   type Names_Table_Type is null record;
   type Names_Table_Acc is access Names_Table_Type;
   pragma Convention (C, Names_Table_Acc);

   Inst : Names_Table_Acc;

   --  Binding to functions defined in sintern.rs

   function Sintern_New_Interner (Cap : Natural) return Names_Table_Acc;
   pragma Import (C, Sintern_New_Interner);

   procedure Sintern_Delete_Interner (Inst : Names_Table_Acc);
   pragma Import (C, Sintern_Delete_Interner);

   function Sintern_Get_Identifier_Extra_With_Len
     (Inst : Names_Table_Acc; Name : Address; Len : Natural) return Name_Id;
   pragma Import (C, Sintern_Get_Identifier_Extra_With_Len);

   function Sintern_Get_Identifier_No_Create_With_Len
     (Inst : Names_Table_Acc; Name : Thin_String_Ptr; Len : Natural)
     return Name_Id;
   pragma Import (C, Sintern_Get_Identifier_No_Create_With_Len);

   function Sintern_Get_Identifier_With_Len
     (Inst : Names_Table_Acc; Name : Thin_String_Ptr; Len : Natural)
     return Name_Id;
   pragma Import (C, Sintern_Get_Identifier_With_Len);

   function Sintern_Get_Length (Inst : Names_Table_Acc; Id : Name_Id)
                               return Natural;
   pragma Import (C, Sintern_Get_Length);

   function Sintern_Get_Address (Inst : Names_Table_Acc; Id : Name_Id)
                                return Address;
   pragma Import (C, Sintern_Get_Address);

   function Sintern_Get_Last (Inst : Names_Table_Acc) return Name_Id;
   pragma Import (C, Sintern_Get_Last);

   function Sintern_Get_Info (Inst : Names_Table_Acc; Id : Name_Id)
                             return Int32;
   pragma Import (C, Sintern_Get_Info);

   procedure Sintern_Set_Info
     (Inst : Names_Table_Acc; Id : Name_Id; Info : Int32);
   pragma Import (C, Sintern_Set_Info);

   procedure Initialize
   is
      S : String (1 .. 2);
      Res : Name_Id;
   begin
      Inst := Sintern_New_Interner (32 * 1024);

      --  Reserve entry 0 for Null_Identifier.
      Res := Sintern_Get_Identifier_Extra_With_Len (Inst, S'Address, 0);
      pragma Assert (Res = Null_Identifier);

      --  Store characters.  They aren't put in the hash table.
      --  Note: the characters use ISO-8859-1, which are the same as the
      --   first 256 characters of unicode.
      for C in Character loop
         if C < Character'Val (128) then
            S (1) := C;
            Res := Sintern_Get_Identifier_Extra_With_Len (Inst, S'Address, 1);
         else
            S (1) := Character'Val (16#c0# + Character'Pos (C) / 64);
            S (2) := Character'Val (16#80# + Character'Pos (C) mod 64);
            Res := Sintern_Get_Identifier_Extra_With_Len (Inst, S'Address, 2);
         end if;
         pragma Assert (Res = First_Character_Name_Id + Character'Pos (C));
      end loop;
   end Initialize;

   procedure Finalize is
   begin
      Sintern_Delete_Interner (Inst);
      Inst := null;
   end Finalize;

   --  Get the string associed to an identifier.
   function Image (Id : Name_Id) return String is
   begin
      if Is_Character (Id) then
         return ''' & Get_Character (Id) & ''';
      else
         declare
            Len : constant Natural := Get_Name_Length ( Id);
            Addr : constant Address := Get_Address (Id);
            Res : String (1 .. Len);
            pragma Import (Ada, Res);
            for Res'Address use Addr;
         begin
            return Res;
         end;
      end if;
   end Image;

   --  Get the address of the first character of ID.
   --  The string is NUL-terminated (this is done by get_identifier).
   function Get_Address (Id : Name_Id) return System.Address is
   begin
      return Sintern_Get_Address (Inst, Id);
   end Get_Address;

   function Get_Name_Ptr (Id : Name_Id) return Thin_String_Ptr is
   begin
      return To_Thin_String_Ptr (Get_Address (Id));
   end Get_Name_Ptr;

   function Get_Name_Length (Id : Name_Id) return Natural is
   begin
      return Sintern_Get_Length (Inst, Id);
   end Get_Name_Length;

   function Is_Character (Id : Name_Id) return Boolean is
   begin
      return Id >= First_Character_Name_Id
        and then
        Id <= First_Character_Name_Id + Character'Pos (Character'Last);
   end Is_Character;

   --  Get the character associed to an identifier.
   function Get_Character (Id : Name_Id) return Character is
   begin
      pragma Assert (Is_Character (Id));
      return Character'Val (Id - First_Character_Name_Id);
   end Get_Character;

   --  Get and set the info field associated with each identifier.
   --  Used to store interpretations of the name.
   function Get_Name_Info (Id : Name_Id) return Int32 is
   begin
      return Sintern_Get_Info (Inst, Id);
   end Get_Name_Info;

   procedure Set_Name_Info (Id : Name_Id; Info : Int32) is
   begin
      Sintern_Set_Info (Inst, Id, Info);
   end Set_Name_Info;

   --  Get or create an entry in the name table.
   function Get_Identifier_With_Len (Str : Thin_String_Ptr; Len : Natural)
                                    return Name_Id is
   begin
      return Sintern_Get_Identifier_With_Len (Inst, Str, Len);
   end Get_Identifier_With_Len;

   function Get_Identifier_No_Create_With_Len
     (Str : Thin_String_Ptr; Len : Natural) return Name_Id is
   begin
      return Sintern_Get_Identifier_No_Create_With_Len (Inst, Str, Len);
   end Get_Identifier_No_Create_With_Len;

   function Get_Identifier_No_Create (Str : String) return Name_Id is
   begin
      return Get_Identifier_No_Create_With_Len
        (To_Thin_String_Ptr (Str'Address), Str'Length);
   end Get_Identifier_No_Create;

   --  Get or create an entry in the name table.
   function Get_Identifier (Str : String) return Name_Id is
   begin
      return Get_Identifier_With_Len
        (To_Thin_String_Ptr (Str'Address), Str'Length);
   end Get_Identifier;

   function Get_Identifier (Char : Character) return Name_Id is
   begin
      return First_Character_Name_Id + Character'Pos (Char);
   end Get_Identifier;

   --  Return the latest name_id used.
   --  kludge, use only for debugging.
   function Last_Name_Id return Name_Id is
   begin
      return Sintern_Get_Last (Inst);
   end Last_Name_Id;

   procedure Disp_Stats is
   begin
      null;
   end Disp_Stats;
end Name_Table;
