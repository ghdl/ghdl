--  Name table.
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
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Table;

package body Name_Table is
   -- A flag that creates verbosity.
   Debug_Name_Table: constant Boolean := False;

   First_Character_Name_Id : constant Name_Id := 1;

   type Hash_Value_Type is mod 2**32;

   -- An entry in the name table.
   type Identifier is record
      Hash: Hash_Value_Type;
      Next: Name_Id;

      --  FIXME: to be removed (compute from name of next identifier).
      Length: Natural;

      --  Index in strings_table.
      Name: Natural;

      --  User infos.
      Info: Int32;
   end record;

   -- Hash table.
   -- Number of entry points.
   Hash_Table_Size: constant Hash_Value_Type := 1024;
   Hash_Table: array (0 .. Hash_Table_Size - 1) of Name_Id;

   -- The table to store all the strings.
   package Strings_Table is new GNAT.Table
     (Table_Index_Type => Natural,
      Table_Component_Type => Character,
      Table_Low_Bound => Natural'First,
      Table_Initial => 4096,
      Table_Increment => 100);

   -- A NUL character is stored after each word in the strings_table.
   -- This is used for compatibility with C.
   NUL: constant Character := Character'Val (0);

   -- Allocate place in the strings_table, and store the name_buffer into it.
   -- Also append a NUL.
   function Store return Natural is
      Res: Natural;
   begin
      Res := Strings_Table.Allocate (Name_Length + 1);
      Strings_Table.Table (Res .. Res + Name_Length - 1) :=
        Strings_Table.Table_Type (Name_Buffer (1 .. Name_Length));
      Strings_Table.Table (Res + Name_Length) := NUL;
      return Res;
   end Store;

   package Names_Table is new GNAT.Table
     (Table_Index_Type => Name_Id,
      Table_Component_Type => Identifier,
      Table_Low_Bound => Name_Id'First,
      Table_Initial => 1024,
      Table_Increment => 100);

   -- Initialize this package
   -- This must be called once and only once before any use.
   procedure Initialize is
      Pos: Natural;
      Id: Name_Id;
   begin
      Strings_Table.Init;
      Names_Table.Init;
      -- Reserve entry 0.
      if Names_Table.Allocate /= Null_Identifier then
         raise Program_Error;
      end if;
      Strings_Table.Set_Last (1);
      Names_Table.Table (Null_Identifier) := (Length => 0,
                                              Hash => 0,
                                              Name => 1,
                                              Next => Null_Identifier,
                                              Info => 0);
      -- Store characters.
      for C in Character loop
         Pos := Strings_Table.Allocate;
         Strings_Table.Table (Pos) := C;
         Id := Names_Table.Allocate;
         Names_Table.Table (Id) := (Length => 1,
                                    Hash => 0,
                                    Name => Pos,
                                    Next => Null_Identifier,
                                    Info => 0);
      end loop;
      Hash_Table := (others => Null_Identifier);
   end Initialize;

   -- Compute the hash value of a string.
   function Hash return Hash_Value_Type is
      Res: Hash_Value_Type := 0;
   begin
      for I in 1 .. Name_Length loop
         Res := Res * 7 + Character'Pos(Name_Buffer(I));
         Res := Res + Res / 2**28;
      end loop;
      return Res;
   end Hash;

   -- Get the string associed to an identifier.
   function Image (Id: Name_Id) return String is
      Name_Entry: Identifier renames Names_Table.Table(Id);
      subtype Result_Type is String (1 .. Name_Entry.Length);
   begin
      if Is_Character (Id) then
         return ''' & Strings_Table.Table (Name_Entry.Name) & ''';
      else
         return Result_Type
           (Strings_Table.Table
            (Name_Entry.Name .. Name_Entry.Name + Name_Entry.Length - 1));
      end if;
   end Image;

   procedure Image (Id : Name_Id)
   is
      Name_Entry: Identifier renames Names_Table.Table(Id);
   begin
      if Is_Character (Id) then
         Name_Buffer (1) := Get_Character (Id);
         Name_Length := 1;
      else
         Name_Length := Name_Entry.Length;
         Name_Buffer (1 .. Name_Entry.Length) := String
           (Strings_Table.Table
            (Name_Entry.Name .. Name_Entry.Name + Name_Entry.Length - 1));
      end if;
   end Image;

   -- Get the address of the first character of ID.
   -- The string is NUL-terminated (this is done by get_identifier).
   function Get_Address (Id: Name_Id) return System.Address is
      Name_Entry: Identifier renames Names_Table.Table(Id);
   begin
      return Strings_Table.Table (Name_Entry.Name)'Address;
   end Get_Address;

   function Get_Name_Length (Id: Name_Id) return Natural is
   begin
      return Names_Table.Table(Id).Length;
   end Get_Name_Length;

   function Is_Character (Id: Name_Id) return Boolean is
   begin
      return Id >= First_Character_Name_Id and then
        Id <= First_Character_Name_Id + Character'Pos (Character'Last);
   end Is_Character;

   -- Get the character associed to an identifier.
   function Get_Character (Id: Name_Id) return Character is
   begin
      pragma Assert (Is_Character (Id));
      return Character'Val (Id - First_Character_Name_Id);
   end Get_Character;

   -- Get and set the info field associated with each identifier.
   -- Used to store interpretations of the name.
   function Get_Info (Id: Name_Id) return Int32 is
   begin
      return Names_Table.Table (Id).Info;
   end Get_Info;

   procedure Set_Info (Id: Name_Id; Info: Int32) is
   begin
      Names_Table.Table (Id).Info := Info;
   end Set_Info;

   function Compare_Name_Buffer_With_Name (Id : Name_Id) return Boolean
   is
      Ne: Identifier renames Names_Table.Table(Id);
   begin
      return String (Strings_Table.Table (Ne.Name .. Ne.Name + Ne.Length - 1))
        = Name_Buffer (1 .. Name_Length);
   end Compare_Name_Buffer_With_Name;

   -- Get or create an entry in the name table.
   -- The string is taken from NAME_BUFFER and NAME_LENGTH.
   function Get_Identifier return Name_Id
   is
      Hash_Value, Hash_Index: Hash_Value_Type;
      Res: Name_Id;
   begin
      Hash_Value := Hash;
      Hash_Index := Hash_Value mod Hash_Table_Size;

      if Debug_Name_Table then
         Put_Line ("get_identifier " & Name_Buffer (1 .. Name_Length));
      end if;

      Res := Hash_Table (Hash_Index);
      while Res /= Null_Identifier loop
         --Put_Line ("compare with " & Get_String (Res));
         if Names_Table.Table (Res).Hash = Hash_Value
           and then Names_Table.Table (Res).Length = Name_Length
           and then Compare_Name_Buffer_With_Name (Res)
         then
            --Put_Line ("found");
            return Res;
         end if;
         Res := Names_Table.Table (Res).Next;
      end loop;
      Res := Names_Table.Allocate;
      Names_Table.Table (Res) := (Length => Name_Length,
                                  Hash => Hash_Value,
                                  Name => Store,
                                  Next => Hash_Table (Hash_Index),
                                  Info => 0);
      Hash_Table (Hash_Index) := Res;
      --Put_Line ("created");
      return Res;
   end Get_Identifier;

   function Get_Identifier_No_Create return Name_Id
   is
      Hash_Value, Hash_Index: Hash_Value_Type;
      Res: Name_Id;
   begin
      Hash_Value := Hash;
      Hash_Index := Hash_Value mod Hash_Table_Size;

      Res := Hash_Table (Hash_Index);
      while Res /= Null_Identifier loop
         if Names_Table.Table (Res).Hash = Hash_Value
           and then Names_Table.Table (Res).Length = Name_Length
           and then Compare_Name_Buffer_With_Name (Res)
         then
            return Res;
         end if;
         Res := Names_Table.Table (Res).Next;
      end loop;
      return Null_Identifier;
   end Get_Identifier_No_Create;

   -- Get or create an entry in the name table.
   function Get_Identifier (Str: String) return Name_Id is
   begin
      Name_Length := Str'Length;
      Name_Buffer (1 .. Name_Length) := Str;
      return Get_Identifier;
   end Get_Identifier;

   function Get_Identifier (Char: Character) return Name_Id is
   begin
      return First_Character_Name_Id + Character'Pos (Char);
   end Get_Identifier;

   -- Be sure all info fields have their default value.
   procedure Assert_No_Infos is
      Err: Boolean := False;
   begin
      for I in Names_Table.First .. Names_Table.Last loop
         if Get_Info (I) /= 0 then
            Err := True;
            Put_Line ("still infos in" & Name_Id'Image (I) & ", ie: "
                      & Image (I) & ", info ="
                      & Int32'Image (Names_Table.Table (I).Info));
         end if;
      end loop;
      if Err then
         raise Program_Error;
      end if;
   end Assert_No_Infos;

   -- Return the latest name_id used.
   -- kludge, use only for debugging.
   function Last_Name_Id return Name_Id is
   begin
      return Names_Table.Last;
   end Last_Name_Id;

   -- Used to debug.
   -- Disp the strings table, one word per line.
   procedure Dump;
   pragma Unreferenced (Dump);

   procedure Dump
   is
      First: Natural;
   begin
      Put_Line ("strings_table:");
      First := 0;
      for I in 0 .. Strings_Table.Last loop
         if Strings_Table.Table(I) = NUL then
            Put_Line (Natural'Image (First) & ": "
                      & String (Strings_Table.Table (First .. I - 1)));
            First := I + 1;
         end if;
      end loop;
   end Dump;

   function Get_Hash_Entry_Length (H : Hash_Value_Type) return Natural
   is
      Res : Natural := 0;
      N : Name_Id;
   begin
      N := Hash_Table (H);
      while N /= Null_Identifier loop
         Res := Res + 1;
         N := Names_Table.Table (N).Next;
      end loop;
      return Res;
   end Get_Hash_Entry_Length;

   procedure Disp_Stats
   is
      Min : Natural;
      Max : Natural;
      N : Natural;
   begin
      Put_Line ("Name table statistics:");
      Put_Line (" number of identifiers: " & Name_Id'Image (Last_Name_Id));
      Put_Line (" size of strings: " & Natural'Image (Strings_Table.Last));
      Put_Line (" hash distribution (number of entries per length):");
      Min := Natural'Last;
      Max := Natural'First;
      for I in Hash_Table'Range loop
         N := Get_Hash_Entry_Length (I);
         Min := Natural'Min (Min, N);
         Max := Natural'Max (Max, N);
      end loop;
      declare
         type Nat_Array is array (Min .. Max) of Natural;
         S : Nat_Array := (others => 0);
      begin
         for I in Hash_Table'Range loop
            N := Get_Hash_Entry_Length (I);
            S (N) := S (N) + 1;
         end loop;
         for I in S'Range loop
            if S (I) /= 0 then
               Put_Line ("  " & Natural'Image (I)
                         & ":" & Natural'Image (S (I)));
            end if;
         end loop;
      end;
   end Disp_Stats;
end Name_Table;
