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
with Ada.Unchecked_Deallocation;
with Interfaces;
with Logging; use Logging;
with Tables;

package body Name_Table is
   --  Id of the first character (NUL).
   First_Character_Name_Id : constant Name_Id := 1;

   --  Type for the hash value.
   type Hash_Value_Type is mod 2**32;

   type Str_Idx is new Nat32;

   --  An entry in the name table.
   type Identifier is record
      --  Hash value of the identifier.
      Hash : Hash_Value_Type;

      --  Simply linked collision chain.
      Next : Name_Id;

      --  Index in Strings_Table of the first character of the identifier.
      --  The name is always NUL terminated, but the length can be computed
      --  from the name of the next identifier.  Indeed, names are put in
      --  Strings_Table in the same order as identifiers.
      Name : Str_Idx;

      --  User infos.
      Info : Int32;
   end record;

   --  Size of the hash table.  Must be a power of 2, so that bit masked can be
   --  used to get the entry number from the hash value.
   Hash_Table_Size : Hash_Value_Type := 1024;

   type Hash_Array is array (Hash_Value_Type range <>) of Name_Id;
   type Hash_Array_Acc is access Hash_Array;

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Hash_Array, Hash_Array_Acc);

   --  Hash table.  Lower bound is always 0, upper bound is always
   --  Hash_Table_Size - 1.
   Hash_Table: Hash_Array_Acc;

   --  Table of identifiers.
   package Names_Table is new Tables
     (Table_Index_Type => Name_Id,
      Table_Component_Type => Identifier,
      Table_Low_Bound => Name_Id'First,
      Table_Initial => 1024);

   --  A NUL character is stored after each word in the strings_table.
   --  This is used for compatibility with C.
   NUL : constant Character := Character'Val (0);

   --  The table to store all the strings.  Strings are always NUL terminated.
   package Strings_Table is new Tables
     (Table_Index_Type => Str_Idx,
      Table_Component_Type => Character,
      Table_Low_Bound => Str_Idx'First,
      Table_Initial => 4096);

   --  Allocate place in the strings_table, and store the name_buffer into it.
   --  Also append a NUL.
   function Store (Str : Thin_String_Ptr; Len : Natural) return Str_Idx
   is
      Res : Str_Idx;
   begin
      Res := Strings_Table.Allocate (Len + 1);
      Strings_Table.Table (Res .. Res + Str_Idx (Len) - 1) :=
        Strings_Table.Table_Type (Str (1 .. Len));
      Strings_Table.Table (Res + Str_Idx (Len)) := NUL;
      return Res;
   end Store;

   --  Append the terminator in Names_Table.  This is required so that the
   --  length of the last identifier can be computed (like any other
   --  identifiers).
   procedure Append_Terminator is
   begin
      Names_Table.Append ((Hash => 0,
                           Name => Strings_Table.Last + 1,
                           Next => Null_Identifier,
                           Info => 0));
   end Append_Terminator;

   procedure Initialize is
   begin
      Strings_Table.Init;
      Names_Table.Init;

      Strings_Table.Append (NUL);

      --  Reserve entry 0 for Null_Identifier.
      Strings_Table.Append (NUL);
      Names_Table.Append ((Hash => 0,
                           Name => Strings_Table.Last,
                           Next => Null_Identifier,
                           Info => 0));
      pragma Assert (Names_Table.Last = Null_Identifier);

      --  Store characters.  They aren't put in the hash table.
      for C in Character loop
         Strings_Table.Append (C);
         Names_Table.Append ((Hash => 0,
                              Name => Strings_Table.Last,
                              Next => Null_Identifier,
                              Info => 0));

         Strings_Table.Append (NUL);
      end loop;

      Append_Terminator;

      --  Allocate the Hash_Table.
      Hash_Table_Size := 1024;
      Hash_Table :=
        new Hash_Array'(0 .. Hash_Table_Size - 1 => Null_Identifier);
   end Initialize;

   procedure Finalize is
   begin
      Strings_Table.Free;
      Names_Table.Free;
      Deallocate (Hash_Table);
   end Finalize;

   --  Compute the hash value of a string.  In case of algorithm change, check
   --  the performance using Disp_Stats.
   function Compute_Hash (Str : Thin_String_Ptr; Len : Natural)
                         return Hash_Value_Type
   is
      use Interfaces;
      Res : Unsigned_32;
   begin
      Res := Unsigned_32 (Len);
      for I in 1 .. Len loop
         Res := Rotate_Left (Res, 4) + Res + Character'Pos (Str (I));
      end loop;
      return Hash_Value_Type (Res);
   end Compute_Hash;

   --  Get the string associed to an identifier.
   function Image (Id : Name_Id) return String
   is
      Ent : Identifier renames Names_Table.Table (Id);
   begin
      if Is_Character (Id) then
         return ''' & Strings_Table.Table (Ent.Name) & ''';
      else
         declare
            Len : constant Natural := Get_Name_Length (Id);
            subtype Result_Type is String (1 .. Len);
         begin
            return Result_Type
              (Strings_Table.Table (Ent.Name .. Ent.Name + Str_Idx (Len) - 1));
         end;
      end if;
   end Image;

   --  Get the address of the first character of ID.
   --  The string is NUL-terminated (this is done by get_identifier).
   function Get_Address (Id : Name_Id) return System.Address
   is
      Name_Entry: Identifier renames Names_Table.Table(Id);
   begin
      return Strings_Table.Table (Name_Entry.Name)'Address;
   end Get_Address;

   function Get_Name_Ptr (Id : Name_Id) return Thin_String_Ptr
   is
      Name_Entry: Identifier renames Names_Table.Table(Id);
   begin
      return To_Thin_String_Ptr
        (Strings_Table.Table (Name_Entry.Name)'Address);
   end Get_Name_Ptr;

   function Get_Name_Length (Id : Name_Id) return Natural
   is
      pragma Assert (Id < Names_Table.Last);
      Id_Name : constant Str_Idx := Names_Table.Table (Id).Name;
      Id1_Name : constant Str_Idx := Names_Table.Table (Id + 1).Name;
   begin
      --  Do not count NUL terminator.
      return Natural (Id1_Name - Id_Name - 1);
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
      return Names_Table.Table (Id).Info;
   end Get_Name_Info;

   procedure Set_Name_Info (Id : Name_Id; Info : Int32) is
   begin
      Names_Table.Table (Id).Info := Info;
   end Set_Name_Info;

   --  Compare ID with Str / Len.  Length of ID must be equal to Len.
   function Compare_Name_Buffer_With_Name
     (Id : Name_Id; Str : Thin_String_Ptr; Len : Natural) return Boolean
   is
      Ne: Identifier renames Names_Table.Table (Id);
   begin
      return String
        (Strings_Table.Table (Ne.Name .. Ne.Name + Str_Idx (Len) - 1))
        = Str (1 .. Len);
   end Compare_Name_Buffer_With_Name;

   --  Expand the hash table (double the size).
   procedure Expand
   is
      Old_Hash_Table : Hash_Array_Acc;
      Id : Name_Id;
   begin
      Old_Hash_Table := Hash_Table;
      Hash_Table_Size := Hash_Table_Size * 2;
      Hash_Table :=
        new Hash_Array'(0 .. Hash_Table_Size - 1 => Null_Identifier);

      --  Rehash.
      for I in Old_Hash_Table'Range loop
         Id := Old_Hash_Table (I);
         while Id /= Null_Identifier loop
            --  Note: collisions are put in reverse order.
            declare
               Ent : Identifier renames Names_Table.Table (Id);
               Hash_Index : constant Hash_Value_Type :=
                 Ent.Hash and (Hash_Table_Size - 1);
               Next_Id : constant Name_Id := Ent.Next;
            begin
               Ent.Next := Hash_Table (Hash_Index);
               Hash_Table (Hash_Index) := Id;
               Id := Next_Id;
            end;
         end loop;
      end loop;

      Deallocate (Old_Hash_Table);
   end Expand;

   --  Get or create an entry in the name table.
   function Get_Identifier_With_Len (Str : Thin_String_Ptr; Len : Natural)
                                    return Name_Id
   is
      Hash_Value : Hash_Value_Type;
      Hash_Index : Hash_Value_Type;
      Res : Name_Id;
   begin
      Hash_Value := Compute_Hash (Str, Len);
      Hash_Index := Hash_Value and (Hash_Table_Size - 1);

      --  Find the name.
      Res := Hash_Table (Hash_Index);
      while Res /= Null_Identifier loop
         if Names_Table.Table (Res).Hash = Hash_Value
           and then Get_Name_Length (Res) = Len
           and then Compare_Name_Buffer_With_Name (Res, Str, Len)
         then
            return Res;
         end if;
         Res := Names_Table.Table (Res).Next;
      end loop;

      --  Maybe expand Hash_Table.
      if Hash_Value_Type (Names_Table.Last) > 2 * Hash_Table_Size then
         Expand;
         --  The Hash_Index has certainly changed.
         Hash_Index := Hash_Value and (Hash_Table_Size - 1);
      end if;

      --  Insert new entry.
      Res := Names_Table.Last;
      Names_Table.Table (Res) := (Hash => Hash_Value,
                                  Name => Store (Str, Len),
                                  Next => Hash_Table (Hash_Index),
                                  Info => 0);
      Hash_Table (Hash_Index) := Res;
      Append_Terminator;

      return Res;
   end Get_Identifier_With_Len;

   function Get_Identifier_No_Create_With_Len
     (Str : Thin_String_Ptr; Len : Natural) return Name_Id
   is
      Hash_Value : Hash_Value_Type;
      Hash_Index : Hash_Value_Type;
      Res: Name_Id;
   begin
      Hash_Value := Compute_Hash (Str, Len);
      Hash_Index := Hash_Value and (Hash_Table_Size - 1);

      Res := Hash_Table (Hash_Index);
      while Res /= Null_Identifier loop
         if Names_Table.Table (Res).Hash = Hash_Value
           and then Get_Name_Length (Res) = Len
           and then Compare_Name_Buffer_With_Name (Res, Str, Len)
         then
            return Res;
         end if;
         Res := Names_Table.Table (Res).Next;
      end loop;
      return Null_Identifier;
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

   --  Be sure all info fields have their default value.
   procedure Assert_No_Infos
   is
      Err : Boolean := False;
   begin
      for I in Names_Table.First .. Names_Table.Last loop
         if Get_Name_Info (I) /= 0 then
            Err := True;
            Log_Line ("still infos in" & Name_Id'Image (I) & ", ie: "
                      & Image (I) & ", info ="
                      & Int32'Image (Names_Table.Table (I).Info));
         end if;
      end loop;
      if Err then
         raise Program_Error;
      end if;
   end Assert_No_Infos;

   --  Return the latest name_id used.
   --  kludge, use only for debugging.
   function Last_Name_Id return Name_Id is
   begin
      return Names_Table.Last;
   end Last_Name_Id;

   --  Used to debug.
   --  Disp the strings table, one word per line.
   procedure Dump;
   pragma Unreferenced (Dump);

   procedure Dump
   is
      First : Str_Idx;
   begin
      Log_Line ("strings_table:");
      First := Strings_Table.First;
      for I in Strings_Table.First .. Strings_Table.Last loop
         if Strings_Table.Table(I) = NUL then
            if I > Strings_Table.First then
               Log (Str_Idx'Image (First) & ": ");
               Log (String (Strings_Table.Table (First .. I - 1)));
               Log_Line;
            end if;
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
      Log_Line ("Name table statistics:");
      Log_Line (" number of identifiers: " & Name_Id'Image (Last_Name_Id));
      Log_Line (" size of strings: " & Str_Idx'Image (Strings_Table.Last));
      Log_Line (" hash array length: "
                  & Hash_Value_Type'Image (Hash_Table_Size));
      Log_Line (" hash distribution (number of entries per length):");
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
               Log_Line ("  " & Natural'Image (I)
                         & ":" & Natural'Image (S (I)));
            end if;
         end loop;
      end;
   end Disp_Stats;
end Name_Table;
