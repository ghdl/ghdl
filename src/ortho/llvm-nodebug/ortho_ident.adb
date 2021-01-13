--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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

package body Ortho_Ident is
   type Chunk (Max : Positive);
   type Chunk_Acc is access Chunk;

   type Chunk (Max : Positive) is record
      Prev : Chunk_Acc;
      Len : Natural := 0;
      S : String (1 .. Max);
   end record;

   Cur_Chunk : Chunk_Acc := null;

   subtype Fat_String is String (Positive);

   function Get_Identifier (Str : String) return O_Ident
   is
      Len : constant Natural := Str'Length;
      Max : Positive;
      Org : Positive;
   begin
      if Cur_Chunk = null or else Cur_Chunk.Len + Len >= Cur_Chunk.Max then
         if Cur_Chunk = null then
            Max := 32 * 1024;
         else
            Max := 2 * Cur_Chunk.Max;
         end if;
         if Len + 2 > Max then
            Max := 2 * (Len + 2);
         end if;
         declare
            New_Chunk : Chunk_Acc;
         begin
            --  Do not use allocator by expression, as we don't want to
            --  initialize S.
            New_Chunk := new Chunk (Max);
            New_Chunk.Len := 0;
            New_Chunk.Prev := Cur_Chunk;
            Cur_Chunk := New_Chunk;
         end;
      end if;

      Org := Cur_Chunk.Len + 1;
      Cur_Chunk.S (Org .. Org + Len - 1) := Str;
      Cur_Chunk.S (Org + Len) := ASCII.NUL;
      Cur_Chunk.Len := Org + Len;

      return (Addr => Cur_Chunk.S (Org)'Address);
   end Get_Identifier;

   function Is_Equal (L, R : O_Ident) return Boolean
   is
   begin
      return L = R;
   end Is_Equal;

   function Get_String_Length (Id : O_Ident) return Natural
   is
      Str : Fat_String;
      pragma Import (Ada, Str);
      for Str'Address use Id.Addr;
   begin
      for I in Str'Range loop
         if Str (I) = ASCII.NUL then
            return I - 1;
         end if;
      end loop;
      raise Program_Error;
   end Get_String_Length;

   function Get_String (Id : O_Ident) return String
   is
      Str : Fat_String;
      pragma Import (Ada, Str);
      for Str'Address use Id.Addr;
   begin
      for I in Str'Range loop
         if Str (I) = ASCII.NUL then
            return Str (1 .. I - 1);
         end if;
      end loop;
      raise Program_Error;
   end Get_String;

   function Get_Cstring (Id : O_Ident) return System.Address is
   begin
      return Id.Addr;
   end Get_Cstring;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean
   is
      Istr : Fat_String;
      pragma Import (Ada, Istr);
      for Istr'Address use Id.Addr;

      Str_Len : constant Natural := Str'Length;
   begin
      for I in Istr'Range loop
         if Istr (I) = ASCII.NUL then
            return I - 1 = Str_Len;
         end if;
         if I > Str_Len then
            return False;
         end if;
         if Istr (I) /= Str (Str'First + I - 1) then
            return False;
         end if;
      end loop;
      raise Program_Error;
   end Is_Equal;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = O_Ident_Nul;
   end Is_Nul;

end Ortho_Ident;
