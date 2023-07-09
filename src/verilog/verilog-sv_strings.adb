--  Verilog strings
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

with Ada.Unchecked_Deallocation;
with Name_Table;
with Str_Table;

package body Verilog.Sv_Strings is
   --  Use a ref counter of 1 so that it is always allocated.
   Empty_Sv_String_Obj : aliased Sv_String_Type :=
     (Len => 0, Ref => 1, S => "");

   function Empty_Sv_String return Sv_String is
   begin
      Ref (Empty_Sv_String_Obj'Access);
      return Empty_Sv_String_Obj'Access;
   end Empty_Sv_String;

   function New_Sv_String (Len : Natural) return Sv_String
   is
      Res : Sv_String;
   begin
      Res := new Sv_String_Type (Len);
      Res.Ref := 1;
      return Res;
   end New_Sv_String;

   function New_Sv_String (Id : Name_Id) return Sv_String
   is
      use Name_Table;
      Res : Sv_String;
   begin
      Res := New_Sv_String (Get_Name_Length (Id));
      Res.S := Image (Id);
      return Res;
   end New_Sv_String;

   function New_Sv_String (Id : String8_Id; Len : Natural) return Sv_String
   is
      use Str_Table;
      Res : Sv_String;
   begin
      Res := New_Sv_String (Len);
      for I in 1 .. Len loop
         Res.S (I) := Char_String8 (Id, Pos32 (I));
      end loop;
      return Res;
   end New_Sv_String;

   function New_Sv_String (S : Ghdl_C_String) return Sv_String
   is
      Len : constant Natural := strlen (S);
      Res : Sv_String;
   begin
      Res := New_Sv_String (Len);
      Res.S := S (1 .. Len);
      return Res;
   end New_Sv_String;

   procedure Ref (S : Sv_String) is
   begin
      S.Ref := S.Ref + 1;
   end Ref;

   procedure Free is
      new Ada.Unchecked_Deallocation (Sv_String_Type, Sv_String);

   procedure Unref (S : Sv_String)
   is
      S_Bis : Sv_String;
   begin
      if S.Ref > 1 then
         S.Ref := S.Ref - 1;
      else
         S_Bis := S;
         Free (S_Bis);
      end if;
   end Unref;

   function Get_Length (S : Sv_String) return Natural is
   begin
      return S.Len;
   end Get_Length;

   function Get_String (S : Sv_String) return String is
   begin
      return S.S;
   end Get_String;

   function Is_Eq (L, R : Sv_String) return Boolean is
   begin
      return L.Len = R.Len and then L.S = R.S;
   end Is_Eq;

   procedure Make_Unique (S : in out Sv_String)
   is
      Copy : Sv_String;
   begin
      if S.Ref /= 1 then
         Copy := New_Sv_String (S.Len);
         Copy.S := S.S;
         S := Copy;
      end if;
   end Make_Unique;

   procedure Set_String_El (S : in out Sv_String; I : Positive; C : Character)
   is
      --  As the string is being mutated, there should be only one ref.
      pragma Assert (S.Ref = 1);
   begin
      S.S (I) := C;
   end Set_String_El;

   function Get_String_El (S : Sv_String; I : Positive) return Character is
   begin
      return S.S (I);
   end Get_String_El;

   function Compare (L, R : Sv_String) return Order_Type
   is
      Llen : constant Natural := Get_Length (L);
      Rlen : constant Natural := Get_Length (R);
      Lc, Rc : Character;
   begin
      for I in Positive'Range loop
         if I > Llen and I > Rlen then
            return Equal;
         elsif I > Llen then
            return Greater;
         elsif I > Rlen then
            return Less;
         end if;

         Lc := Get_String_El (L, I);
         Rc := Get_String_El (R, I);
         if Lc /= Rc then
            if Lc > Rc then
               return Greater;
            else
               return Less;
            end if;
         end if;
      end loop;
      raise Internal_Error;
   end Compare;
end Verilog.Sv_Strings;
