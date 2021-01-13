--  GHDL Run Time (GRT) - secondary stack.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Grt.Stdio;
with Grt.Astdio;

package body Grt.Stack2 is
   --  This should be storage_elements.storage_element, but I don't want to
   --  use system.storage_elements package (not pure).  Unfortunatly, this is
   --  currently a failure (storage_elements is automagically used).
   type Memory is array (Mark_Id range <>) of Character;

   --  Minimal chunk size.  Avoid to allocate too many small chunks.
   Min_Chunk_Size : constant Mark_Id := 8 * 1024;

   type Chunk_Type (First, Last : Mark_Id);
   type Chunk_Acc is access all Chunk_Type;
   type Chunk_Type (First, Last : Mark_Id) is record
      Next : Chunk_Acc;
      Mem : Memory (First .. Last);
   end record;

   type Stack2_Type is record
      First_Chunk : Chunk_Acc;
      Last_Chunk : Chunk_Acc;

      --  Index of the first free byte.
      Top : Mark_Id;
   end record;
   type Stack2_Acc is access all Stack2_Type;

   function To_Acc is new Ada.Unchecked_Conversion
     (Source => Stack2_Ptr, Target => Stack2_Acc);
   function To_Addr is new Ada.Unchecked_Conversion
     (Source => Stack2_Acc, Target => Stack2_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Chunk_Type, Name => Chunk_Acc);

   function Mark (S : Stack2_Ptr) return Mark_Id
   is
      S2 : constant Stack2_Acc := To_Acc (S);
   begin
      return S2.Top;
   end Mark;

   procedure Release (S : Stack2_Ptr; Mark : Mark_Id)
   is
      S2 : constant Stack2_Acc := To_Acc (S);
   begin
      S2.Top := Mark;
   end Release;

   function Allocate (S : Stack2_Ptr; Size : Ghdl_Index_Type)
                     return System.Address
   is
      pragma Suppress (All_Checks);

      S2 : constant Stack2_Acc := To_Acc (S);
      Chunk : Chunk_Acc;
      N_Chunk : Chunk_Acc;
      L_Chunk : Chunk_Acc;

      Max_Align : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Aligned_Size  : constant Mark_Id :=
        ((Mark_Id (Size) + Max_Align - 1) / Max_Align) * Max_Align;

      Res : System.Address;
   begin
      --  Find the chunk to which S2.TOP belong.
      --  FIXME: save an hint to that value ?
      Chunk := S2.First_Chunk;
      loop
         exit when S2.Top >= Chunk.First and S2.Top <= Chunk.Last;
         Chunk := Chunk.Next;
         exit when Chunk = null;
      end loop;

      if Chunk /= null then
         --  If there is enough place in it, allocate from the chunk.
         if Aligned_Size <= Chunk.Last - S2.Top + 1 then
            Res := Chunk.Mem (S2.Top)'Address;
            S2.Top := S2.Top + Aligned_Size;
            return Res;
         end if;

         --  If there is not enough place in it: try the next one.  If not
         --   enough room, free it and all the following chunks.
         L_Chunk := Chunk;
         Chunk := Chunk.Next;
         if Chunk /= null then
            if Aligned_Size <= Chunk.Last - Chunk.First + 1 then
               Res := Chunk.Mem (Chunk.First)'Address;
               S2.Top := Chunk.First + Aligned_Size;
               return Res;
            else
               --  Free Chunk and all the following ones.  Do not forget to
               --  update Last_Chunk.
               S2.Last_Chunk := L_Chunk;
               loop
                  N_Chunk := Chunk.Next;
                  Free (Chunk);
                  Chunk := N_Chunk;
                  exit when Chunk = null;
               end loop;
            end if;
         end if;
      end if;

      --    If not such chunk, allocate a chunk
      S2.Top := S2.Last_Chunk.Last + 1;
      Chunk := new Chunk_Type
        (First => S2.Top,
         Last => S2.Top + Mark_Id'Max (Aligned_Size, Min_Chunk_Size) - 1);
      Chunk.Next := null;
      S2.Last_Chunk.Next := Chunk;
      S2.Last_Chunk := Chunk;
      S2.Top := Chunk.Last + 1;
      return Chunk.Mem (Chunk.First)'Address;
   end Allocate;

   function Create return Stack2_Ptr
   is
      Res : Stack2_Acc;
      Chunk : Chunk_Acc;
   begin
      Chunk := new Chunk_Type (First => 1, Last => Min_Chunk_Size);
      Chunk.Next := null;
      Res := new Stack2_Type'(First_Chunk => Chunk,
                              Last_Chunk => Chunk,
                              Top => 1);
      return To_Addr (Res);
   end Create;

   function Is_Empty (S : Stack2_Ptr) return Boolean
   is
      S2 : constant Stack2_Acc := To_Acc (S);
   begin
      if S2 = null then
         return True;
      end if;
      return S2.Top = 1;
   end Is_Empty;

   procedure Dump_Stack2 (S : Stack2_Ptr)
   is
      use Grt.Astdio;
      use Grt.Stdio;
      use System;
      function To_Address is new Ada.Unchecked_Conversion
        (Source => Chunk_Acc, Target => Address);
      function To_Address is new Ada.Unchecked_Conversion
        (Source => Mark_Id, Target => Address);
      S2 : Stack2_Acc;
      Chunk : Chunk_Acc;
   begin
      S2 := To_Acc (S);
      Put ("Stack 2 at ");
      Put (stdout, Address (S));
      New_Line;
      if S2 = null then
         return;
      end if;
      Put ("First Chunk at ");
      Put (stdout, To_Address (S2.First_Chunk));
      Put (", last chunk at ");
      Put (stdout, To_Address (S2.Last_Chunk));
      Put (", top at ");
      Put (stdout, To_Address (S2.Top));
      New_Line;
      Chunk := S2.First_Chunk;
      while Chunk /= null loop
         Put ("Chunk ");
         Put (stdout, To_Address (Chunk));
         Put (": first: ");
         Put (stdout, To_Address (Chunk.First));
         Put (", last: ");
         Put (stdout, To_Address (Chunk.Last));
         Put (", len: ");
         Put (stdout, To_Address (Chunk.Last - Chunk.First + 1));
         Put (", next = ");
         Put (stdout, To_Address (Chunk.Next));
         New_Line;
         Chunk := Chunk.Next;
      end loop;
   end Dump_Stack2;
end Grt.Stack2;
