--  Simple key-element hash table
--  Copyright (C) 2023 Tristan Gingold
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

package body Dyn_Htables is
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Hash_Array, Hash_Array_Acc);

   procedure Init (HT : out Instance) is
   begin
      HT.Size := Initial_Size;
      HT.Hash_Table := new Hash_Array'(0 .. Initial_Size - 1 => No_Index);
      Wrapper_Tables.Init (HT.Els);
      pragma Assert (Wrapper_Tables.Last (HT.Els) = No_Index);
   end Init;

   --  Find K in HT, return its index or No_Index if not found.
   function Find (HT : Instance; K : Key_Type) return Index_Type
   is
      Hash_Value : Hash_Value_Type;
      Hash_Index : Hash_Value_Type;
      Res: Index_Type;
   begin
      Hash_Value := Hash (K);
      Hash_Index := Hash_Value and (HT.Size - 1);

      Res := HT.Hash_Table (Hash_Index);
      while Res /= No_Index loop
         declare
            E : Element_Wrapper renames HT.Els.Table (Res);
         begin
            if E.Hash = Hash_Value and then Equal (K, E.Key) then
               return Res;
            end if;
            Res := E.Next;
         end;
      end loop;
      return No_Index;
   end Find;

   --  Expand the hash table (double the size).
   procedure Expand (HT : in out Instance)
   is
      Old_Hash_Table : Hash_Array_Acc;
      Idx : Index_Type;
   begin
      Old_Hash_Table := HT.Hash_Table;
      HT.Size := HT.Size * 2;
      HT.Hash_Table := new Hash_Array'(0 .. HT.Size - 1 => No_Index);

      --  Rehash.
      for I in Old_Hash_Table'Range loop
         Idx := Old_Hash_Table (I);
         while Idx /= No_Index loop
            --  Note: collisions are put in reverse order.
            declare
               Ent : Element_Wrapper renames HT.Els.Table (Idx);
               Hash_Index : constant Hash_Value_Type :=
                 Ent.Hash and (HT.Size - 1);
               Next_Idx : constant Index_Type := Ent.Next;
            begin
               Ent.Next := HT.Hash_Table (Hash_Index);
               HT.Hash_Table (Hash_Index) := Idx;
               Idx := Next_Idx;
            end;
         end loop;
      end loop;

      Deallocate (Old_Hash_Table);
   end Expand;

   --  Find Key in HT, return its index in RES if the key exists.
   --  Otherwise, create it (and set Created to True).
   procedure Find_Or_Create
     (HT : in out  Instance;
      Key : Key_Type;
      Created : out Boolean;
      Res : out Index_Type)
   is
      Hash_Value : Hash_Value_Type;
      Hash_Index : Hash_Value_Type;
   begin
      Hash_Value := Hash (Key);
      Hash_Index := Hash_Value and (HT.Size - 1);

      Res := HT.Hash_Table (Hash_Index);
      while Res /= No_Index loop
         declare
            E : Element_Wrapper renames HT.Els.Table (Res);
         begin
            if E.Hash = Hash_Value and then Equal (Key, E.Key) then
               Created := False;
               return;
            end if;
            Res := E.Next;
         end;
      end loop;

      --  Maybe expand the table.
      if Hash_Value_Type (Wrapper_Tables.Last (HT.Els)) > 2 * HT.Size then
         Expand (HT);

         --  Recompute hash index.
         Hash_Index := Hash_Value and (HT.Size - 1);
      end if;

      --  Insert.
      Wrapper_Tables.Append (HT.Els,
                             (Hash => Hash_Value,
                              Next => HT.Hash_Table (Hash_Index),
                              Key => Key,
                              El  => No_Element));
      Created := True;
      Res := Wrapper_Tables.Last (HT.Els);
      HT.Hash_Table (Hash_Index) := Res;
   end Find_Or_Create;

   procedure Set (HT : in out Instance; K : Key_Type; El : Element_Type)
   is
      Idx : Index_Type;
      Created : Boolean;
   begin
      Find_Or_Create (HT, K, Created, Idx);
      HT.Els.Table (Idx).El := El;
   end Set;

   function Get (HT : Instance; K : Key_Type) return Element_Type
   is
      Idx : Index_Type;
   begin
      Idx := Find (HT, K);
      if Idx = No_Index then
         return No_Element;
      else
         return HT.Els.Table (Idx).El;
      end if;
   end Get;

   procedure Unify (HT : in out Instance;
                    K : Key_Type;
                    El : Element_Type;
                    Res : out Element_Type)
   is
      Idx : Index_Type;
      Created : Boolean;
   begin
      Find_Or_Create (HT, K, Created, Idx);
      if Created then
         HT.Els.Table (Idx).El := El;
         Res := El;
      else
         Res := HT.Els.Table (Idx).El;
      end if;
   end Unify;

   function Exists (HT : Instance; K : Key_Type) return Boolean is
   begin
      return Find (HT, K) /= No_Index;
   end Exists;
end Dyn_Htables;
