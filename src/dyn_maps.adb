--  Type interning - set of unique objects.
--  Copyright (C) 2019 Tristan Gingold
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

package body Dyn_Maps is
   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Hash_Array, Hash_Array_Acc);

   procedure Init (Inst : out Instance) is
   begin
      Inst.Size := Initial_Size;
      Inst.Hash_Table := new Hash_Array'(0 .. Initial_Size - 1 => No_Index);
      Wrapper_Tables.Init (Inst.Els, 128);
      pragma Assert (Wrapper_Tables.Last (Inst.Els) = No_Index);
   end Init;

   procedure Free (Inst : in out Instance) is
   begin
      Deallocate (Inst.Hash_Table);
      Inst.Size := 0;
      Wrapper_Tables.Free (Inst.Els);
   end Free;

   --  Expand the hash table (double the size).
   procedure Expand (Inst : in out Instance)
   is
      Old_Hash_Table : Hash_Array_Acc;
      Idx : Index_Type;
   begin
      Old_Hash_Table := Inst.Hash_Table;
      Inst.Size := Inst.Size * 2;
      Inst.Hash_Table := new Hash_Array'(0 .. Inst.Size - 1 => No_Index);

      --  Rehash.
      for I in Old_Hash_Table'Range loop
         Idx := Old_Hash_Table (I);
         while Idx /= No_Index loop
            --  Note: collisions are put in reverse order.
            declare
               Ent : Element_Wrapper renames Inst.Els.Table (Idx);
               Hash_Index : constant Hash_Value_Type :=
                 Ent.Hash and (Inst.Size - 1);
               Next_Idx : constant Index_Type := Ent.Next;
            begin
               Ent.Next := Inst.Hash_Table (Hash_Index);
               Inst.Hash_Table (Hash_Index) := Idx;
               Idx := Next_Idx;
            end;
         end loop;
      end loop;

      Deallocate (Old_Hash_Table);
   end Expand;

   function Get_Index_With_Hash
     (Inst : Instance; Params : Params_Type; Hash_Value : Hash_Value_Type)
     return Index_Type
   is
      Hash_Index : Hash_Value_Type;
      Idx : Index_Type;
   begin
      Hash_Index := Hash_Value and (Inst.Size - 1);

      Idx := Inst.Hash_Table (Hash_Index);
      while Idx /= No_Index loop
         declare
            E : Element_Wrapper renames Inst.Els.Table (Idx);
         begin
            if E.Hash = Hash_Value and then Equal (E.Obj, Params) then
               return Idx;
            end if;
            Idx := E.Next;
         end;
      end loop;

      return No_Index;
   end Get_Index_With_Hash;

   function Get_Index_Soft (Inst : Instance; Params : Params_Type)
                           return Index_Type is
   begin
      --  Check if the package was initialized.
      pragma Assert (Inst.Hash_Table /= null);

      return Get_Index_With_Hash (Inst, Params, Hash (Params));
   end Get_Index_Soft;

   procedure Get_Index
     (Inst : in out Instance; Params : Params_Type; Idx : out Index_Type)
   is
      Hash_Value : constant Hash_Value_Type := Hash (Params);
      Hash_Index : Hash_Value_Type;
   begin
      --  Check if the package was initialized.
      pragma Assert (Inst.Hash_Table /= null);

      Idx := Get_Index_With_Hash (Inst, Params, Hash_Value);
      if Idx /= No_Index then
         return;
      end if;

      --  Insert.

      --  Maybe expand the table.
      if Hash_Value_Type (Wrapper_Tables.Last (Inst.Els)) > 2 * Inst.Size then
         Expand (Inst);
      end if;

      --  Compute hash index.
      Hash_Index := Hash_Value and (Inst.Size - 1);

      declare
         Res : Object_Type;
         Val : Value_Type;
      begin
         Res := Build (Params);
         Val := Build_Value (Res);

         --  Insert.
         Wrapper_Tables.Append (Inst.Els,
                                (Hash => Hash_Value,
                                 Next => Inst.Hash_Table (Hash_Index),
                                 Obj => Res,
                                 Val => Val));
         Inst.Hash_Table (Hash_Index) := Wrapper_Tables.Last (Inst.Els);
      end;

      Idx := Wrapper_Tables.Last (Inst.Els);
   end Get_Index;

   function Last_Index (Inst : Instance) return Index_Type is
   begin
      return Wrapper_Tables.Last (Inst.Els);
   end Last_Index;

   function Get_By_Index (Inst : Instance; Index : Index_Type)
                         return Object_Type is
   begin
      pragma Assert (Index <= Wrapper_Tables.Last (Inst.Els));
      return Inst.Els.Table (Index).Obj;
   end Get_By_Index;

   function Get_Value (Inst : Instance; Index : Index_Type)
                       return Value_Type is
   begin
      pragma Assert (Index <= Wrapper_Tables.Last (Inst.Els));
      return Inst.Els.Table (Index).Val;
   end Get_Value;

   procedure Set_Value
     (Inst : in out Instance; Index : Index_Type; Val : Value_Type) is
   begin
      pragma Assert (Index <= Wrapper_Tables.Last (Inst.Els));
      Inst.Els.Table (Index).Val := Val;
   end Set_Value;
end Dyn_Maps;
