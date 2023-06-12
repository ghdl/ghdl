--  Simple name_id-element hash table
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

package body Name_Maps is
   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Array, Element_Arr_Acc);

   procedure Init (Map : out Map_Type) is
   begin
      Map := (Els => new Element_Array'(0 .. 63 => (Name => Null_Identifier,
                                                    User => No_Element)),
              Count => 0);
   end Init;

   function Get_Element (Map : Map_Type; Name : Name_Id) return T
   is
      Mask : constant Uns32 := Map.Els'Last;
      Idx : Uns32;
   begin
      Idx := Uns32 (Name) and Mask;

      for I in 0 .. Map.Count loop
         declare
            El : Element renames Map.Els (Idx);
         begin
            if El.Name = Name then
               return El.User;
            elsif El.Name = Null_Identifier then
               return No_Element;
            end if;
         end;
         Idx := (Idx + 1) and Mask;
      end loop;
      raise Program_Error;
   end Get_Element;

   procedure Set_Element (Map : in out Map_Type; Name : Name_Id; Val : T)
   is
   begin
      declare
         Mask : constant Uns32 := Map.Els'Last;
         Idx : Uns32;
      begin
         Idx := Uns32 (Name) and Mask;

         --  Try to find an existing slot or a free one.
         for I in 1 .. Map.Count + 1 loop
            declare
               El : Element renames Map.Els (Idx);
            begin
               if El.Name = Name then
                  --  Overwrite existing data.
                  El.User := Val;
                  return;
               elsif El.Name = Null_Identifier then
                  --  It is a free slot.
                  if Map.Count * 2 + 1 >= Mask then
                     --  Load is too high.
                     exit;
                  else
                     Map.Count := Map.Count + 1;
                     El := (Name, Val);
                     return;
                  end if;
               end if;
            end;
            Idx := (Idx + 1) and Mask;
         end loop;
      end;

      --  Extend and rehash.
      declare
         Count : constant Uns32 := Map.Count;
         Old_Els : Element_Arr_Acc;
      begin
         Old_Els := Map.Els;
         Map := (Els => new Element_Array'
                   (0 .. 2 * Map.Els'Last + 1 => (Name => Null_Identifier,
                                                  User => No_Element)),
                 Count => 0);
         for I in Old_Els'Range loop
            if Old_Els (I).Name /= Null_Identifier then
               Set_Element (Map, Old_Els (I).Name, Old_Els (I).User);
            end if;
         end loop;

         pragma Assert (Map.Count = Count);

         Free (Old_Els);
      end;

      --  Insert.
      Set_Element (Map, Name, Val);
   end Set_Element;
end Name_Maps;
