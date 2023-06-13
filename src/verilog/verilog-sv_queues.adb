--  Verilog queues
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

package body Verilog.Sv_Queues is
   Initial_Capacity : constant Uns32 := 16;

   --  Create a new queue.
   function Queue_New (El_Size : Storage_Index;
                       Limit : Uns32;
                       Capacity : Uns32) return Sv_Queue
   is
      Res : Sv_Queue;
      Base : Data_Ptr;
   begin
      if Capacity = 0 then
         Base := No_Data_Ptr;
      else
         Base := Malloc (El_Size * Storage_Index (Capacity));
      end if;

      Res := new Sv_Queue_Type'(El_Size => El_Size,
                                Limit => Limit,
                                Capacity => Capacity,
                                Size => 0,
                                First => 0,
                                Base => Base);
      return Res;
   end Queue_New;

   function Queue_Size (Q : Sv_Queue) return Uns32 is
   begin
      return Q.Size;
   end Queue_Size;

   procedure Ensure_Capacity (Q : Sv_Queue; Size : Uns32)
   is
      Ncap : Uns32;
      Elsz : Storage_Index;
   begin
      if Q.Capacity >= Size then
         return;
      end if;
      --  Cannot go beyond capacity.
      pragma Assert (Q.Limit = Unlimited or else Size <= Q.Capacity + 1);

      if Q.Capacity = 0 then
         Ncap := Initial_Capacity;
      elsif Q.Capacity < Uns32'Last / 2 then
         Ncap := Q.Capacity * 2;
      else
         --  Too large
         raise Internal_Error;
      end if;
      if Ncap > Q.Limit then
         Ncap := Q.Limit + 1;
      end if;

      Elsz := Q.El_Size;

      if Q.First + Q.Size > Q.Capacity then
         --  Cannot just grow, need to move elements.
         declare
            Nhead : constant Uns32 := Q.First + Q.Size - Q.Capacity;
            Ntail : constant Uns32 := Q.Capacity - Q.First;
            Nbase : Data_Ptr;
         begin
            Nbase := Malloc (Storage_Index (Ncap) * Elsz);
            Memcpy (Nbase,
                    Q.Base + Storage_Index (Q.First) * Elsz,
                    Storage_Index (Ntail) * Elsz);
            Memcpy (Nbase + Storage_Index (Ntail) * Elsz,
                    Q.Base,
                    Storage_Index (Nhead) * Elsz);
            Free (Q.Base);
            Q.Base := Nbase;
            Q.First := 0;
         end;
      else
         Q.Base := Realloc (Q.Base, Storage_Index (Ncap) * Elsz);
      end if;
      Q.Capacity := Ncap;
   end Ensure_Capacity;

   function Get_Address (Q : Sv_Queue; Idx : Uns32) return Data_Ptr
   is
      Tail : constant Uns32 := Q.Capacity - Q.First;
      Off : Uns32;
   begin
      pragma Assert (Idx <= Q.Size + 1);
      if Idx > Tail then
         Off := Idx - Tail;
      else
         Off := Q.First + Idx;
      end if;
      return Q.Base + Storage_Index (Off) * Q.El_Size;
   end Get_Address;

   function Queue_Index (Q : Sv_Queue; Idx : Int32) return Data_Ptr is
   begin
      if Idx < 0 or else Uns32 (Idx) >= Q.Size then
         return No_Data_Ptr;
      end if;
      return Get_Address (Q, Uns32 (Idx));
   end Queue_Index;

   function Queue_Push_Back (Q : Sv_Queue) return Data_Ptr
   is
      Res : Data_Ptr;
   begin
      Ensure_Capacity (Q, Q.Size + 1);
      Res := Get_Address (Q, Q.Size);
      if Q.Size < Q.Limit then
         Q.Size := Q.Size + 1;
      end if;
      return Res;
   end Queue_Push_Back;

   --  Make the queue empty.
   procedure Queue_Empty (Dest : Sv_Queue) is
   begin
      Dest.Size := 0;
      Dest.First := 0;
   end Queue_Empty;

   procedure Queue_Assign (Dest : Sv_Queue; Src : Sv_Queue)
   is
      Elsz : constant Storage_Index := Src.El_Size;
      pragma Assert (Elsz = Dest.El_Size);
   begin
      Queue_Empty (Dest);
      Ensure_Capacity (Dest, Src.Size);

      if Src.Size > Dest.Limit then
         --  TODO: limit size.
         raise Internal_Error;
      end if;

      --  Copy
      if Src.First + Src.Size > Src.Capacity then
         --  Disjoint
         declare
            Nhead : constant Uns32 := Src.First + Src.Size - Src.Capacity;
            Ntail : constant Uns32 := Src.Capacity - Src.First;
         begin
            Memcpy (Dest.Base,
                    Src.Base + Storage_Index (Src.First) * Elsz,
                    Storage_Index (Ntail) * Elsz);
            Memcpy (Dest.Base + Storage_Index (Ntail) * Elsz,
                    Src.Base,
                    Storage_Index (Nhead) * Elsz);
         end;
      else
         Memcpy (Dest.Base, Src.Base, Storage_Index (Src.Size) * Elsz);
      end if;

      Dest.Size := Src.Size;
   end Queue_Assign;


   procedure Delete (Arr : in out Sv_Queue) is
   begin
      raise Internal_Error;
   end Delete;
end Verilog.Sv_Queues;
