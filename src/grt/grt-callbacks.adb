--  GHDL Run Time (GRT) -  Callbacks.
--  Copyright (C) 2015 Tristan Gingold
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

package body Grt.Callbacks is
   Recycled_Handles : Callback_Handle := null;

   procedure Free_Handle (Hand : Callback_Handle) is
   begin
      Hand.Next := Recycled_Handles;
      Recycled_Handles := Hand;
   end Free_Handle;

   function Allocate_Handle return Callback_Handle
   is
      Res : Callback_Handle;
   begin
      Res := Recycled_Handles;
      if Res = null then
         return new Cb_Cell;
      else
         Recycled_Handles := Res.Next;
         return Res;
      end if;
   end Allocate_Handle;

   procedure Register_Callback_At
     (List : in out Callback_Time_List;
      Handle : out Callback_Handle;
      T : Std_Time;
      Proc : Callback_Acc;
      Arg : System.Address := System.Null_Address)
   is
      Last, Cur : Callback_Handle;
   begin
      Handle := Allocate_Handle;
      Handle.all := (T => T, Mode => Timed,
                     Proc => Proc, Arg => Arg, Next => null);

      Last := null;
      Cur := List.First;

      --  Insert after timeouts before (<=) T.
      while Cur /= null loop
         exit when Cur.T > T;
         Last := Cur;
         Cur := Cur.Next;
      end loop;

      if Last = null then
         --  At head.
         Handle.Next := List.First;
         List.First := Handle;
         List.First_Timeout := T;
      else
         pragma Assert (Cur = Last.Next);
         Handle.Next := Cur;
         Last.Next := Handle;
      end if;
   end Register_Callback_At;

   procedure Call_Time_Callbacks (List : in out Callback_Time_List)
   is
      C : Callback_Handle;
   begin
      pragma Assert (List.First_Timeout = Current_Time);

      loop
         C := List.First;
         if C = null then
            --  No more callback.
            List.First_Timeout := Std_Time'Last;
            exit;
         elsif C.T > Current_Time then
            --  No more callbacks for current time.
            List.First_Timeout := C.T;
            exit;
         end if;

         List.First := C.Next;

         --  Calling the callback may have side effects, like adding a new
         --  callback.  They should be in the future.
         declare
            Proc : constant Callback_Acc := C.Proc;
            Arg : constant System.Address := C.Arg;
         begin
            Free_Handle (C);
            Proc.all (Arg);
         end;
      end loop;
   end Call_Time_Callbacks;

   procedure Register_Callback
     (List : in out Callback_List;
      Handle : out Callback_Handle;
      Mode : Callback_Mode;
      Proc : Callback_Acc;
      Arg : System.Address := System.Null_Address) is
   begin
      Handle := Allocate_Handle;
      Handle.all := (T => 0, Mode => Mode,
                     Proc => Proc, Arg => Arg, Next => null);

      --  Append.
      if List.First = null then
         pragma Assert (List.Last = null);
         List.First := Handle;
      else
         pragma Assert (List.Last /= null);
         List.Last.Next := Handle;
      end if;
      List.Last := Handle;
   end Register_Callback;

   procedure Call_Callbacks (List : in out Callback_List)
   is
      --  Last cell to call.  Newly appended cells are not executed.
      Last : constant Callback_Handle := List.Last;

      Cell, Next_Cell, Prev_Cell : Callback_Handle;
   begin
      Cell := List.First;

      if Cell = null then
         return;
      end if;

      Prev_Cell := null;
      loop
         --  First, call the callback.  This may change the queue (for example
         --  append a new callback and therefore change the next link of that
         --  cell).
         declare
            Proc : constant Callback_Acc := Cell.Proc;
            Arg : constant System.Address := Cell.Arg;
         begin
            Proc.all (Arg);
         end;

         Next_Cell := Cell.Next;
         if Cell.Mode = Oneshot then
            if Prev_Cell = null then
               --  First cell of the list, update head.
               List.First := Next_Cell;
            else
               Prev_Cell.Next := Next_Cell;
            end if;
            if Next_Cell = null then
               List.Last := Prev_Cell;
            end if;
            Free_Handle (Cell);
         else
            Prev_Cell := Cell;
         end if;
         exit when Cell = Last;
         Cell := Next_Cell;
      end loop;
   end Call_Callbacks;

   procedure Nop_Callback (Arg : System.Address) is
   begin
      null;
   end Nop_Callback;

   procedure Delete_Callback (Handle : Callback_Handle) is
   begin
      Handle.Proc := Nop_Callback'Access;

      if Handle.Mode = Repeat then
         --  Be sure the callback will be removed at the next call.
         Handle.Mode := Oneshot;
      end if;
   end Delete_Callback;

   function Get_First_Time (List : Callback_Time_List) return Std_Time is
   begin
      return List.First_Timeout;
   end Get_First_Time;

   function Has_Callbacks (List : Callback_List) return Boolean is
   begin
      return List.First /= null;
   end Has_Callbacks;

end Grt.Callbacks;
