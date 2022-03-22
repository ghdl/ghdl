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
with System;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

--  Callbacks are user registered procedures that are called during simulation.
--  They are used to implement vpi/vhpi callbacks, but also some features like
--  vcd or fst.

package Grt.Callbacks is
   pragma Preelaborate (Grt.Callbacks);

   --  It would be nice to use OOP (tagged types and overriding), but this is
   --  not anymore available in the context of pragma No_Run_Time.
   --  Furthermore, that wouldn't be that convenient because of lack of
   --  multiple inheritance.
   --
   --  Thus the use of a 'generic' callback type.  The type Address is used for
   --  any pointer type.
   type Callback_Acc is access procedure (Arg : System.Address);

   type Callback_Handle is private;

   type Callback_List is limited private;
   pragma Preelaborable_Initialization (Callback_List);

   type Callback_Time_List is limited private;
   pragma Preelaborable_Initialization (Callback_Time_List);

   --  Register a timeout: PROC will be called with parameter ARG at time T
   --  at the beginning of the cycle (before any process).  Insertion is O(n).
   procedure Register_Callback_At
     (List : in out Callback_Time_List;
      Handle : out Callback_Handle;
      T : Std_Time;
      Proc : Callback_Acc;
      Arg : System.Address := System.Null_Address);

   type Callback_Mode is (Timed, Repeat, Oneshot);
   subtype Callback_Non_Timed_Mode is Callback_Mode range Repeat .. Oneshot;

   procedure Register_Callback
     (List : in out Callback_List;
      Handle : out Callback_Handle;
      Mode : Callback_Mode;
      Proc : Callback_Acc;
      Arg : System.Address := System.Null_Address);

   --  Delete callback.
   --  In fact the callback is just marked as deleted, but will be removed
   --  only at the point it would be called.
   procedure Delete_Callback (Handle : Callback_Handle);

   --  Call the callbacks.
   procedure Call_Callbacks (List : in out Callback_List);
   procedure Call_Time_Callbacks (List : in out Callback_Time_List);

   --  Return the date of the earliest callbacks (or Std_Time'Last if none).
   function Get_First_Time (List : Callback_Time_List) return Std_Time;
   pragma Inline (Get_First_Time);

   --  Return True if there is at least one callback in the list.
   function Has_Callbacks (List : Callback_List) return Boolean;
   pragma Inline (Has_Callbacks);
private
   type Cb_Cell;
   type Callback_Handle is access Cb_Cell;

   type Cb_Cell is record
      T : Std_Time;
      Mode : Callback_Mode;
      Proc : Callback_Acc;
      Arg : System.Address;
      Next : Callback_Handle;
   end record;

   type Callback_List is limited record
      First, Last : Callback_Handle := null;
   end record;

   type Callback_Time_List is limited record
      First : Callback_Handle := null;
      First_Timeout : Std_Time := Std_Time'Last;
   end record;
end Grt.Callbacks;
