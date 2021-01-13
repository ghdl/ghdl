--  GHDL Run Time (GRT) - Error handling during execution (backtrace).
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
with Grt.Types; use Grt.Types;

package Grt.Errors_Exec is
   --  Complete error message with a call stack.  SKIP is the number of
   --  frame to skip, 0 means the caller of this procedure is displayed.
   procedure Error_Call_Stack (Str : String; Skip : Natural);

   --  Backtrace used to report call stack in case of error.
   --  Note: for simplicity we assume that a PC is enough to display the
   --  corresponding file name, line number and routine name.  Might not be
   --  true on some platforms.
   --  There is a C version of this record in grt_itf.h
   type Integer_Address_Array is array (Natural range <>) of Integer_Address;
   type Backtrace_Addrs is record
      Size : Natural;
      Skip : Natural;
      Addrs : Integer_Address_Array (0 .. 31);
   end record;
   pragma Convention (C, Backtrace_Addrs);

   type Backtrace_Addrs_Acc is access Backtrace_Addrs;

   --  Save the current backtrace to BT, but skip SKIP frame.  0 means that
   --  the caller of this procedure will be in the backtrace.
   procedure Save_Backtrace (Bt : out Backtrace_Addrs; Skip : Natural);
   pragma Import (C, Save_Backtrace, "grt_save_backtrace");

   --  Finish error message with a call stack.
   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs);
   pragma No_Return (Error_E_Call_Stack);

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Error_E_Call_Stack);

   --  Display an error message for an overflow.
   procedure Grt_Overflow_Error (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Grt_Overflow_Error);

   --  Display an error message for a NULL access dereference.
   procedure Grt_Null_Access_Error (Bt : Backtrace_Addrs_Acc);
   pragma No_Return (Grt_Null_Access_Error);
private
   pragma Export (C, Grt_Overflow_Error, "grt_overflow_error");
   pragma Export (C, Grt_Null_Access_Error, "grt_null_access_error");
end Grt.Errors_Exec;
