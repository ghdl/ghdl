--  GHDL Run Time (GRT) - Error handling.
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

with Grt.Errors; use Grt.Errors;
with Grt.Backtraces;

with Grt.Signals;
with Grt.Rtis_Addr;
with Grt.Threads;
with Grt.Processes;
with Grt.Rtis_Utils;

package body Grt.Errors_Exec is
   procedure Error_Call_Stack (Str : String; Skip : Natural)
   is
      Bt : Backtrace_Addrs;
   begin
      Save_Backtrace (Bt, Skip + 1);
      Diag_C (Str);
      Error_E_Call_Stack (Bt);
   end Error_Call_Stack;

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs)
   is
      use Grt.Signals;
      use Grt.Rtis_Addr;
      use Grt.Threads;
      use Grt.Processes;
      use Grt.Rtis_Utils;
      Proc : Process_Acc;
      Proc_Rti : Rti_Context;
   begin
      Newline_Err;

      Proc := Get_Current_Process;
      if Proc /= null then
         Proc_Rti := Get_Rti_Context (Proc);
         if Proc_Rti /= Null_Context then
            Diag_C ("in process ");
            Put (Get_Error_Stream, Proc_Rti);
            Newline_Err;
         end if;
      end if;

      Grt.Backtraces.Put_Err_Backtrace (Bt);

      --  Should be able to call Error_E, but we don't want the newline.
      Fatal_Error;
   end Error_E_Call_Stack;

   procedure Error_E_Call_Stack (Bt : Backtrace_Addrs_Acc) is
   begin
      if Bt /= null then
         Error_E_Call_Stack (Bt.all);
      else
         Error_E;
      end if;
   end Error_E_Call_Stack;

   procedure Grt_Overflow_Error (Bt : Backtrace_Addrs_Acc) is
   begin
      Error_S ("overflow detected");
      Error_E_Call_Stack (Bt);
   end Grt_Overflow_Error;

   procedure Grt_Null_Access_Error (Bt : Backtrace_Addrs_Acc) is
   begin
      Error_S ("NULL access dereferenced");
      Error_E_Call_Stack (Bt);
   end Grt_Null_Access_Error;
end Grt.Errors_Exec;
