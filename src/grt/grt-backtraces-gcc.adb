--  GHDL Run Time (GRT) - Symbolization using gcc libbacktrace.
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

with System; use System;

package body Grt.Backtraces.Gcc is
   --  From backtrace.h
   type Backtrace_Error_Callback is access procedure
     (Data : System.Address; Msg : Address; Errnum : Integer);
   pragma Convention (C, Backtrace_Error_Callback);

   type Backtrace_State is null record;
   pragma Convention (C, Backtrace_State);

   type Backtrace_State_Acc is access all Backtrace_State;
   pragma Convention (C, Backtrace_State_Acc);

   function Backtrace_Create_State (Filename : Address;
                                    Threaded : Integer;
                                    Error_Callback : Backtrace_Error_Callback;
                                    Data : System.Address)
                                   return Backtrace_State_Acc;
   pragma Import (C, Backtrace_Create_State);

   type Backtrace_Full_Callback is access function
     (Data : System.Address;
      Pc : Address;
      Filename : System.Address;
      Lineno : Integer;
      Function_Name : System.Address)
     return Integer;
   pragma Convention (C, Backtrace_Full_Callback);


   function Backtrace_Pcinfo (State : Backtrace_State_Acc;
                              Pc : Address;
                              Callback : Backtrace_Full_Callback;
                              Error_Callback : Backtrace_Error_Callback;
                              Data : System.Address)
                             return Integer;
   pragma Import (C, Backtrace_Pcinfo);

   State : Backtrace_State_Acc;
   Initialized : Boolean := False;

   Res_Filename : System.Address;
   Res_Lineno : Natural;
   Res_Subprg : System.Address;

   procedure Error_Cb
     (Data : Address; Msg : Address; Errnum : Integer);
   pragma Convention (C, Error_Cb);

   procedure Error_Cb
     (Data : Address; Msg : Address; Errnum : Integer) is
   begin
      null;
   end Error_Cb;

   function Cb (Data : System.Address;
                Pc : Address;
                Filename : System.Address;
                Lineno : Integer;
                Function_Name : System.Address)
               return Integer;
   pragma Convention (C, Cb);

   function Cb (Data : System.Address;
                Pc : Address;
                Filename : System.Address;
                Lineno : Integer;
                Function_Name : System.Address)
               return Integer
   is
      pragma Unreferenced (Data, Pc);
   begin
      if Res_Filename = Null_Address then
         Res_Filename := Filename;
         Res_Lineno := Lineno;
         Res_Subprg := Function_Name;
      end if;
      return 0;
   end Cb;

   procedure Symbolizer (Pc : System.Address;
                         Filename : out System.Address;
                         Lineno : out Natural;
                         Subprg : out System.Address)
   is
      Res : Integer;
      pragma Unreferenced (Res);
   begin
      if not Initialized then
         Initialized := True;
         State := Backtrace_Create_State
           (Null_Address, 0, Error_Cb'Access, Null_Address);
      end if;

      Res_Filename := Null_Address;
      Res_Lineno := 0;
      Res_Subprg := Null_Address;

      if State /= null then
         Res := Backtrace_Pcinfo
           (State, Pc, Cb'access, Error_Cb'Access, Null_Address);
      end if;

      Filename := Res_Filename;
      Lineno := Res_Lineno;
      Subprg := Res_Subprg;
   end Symbolizer;
end Grt.Backtraces.Gcc;
