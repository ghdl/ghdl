--  GHDL Run Time (GRT) stdio subprograms for GRT types.
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
with Grt.Options;

package body Grt.Astdio.Vhdl is
   procedure Put_Str_Len (Stream : FILEs; Str : Ghdl_Str_Len_Type)
   is
      S : String (1 .. 3);
   begin
      if Str.Str = null then
         S (1) := ''';
         S (2) := Character'Val (Str.Len);
         S (3) := ''';
         Put (Stream, S);
      else
         Put (Stream, Str.Str (1 .. Str.Len));
      end if;
   end Put_Str_Len;

   procedure Put_Dir (Stream : FILEs; Dir : Ghdl_Dir_Type) is
   begin
      case Dir is
         when Dir_To =>
            Put (Stream, " to ");
         when Dir_Downto =>
            Put (Stream, " downto ");
      end case;
   end Put_Dir;

   procedure Put_Time (Stream : FILEs; Time : Std_Time)
   is
      use Grt.Options;
      Unit : Natural_Time_Scale;
      T : Std_Time;
   begin
      if Time = Std_Time'First then
         Put (Stream, "-Inf");
      else
         --  Do not bother with sec, min, and hr.
         Unit := Time_Resolution_Scale;
         T := Time;
         while Unit > 1 and then (T mod 1_000) = 0 loop
            T := T / 1000;
            Unit := Unit - 1;
         end loop;
         Put_I64 (Stream, Ghdl_I64 (T));
         case Unit is
            when 0 =>
               Put (Stream, "sec");
            when 1 =>
               Put (Stream, "ms");
            when 2 =>
               Put (Stream, "us");
            when 3 =>
               Put (Stream, "ns");
            when 4 =>
               Put (Stream, "ps");
            when 5 =>
               Put (Stream, "fs");
         end case;
      end if;
   end Put_Time;

end Grt.Astdio.Vhdl;
