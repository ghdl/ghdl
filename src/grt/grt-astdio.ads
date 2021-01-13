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
with System;
with Grt.Types; use Grt.Types;
with Grt.Stdio; use Grt.Stdio;

package Grt.Astdio is
   pragma Preelaborate (Grt.Astdio);

   --  Procedures to disp on STREAM.
   procedure Put (Stream : FILEs; Str : String);
   procedure Put_Line (Stream : FILEs; Str : String);
   procedure Put_I32 (Stream : FILEs; I32 : Ghdl_I32);
   procedure Put_U32 (Stream : FILEs; U32 : Ghdl_U32);
   procedure Put_I64 (Stream : FILEs; I64 : Ghdl_I64);
   procedure Put_U64 (Stream : FILEs; U64 : Ghdl_U64);
   procedure Put_F64 (Stream : FILEs; F64 : Ghdl_F64);
   procedure Put (Stream : FILEs; Addr : System.Address);
   procedure Put (Stream : FILEs; Str : Ghdl_C_String);
   procedure Put (Stream : FILEs; C : Character);
   procedure New_Line (Stream : FILEs);

   --  And on stdout.
   procedure Put (Str : String);
   procedure Put (C : Character);
   procedure New_Line;
   procedure Put_Line (Str : String);
   procedure Put (Str : Ghdl_C_String);
end Grt.Astdio;
