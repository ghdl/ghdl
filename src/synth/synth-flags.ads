--  Flags for synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

package Synth.Flags is
   Flag_Debug_Noinference : Boolean := False;

   Flag_Debug_Nocleanup : Boolean := False;

   Flag_Debug_Nomemory : Boolean := False;

   Flag_Debug_Noexpand : Boolean := False;

   Flag_Trace_Statements : Boolean := False;

   --  True to start debugger at elaboration.
   Flag_Debug_Init : Boolean := False;

   --  True to start debugger on error.
   Flag_Debug_Enable : Boolean := True;
end Synth.Flags;
