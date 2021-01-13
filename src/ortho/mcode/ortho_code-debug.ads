--  Mcode back-end for ortho - Internal debugging.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Text_IO;

package Ortho_Code.Debug is
   package Int32_IO is new Ada.Text_IO.Integer_IO (Ortho_Code.Int32);

   procedure Disp_Mode (M : Mode_Type);

   --  Set a debug flag (--be-debug=X).
   procedure Set_Debug_Be_Flag (C : Character);

   --  any '--be-XXX=YY' option.
   procedure Set_Be_Flag (Str : String);

   --  --be-disp=c: tree created, before any back-end.
   Flag_Disp_Code : Boolean := False;
   --  --be-dump=c: tree created, before any back-end.
   Flag_Dump_Code : Boolean := False;

   --  a: disp assembly code.
   Flag_Debug_Asm : Boolean := False;

   --  A: do internal checks (assertions).
   Flag_Debug_Assert : Boolean := True;

   --  B: dump generated insns (at the end of insn generation).
   Flag_Debug_Body : Boolean := False;

   --  c: display generated insns (at the end of insn generation).
   --  This is a log dump.
   Flag_Debug_Code : Boolean := False;

   --  C: display insns when generating code.  Useful to debug code generation.
   Flag_Debug_Code2 : Boolean := False;

   --  h: disp bytes generated (in hexa).
   Flag_Debug_Hex : Boolean := False;

   --  H: generate high-level instructions.
   Flag_Debug_Hli : Boolean := False;

   --  d: raw dump, do not generate code.
   Flag_Debug_Dump : Boolean := False;

   --  i: disp insns, when generated.
   --  The output may be misleading as a spill inserted later is not displayed.
   --  Useful only when debugging insn generation.  Use --be-debug=c to view
   --  the correct output.
   Flag_Debug_Insn : Boolean := False;

   --  s: disp stats (number of nodes).
   Flag_Debug_Stat : Boolean := False;

   --  k: keep all nodes in memory (do not free).
   Flag_Debug_Keep: Boolean := False;
end Ortho_Code.Debug;
