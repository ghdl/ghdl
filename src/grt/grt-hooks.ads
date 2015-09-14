--  GHDL Run Time (GRT) -  Hooks.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
package Grt.Hooks is
   pragma Preelaborate (Grt.Hooks);

   type Option_Hook_Type is access function (Opt : String) return Boolean;
   type Proc_Hook_Type is access procedure;

   type Cst_String_Acc is access constant String;

   type Hooks_Type is record
      --  A one-line description of the hook.  The format is:
      --  "NAME: description".  NAME should be uniq and is tested by the
      --  switch --has-feature=NAME.
      --  DESC can be null if there is no interesting feature added.
      Desc : Cst_String_Acc;

      --  Called for every unknown command line argument.
      --  Return TRUE if handled.
      Option : Option_Hook_Type;

      --  Display command line help.
      Help : Proc_Hook_Type;

      --  Called at initialization (after decoding options).
      Init : Proc_Hook_Type;

      --  Called just after elaboration.
      Start : Proc_Hook_Type;

      --  Called at the end of execution.
      Finish : Proc_Hook_Type;
   end record;

   type Hooks_Acc is access constant Hooks_Type;

   --  Registers hook.
   procedure Register_Hooks (Hooks : Hooks_Acc);

   --  Register an hook which will call PROC after every non-delta cycles.
   procedure Register_Cycle_Hook (Proc : Proc_Hook_Type);

   --  Display the description of the hooks.
   procedure Display_Hooks_Desc;

   --  Return True if NAME is present in the list of modules.
   function Has_Feature (Name : String) return Boolean;

   --  Call hooks.
   function Call_Option_Hooks (Opt : String) return Boolean;
   procedure Call_Help_Hooks;
   procedure Call_Init_Hooks;
   procedure Call_Start_Hooks;
   procedure Call_Finish_Hooks;

   --  Call non-delta cycles hooks.
   procedure Call_Cycle_Hooks;
   pragma Inline_Always (Call_Cycle_Hooks);

   --  Nil procedure.
   procedure Proc_Hook_Nil;
end Grt.Hooks;
