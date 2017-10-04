--  GHDL Run Time (GRT) -  entry point.
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
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Types; use Grt.Types;
with Grt.Stdio;
with Grt.Errors;
with Grt.Processes;
with Grt.Signals;
with Grt.Options; use Grt.Options;
with Grt.Stats;
with Grt.Hooks;
with Grt.Disp_Signals;
with Grt.Disp;
with Grt.Modules;
with Grt.Change_Generics;

--  The following packages are not referenced in this package.
--  These are subprograms called only from GHDL generated code.
--  They are with'ed in order to be present in the binary.
pragma Warnings (Off);
with Grt.Files;
with Grt.Types;
with Grt.Lib;
with Grt.Shadow_Ieee;
with Grt.Images;
with Grt.Values;
with Grt.Names;
pragma Warnings (On);

package body Grt.Main is
   procedure Ghdl_Elaborate;
   pragma Import (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   --  Wrapper around elaboration just to return 0.
   function Ghdl_Elaborate_Wrapper return Integer is
   begin
      Ghdl_Elaborate;
      return 0;
   end Ghdl_Elaborate_Wrapper;

   procedure Ghdl_Init_Top_Generics is
   begin
      Grt.Change_Generics.Change_All_Generics;
      Grt.Change_Generics.Check_Required_Generic_Override;
   end Ghdl_Init_Top_Generics;

   procedure Disp_Stats_Hook (Code : Integer);
   pragma Convention (C, Disp_Stats_Hook);

   procedure Disp_Stats_Hook (Code : Integer)
   is
      pragma Unreferenced (Code);
   begin
      Stats.End_Simulation;
      Stats.Disp_Stats;
   end Disp_Stats_Hook;

   procedure Check_Flag_String
   is
      Err : Boolean;
   begin
      --  The conditions may be statically known.
      pragma Warnings (Off);

      Err := False;
      if (Std_Integer'Size = 32 and Flag_String (3) /= 'i')
        or else (Std_Integer'Size = 64 and Flag_String (3) /= 'I')
      then
         Err := True;
      end if;
      if (Std_Time'Size = 32 and Flag_String (4) /= 't')
        or else (Std_Time'Size = 64 and Flag_String (4) /= 'T')
      then
         Err := True;
      end if;

      pragma Warnings (On);

      if Err then
         Grt.Errors.Error
           ("GRT is not consistent with the flags used for your design");
      end if;
   end Check_Flag_String;

   procedure Run
   is
      use Grt.Errors;
      Stop : Boolean;
      Status : Integer;
   begin
      --  Set stream for error messages
      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

      --  Register modules.
      --  They may insert hooks.
      Grt.Modules.Register_Modules;

      --  If the time resolution is to be set by the user, select a default
      --  resolution.  Options may override it.
      if Flag_String (5) = '?' then
         Set_Time_Resolution ('n');
      end if;

      --  Decode options.
      Grt.Options.Decode (Stop);

      --  Early stop (for options such as --help).
      if Stop then
         return;
      end if;

      --  Check coherency between GRT and GHDL generated code.
      Check_Flag_String;

      --  Internal initializations.
      Grt.Hooks.Call_Init_Hooks;

      Grt.Processes.Init;

      Grt.Signals.Init;

      if Flag_Stats then
         Stats.Start_Elaboration;
      end if;

      --  Elaboration.  Run through longjump to catch errors.
      if Run_Through_Longjump (Ghdl_Elaborate_Wrapper'Access) < 0 then
         Grt.Errors.Error ("error during elaboration");
         return;
      end if;

      if Flag_Stats then
         Stats.Start_Order;
      end if;

      Grt.Hooks.Call_Start_Hooks;

      if not Flag_No_Run then
         Grt.Signals.Order_All_Signals;

         if Grt.Options.Disp_Signals_Map then
            Grt.Disp_Signals.Disp_Signals_Map;
         end if;
         if Grt.Options.Disp_Signals_Table then
            Grt.Disp_Signals.Disp_Signals_Table;
         end if;
         if Disp_Signals_Order then
            Grt.Disp.Disp_Signals_Order;
         end if;
         if Disp_Sensitivity then
            Grt.Disp_Signals.Disp_All_Sensitivity;
         end if;

         --  Do the simulation.
         Status := Run_Through_Longjump (Grt.Processes.Simulation'Access);
      end if;

      Grt.Hooks.Call_Finish_Hooks;

      if Flag_Stats then
         Disp_Stats_Hook (0);
      end if;

      if Expect_Failure then
         if Status >= 0 then
            Expect_Failure := False;
            Error ("error expected, but none occured");
         end if;
      else
         if Status < 0 then
            Error ("simulation failed");
         end if;
      end if;
   end Run;

end Grt.Main;
