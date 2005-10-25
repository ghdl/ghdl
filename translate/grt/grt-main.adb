--  GHDL Run Time (GRT) -  entry point.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
with Grt.Types; use Grt.Types;
with Grt.Errors;
with Grt.Vcd;
with Grt.Vcdz;
with Grt.Vpi;
with Grt.Waves;
with Grt.Stacks;
with Grt.Processes;
with Grt.Signals;
with Grt.Options; use Grt.Options;
with Grt.Disp_Rti;
with Grt.Stats;
with Grt.Hooks;
with Grt.Disp_Signals;
with Grt.Disp;

--  The following packages are not referenced in this package.
--  They are with'ed in order to be present in the binary.
pragma Warnings (Off);
with Grt.Files;
with Grt.Types;
with Grt.Lib;
with Grt.Shadow_Ieee;
with Grt.Images;
with Grt.Values;
with Grt.Names;
with Grt.Vital_Annotate;
pragma Warnings (On);

package body Grt.Main is
   procedure Ghdl_Elaborate;
   pragma Import (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

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
      if Err then
         Grt.Errors.Error
           ("GRT is not consistent with the flags used for your design");
      end if;
   end Check_Flag_String;

   procedure Register_Modules is
   begin
      --  List of modules to be registered.
      Grt.Vcd.Register;
      Grt.Vcdz.Register;
      Grt.Waves.Register;
      Grt.Vpi.Register;
      Grt.Vital_Annotate.Register;
   end Register_Modules;

   procedure Run
   is
      use Grt.Errors;
      Stop : Boolean;
      Status : Integer;
   begin
      Register_Modules;

      --  If the time resolution is to be set by the user, select a default
      --  resolution.  Options may override it.
      if Flag_String (5) = '?' then
         Set_Time_Resolution ('n');
      end if;

      Grt.Options.Decode (Stop);

      Check_Flag_String;

      --  Early stop (for options such as --help).
      if Stop then
         return;
      end if;

      --  Internal initializations.
      Grt.Stacks.Stack_Init;

      Grt.Hooks.Call_Init_Hooks;

      Grt.Processes.Init;

      Grt.Signals.Init;

      if Flag_Stats then
         if Boolean'(False) then
            --  Replaced by Setjump/Longjump.
            Grt.Errors.Ghdl_Exit_Cb1 := Disp_Stats_Hook'Access;
         end if;
         Stats.Start_Elaboration;
      end if;

      --  Elaboration.
      Ghdl_Elaborate;

      if Flag_Stats then
         Stats.Start_Order;
      end if;

      if Disp_Tree /= Disp_Tree_None then
         Grt.Disp_Rti.Disp_Hierarchy;
      end if;

      if not Flag_No_Run then
         if Grt.Options.Flag_Dump_Rti then
            Grt.Disp_Rti.Disp_All;
         end if;

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

         if Flag_Stats then
            Stats.Start_Cycles;
         end if;

         --  Do the simulation.
         Status := Grt.Processes.Simulation;
      end if;

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
