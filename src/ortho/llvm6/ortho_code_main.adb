--  LLVM back-end for ortho - Main subprogram.
--  Copyright (C) 2014 Tristan Gingold
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Ortho_Front; use Ortho_Front;
with Ortho_LLVM; use Ortho_LLVM;

procedure Ortho_Code_Main is
   --  Name of the output filename (given by option '-o').
   Output : String_Acc := null;

   type Output_Kind_Type is (Output_Llvm, Output_Bitcode,
                             Output_Assembly, Output_Object);
   Output_Kind : Output_Kind_Type := Output_Object;

   --  Index of the first file argument.
   First_File : Natural;

   --  Current option index.
   Optind : Natural;

   --  Number of arguments.
   Argc : constant Natural := Argument_Count;
begin
   Ortho_Front.Init;

   --  Decode options.
   First_File := Natural'Last;
   Optind := 1;
   while Optind <= Argc loop
      declare
         Arg : constant String := Argument (Optind);
      begin
         if Arg (1) = '-' then
            if Arg = "--dump-llvm" then
               Set_Dump_LLVM (1);
            elsif Arg = "--verify-llvm" then
               Set_Verify_LLVM (1);
            elsif Arg = "-o" then
               if Optind = Argc then
                  Put_Line (Standard_Error, "error: missing filename to '-o'");
                  return;
               end if;
               Output := new String'(Argument (Optind + 1) & ASCII.Nul);
               Optind := Optind + 1;
            elsif Arg = "-quiet" then
               --  Skip silently.
               null;
            elsif Arg = "-S" then
               Output_Kind := Output_Assembly;
               --  Codegen := AssemblyFile;
            elsif Arg = "-c" then
               Output_Kind := Output_Object;
            elsif Arg = "-O0" then
               Set_Optimization_Level (0);
            elsif Arg = "-O1" or else Arg = "-O" then
               Set_Optimization_Level (1);
            elsif Arg = "-O2" then
               Set_Optimization_Level (2);
            elsif Arg = "-O3" then
               Set_Optimization_Level (3);
            elsif Arg = "-fpic" or Arg = "-fPIC" then
               Set_PIC_Flag (1);
            elsif Arg = "-fno-pic" then
               Set_PIC_Flag (0);
            elsif Arg = "--emit-llvm" then
               Output_Kind := Output_Llvm;
            elsif Arg = "--emit-bc" then
               Output_Kind := Output_Bitcode;
            elsif Arg = "-glines"
              or else Arg = "-gline-tables-only"
            then
               Set_Debug_Level (1);
            elsif Arg = "-g" then
               Set_Debug_Level (2);
            elsif Arg = "-g0" then
               Set_Debug_Level (0);
            else
               --  This is really an argument.
               declare
                  procedure Unchecked_Deallocation is
                     new Ada.Unchecked_Deallocation
                    (Name => String_Acc, Object => String);

                  Opt : String_Acc := new String'(Arg);
                  Opt_Arg : String_Acc;
                  Res : Natural;
               begin
                  Opt_Arg := null;
                  if Optind < Argument_Count then
                     declare
                        Arg1 : constant String := Argument (Optind + 1);
                     begin
                        if Arg1 (Arg1'First) /= '-' then
                           Opt_Arg := new String'(Arg1);
                        end if;
                     end;
                  end if;

                  Res := Ortho_Front.Decode_Option (Opt, Opt_Arg);
                  case Res is
                     when 0 =>
                        Put_Line (Standard_Error,
                                  "unknown option '" & Arg & "'");
                        return;
                     when 1 =>
                        null;
                     when 2 =>
                        Optind := Optind + 1;
                     when others =>
                        raise Program_Error;
                  end case;
                  Unchecked_Deallocation (Opt);
                  Unchecked_Deallocation (Opt_Arg);
               end;
            end if;
         else
            First_File := Optind;
            exit;
         end if;
      end;
      Optind := Optind + 1;
   end loop;

   if First_File < Argument_Count then
      Put_Line (Standard_Error, "error: too many source filenames");
      return;
   end if;

   Set_Exit_Status (Failure);

   declare
      Filename : String_Acc;
   begin
      if First_File > Argument_Count then
         Filename := new String'("*stdin*");
      else
         Filename := new String'(Argument (First_File));
      end if;

      Ortho_LLVM.Init (Filename.all, Filename'Length);

      if not Parse (Filename) then
         --  Parse error.
         return;
      end if;
   exception
      when others =>
         return;
   end;

   if Output /= null then
      case Output_Kind is
         when Output_Object =>
            Generate_Object (Output.all'Address);
         when Output_Assembly =>
            Generate_Assembly (Output.all'Address);
         when Output_Bitcode =>
            Generate_Bitcode (Output.all'Address);
         when Output_Llvm =>
            Generate_Llvm (Output.all'Address);
      end case;
   end if;

   Set_Exit_Status (Success);
exception
   when others =>
      Set_Exit_Status (2);
      raise;
end Ortho_Code_Main;
