--  Mcode back-end for ortho - Main subprogram.
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
with Ada.Unchecked_Conversion;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Binary_File; use Binary_File;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ortho_Code.Debug;
with Ortho_Mcode; use Ortho_Mcode;
with Ortho_Front; use Ortho_Front;
with Ortho_Code.Flags; use Ortho_Code.Flags;
with Binary_File.Elf;
with Binary_File.Coff;
with Binary_File.Memory;

procedure Ortho_Code_Main
is
   Output : String_Acc := null;
   type Format_Type is (Format_Coff, Format_Elf);
   Format : constant Format_Type := Format_Elf;

   First_File : Natural;
   Opt : String_Acc;
   Opt_Arg : String_Acc;
   Filename : String_Acc;
   Exec_Func : String_Acc;
   Res : Natural;
   I : Natural;
   Argc : Natural;
   Val : Integer;
   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => String_Acc, Object => String);

   procedure Write_Output
   is
      Fd : File_Descriptor;
   begin
      Fd := Create_File (Output.all, Binary);
      if Fd /= Invalid_FD then
         case Format is
            when Format_Elf =>
               Binary_File.Elf.Write (Fd);
            when Format_Coff =>
               Binary_File.Coff.Write (Fd);
         end case;
         Close (Fd);
      end if;
   end Write_Output;
begin
   First_File := Natural'Last;
   Exec_Func := null;
   Val := 0;
   Ortho_Front.Init;

   Argc := Argument_Count;
   I := 1;
   while I <= Argc loop
      declare
         Arg : constant String := Argument (I);
      begin
         if Arg (1) = '-' then
            if Arg'Length > 5 and then Arg (1 .. 5) = "--be-" then
               Ortho_Code.Debug.Set_Be_Flag (Arg);
               I := I + 1;
            elsif Arg = "-o" then
               if I = Argc then
                  Put_Line (Standard_Error, "error: missing filename to '-o'");
                  return;
               end if;
               Output := new String'(Argument (I + 1));
               I := I + 2;
            elsif Arg = "-quiet" then
               --  Skip silently.
               I := I + 1;
            elsif Arg = "--exec" then
               if I = Argc then
                  Put_Line (Standard_Error,
                            "error: missing function name to '--exec'");
                  return;
               end if;
               Exec_Func := new String'(Argument (I + 1));
               I := I + 2;
            elsif Arg = "-a" then
               if I = Argc then
                  Put_Line (Standard_Error,
                            "error: missing value after 'a'");
                  return;
               end if;
               Val := Integer'Value (Argument (I + 1));
               I := I + 2;
            elsif Arg = "-g" then
               Flag_Debug := Debug_Dwarf;
               I := I + 1;
            elsif Arg = "-g0" then
               Flag_Debug := Debug_None;
               I := I + 1;
            elsif Arg = "-p" or Arg = "-pg" then
               Flag_Profile := True;
               I := I + 1;
            else
               --  This is really an argument.
               Opt := new String'(Arg);
               if I < Argument_Count then
                  Opt_Arg := new String'(Argument (I + 1));
               else
                  Opt_Arg := null;
               end if;
               Res := Ortho_Front.Decode_Option (Opt, Opt_Arg);
               case Res is
                  when 0 =>
                     Put_Line (Standard_Error, "unknown option '" & Arg & "'");
                     return;
                  when 1 =>
                     I := I + 1;
                  when 2 =>
                     I := I + 2;
                  when others =>
                     raise Program_Error;
               end case;
               Unchecked_Deallocation (Opt);
               Unchecked_Deallocation (Opt_Arg);
            end if;
         else
            First_File := I;
            exit;
         end if;
      end;
   end loop;

   Ortho_Mcode.Init;

   Set_Exit_Status (Failure);

   if First_File > Argument_Count then
      begin
         if not Parse (null) then
            return;
         end if;
      exception
         when others =>
            return;
      end;
   else
      for I in First_File .. Argument_Count loop
         Filename := new String'(Argument (First_File));
         begin
            if not Parse (Filename) then
               return;
            end if;
         exception
            when others =>
               return;
         end;
      end loop;
   end if;

   Ortho_Mcode.Finish;

   if Ortho_Code.Debug.Flag_Debug_Hli then
      Set_Exit_Status (Success);
      return;
   end if;

   if Exec_Func /= null then
      declare
         Sym : Symbol;

         procedure Putchar (V : Integer);
         pragma Import (C, Putchar);

         type Func_Acc is access function (V : Integer) return Integer;
         function Conv is new Ada.Unchecked_Conversion
           (Source => Pc_Type, Target => Func_Acc);
         F : Func_Acc;

         --  Set a breakpoint on this procedure under a debugger if you need
         --  to debug the resulting binary in memory.
         procedure Breakme (Func : Func_Acc) is
         begin
            F := Func;
         end Breakme;

         V : Integer;
         Err : Boolean;
      begin
         Binary_File.Memory.Write_Memory_Init;

         --  Export putchar.
         Sym := Binary_File.Get_Symbol ("putchar");
         if Sym /= Null_Symbol then
            Binary_File.Memory.Set_Symbol_Address (Sym, Putchar'Address);
         end if;

         --  Relocate.
         Binary_File.Memory.Write_Memory_Relocate (Err);
         if Err then
            return;
         end if;

         --  Dump the binary file.
         if Output /= null then
            Write_Output;
         end if;

         Sym := Binary_File.Get_Symbol (Exec_Func.all);
         if Sym = Null_Symbol then
            Put_Line (Standard_Error, "no '" & Exec_Func.all & "' symbol");
         else
            Breakme (Conv (Get_Symbol_Vaddr (Sym)));
            V := F.all (Val);
            Put_Line ("Result is " & Integer'Image (V));
         end if;
      end;
   elsif Output /= null then
      Write_Output;
   end if;

   Set_Exit_Status (Success);
exception
   when others =>
      Set_Exit_Status (2);
      raise;
end Ortho_Code_Main;
