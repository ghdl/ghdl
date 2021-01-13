--  ELF dumper (main program).
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
with Ada.Text_IO; use Ada.Text_IO;
with Elf_Common; use Elf_Common;
with Ada.Command_Line; use Ada.Command_Line;
with Hex_Images; use Hex_Images;
with Interfaces; use Interfaces;
with Elfdumper; use Elfdumper;

procedure Elfdump is
   Flag_Ehdr : Boolean := False;
   Flag_Shdr : Boolean := False;
   Flag_Strtab : Boolean := False;
   Flag_Symtab : Boolean := False;
   Flag_Dwarf_Info : Boolean := False;
   Flag_Dwarf_Abbrev : Boolean := False;
   Flag_Dwarf_Pubnames : Boolean := False;
   Flag_Dwarf_Aranges : Boolean := False;
   Flag_Dwarf_Line : Boolean := False;
   Flag_Dwarf_Frame : Boolean := False;
   Flag_Eh_Frame_Hdr : Boolean := False;
   Flag_Long_Shdr : Boolean := False;
   Flag_Phdr : Boolean := False;
   Flag_Note : Boolean := False;
   Flag_Dynamic : Boolean := False;

   procedure Disp_Max_Len (Str : String; Len : Natural)
   is
   begin
      if Str'Length > Len then
         Put (Str (Str'First .. Str'First + Len - 1));
      else
         Put (Str);
         Put ((Str'Length + 1 .. Len => ' '));
      end if;
   end Disp_Max_Len;

   procedure Disp_Section_Header (File : Elf_File; Index : Elf_Half) is
   begin
      Put ("Section " & Hex_Image (Index));
      Put (" ");
      Put (Get_Section_Name (File, Index));
      New_Line;
   end Disp_Section_Header;

   procedure Disp_Elf_File (Filename : String)
   is
      File : Elf_File;
      Ehdr : Elf_Ehdr_Acc;
      Shdr : Elf_Shdr_Acc;
      Phdr : Elf_Phdr_Acc;
      Sh_Strtab : Strtab_Type;
   begin
      Open_File (File, Filename);
      if Get_Status (File) /= Status_Ok then
         Put_Line ("cannot open elf file '" & Filename & "': " &
                   Elf_File_Status'Image (Get_Status (File)));
         return;
      end if;

      Ehdr := Get_Ehdr (File);

      if Flag_Ehdr then
         Disp_Ehdr (Ehdr.all);
      end if;

      Load_Shdr (File);
      Sh_Strtab := Get_Sh_Strtab (File);

      if Flag_Long_Shdr then
         if Ehdr.E_Shnum = 0 then
            Put ("no section");
         else
            for I in 0 .. Ehdr.E_Shnum - 1 loop
               Put ("Section " & Hex_Image (I));
               New_Line;
               Disp_Shdr (Get_Shdr (File, I).all, Sh_Strtab);
            end loop;
         end if;
      end if;
      if Flag_Shdr then
         if Ehdr.E_Shnum = 0 then
            Put ("no section");
         else
            Put ("Num   Name                Type       ");
            Put ("Offset   Size     Link Info Al Es");
            New_Line;
            for I in 0 .. Ehdr.E_Shnum - 1 loop
               declare
                  Shdr : Elf_Shdr_Acc := Get_Shdr (File, I);
               begin
                  Put (Hex_Image (I));
                  Put (" ");
                  Disp_Max_Len (Get_Section_Name (File, I), 20);
                  Put (" ");
                  Disp_Max_Len (Get_Shdr_Type_Name (Shdr.Sh_Type), 10);
                  Put (" ");
                  Put (Hex_Image (Shdr.Sh_Offset));
                  Put (" ");
                  Put (Hex_Image (Shdr.Sh_Size));
                  Put (" ");
                  Put (Hex_Image (Unsigned_16 (Shdr.Sh_Link and 16#Ffff#)));
                  Put (" ");
                  Put (Hex_Image (Unsigned_16 (Shdr.Sh_Info and 16#Ffff#)));
                  Put (" ");
                  Put (Hex_Image (Unsigned_8 (Shdr.Sh_Addralign and 16#ff#)));
                  Put (" ");
                  Put (Hex_Image (Unsigned_8 (Shdr.Sh_Entsize and 16#ff#)));
                  New_Line;
               end;
            end loop;
         end if;
      end if;

      if Flag_Phdr then
         Load_Phdr (File);
         if Ehdr.E_Phnum = 0 then
            Put ("no program segment");
         else
            for I in 0 .. Ehdr.E_Phnum - 1 loop
               Put ("segment " & Hex_Image (I));
               New_Line;
               Disp_Phdr (Get_Phdr (File, I).all);
            end loop;
         end if;
      end if;

      --  Dump each section.
      if Ehdr.E_Shnum > 0 then
         for I in 0 .. Ehdr.E_Shnum - 1 loop
            Shdr := Get_Shdr (File, I);
            case Shdr.Sh_Type is
               when SHT_SYMTAB =>
                  if Flag_Symtab then
                     Disp_Section_Header (File, I);
                     Disp_Symtab (File, I);
                  end if;
               when SHT_STRTAB =>
                  if Flag_Strtab then
                     Disp_Section_Header (File, I);
                     Disp_Strtab (File, I);
                  end if;
               when SHT_PROGBITS =>
                  declare
                     Name : String := Get_Section_Name (File, I);
                  begin
                     if Flag_Dwarf_Abbrev and then Name = ".debug_abbrev" then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Abbrev (File, I);
                     elsif Flag_Dwarf_Info and then Name = ".debug_info" then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Info (File, I);
                     elsif Flag_Dwarf_Line and then Name = ".debug_line" then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Line (File, I);
                     elsif Flag_Dwarf_Frame and then Name = ".debug_frame" then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Frame (File, I);
                     elsif Flag_Dwarf_Pubnames
                       and then Name = ".debug_pubnames"
                     then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Pubnames (File, I);
                     elsif Flag_Eh_Frame_Hdr and then Name = ".eh_frame_hdr"
                     then
                        Disp_Section_Header (File, I);
                        Disp_Eh_Frame_Hdr (File, I);
                     elsif Flag_Dwarf_Aranges
                       and then Name = ".debug_aranges"
                     then
                        Disp_Section_Header (File, I);
                        Disp_Debug_Aranges (File, I);
                     end if;
                  end;
               when SHT_NOTE =>
                  if Flag_Note then
                     Disp_Section_Header (File, I);
                     Disp_Section_Note (File, I);
                  end if;
               when SHT_DYNAMIC =>
                  if Flag_Dynamic then
                     Disp_Section_Header (File, I);
                     Disp_Dynamic (File, I);
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      elsif Ehdr.E_Phnum > 0 then
         Load_Phdr (File);
         for I in 0 .. Ehdr.E_Phnum - 1 loop
            Phdr := Get_Phdr (File, I);
            case Phdr.P_Type is
               when PT_NOTE =>
                  if Flag_Note then
                     Disp_Segment_Note (File, I);
                  end if;
               when others =>
                  null;
            end case;
         end loop;
      end if;
   end Disp_Elf_File;

begin
   for I in 1 .. Argument_Count loop
      declare
         Arg : String := Argument (I);
      begin
         if Arg (1) = '-' then
            --  An option.
            if Arg = "-e" then
               Flag_Ehdr := True;
            elsif Arg = "-t" then
               Flag_Strtab := True;
            elsif Arg = "-S" then
               Flag_Symtab := True;
            elsif Arg = "-s" then
               Flag_Shdr := True;
            elsif Arg = "-p" then
               Flag_Phdr := True;
            elsif Arg = "-n" then
               Flag_Note := True;
            elsif Arg = "-d" then
               Flag_Dynamic := True;
            elsif Arg = "--dwarf-info" then
               Flag_Dwarf_Info := True;
            elsif Arg = "--dwarf-abbrev" then
               Flag_Dwarf_Abbrev := True;
            elsif Arg = "--dwarf-line" then
               Flag_Dwarf_Line := True;
            elsif Arg = "--dwarf-frame" then
               Flag_Dwarf_Frame := True;
            elsif Arg = "--dwarf-pubnames" then
               Flag_Dwarf_Pubnames := True;
            elsif Arg = "--dwarf-aranges" then
               Flag_Dwarf_Aranges := True;
            elsif Arg = "--eh-frame-hdr" then
               Flag_Eh_Frame_Hdr := True;
            elsif Arg = "--long-shdr" then
               Flag_Long_Shdr := True;
            else
               Put_Line ("unknown option '" & Arg & "'");
               return;
            end if;
         else
            Disp_Elf_File (Arg);
         end if;
      end;
   end loop;
end Elfdump;

