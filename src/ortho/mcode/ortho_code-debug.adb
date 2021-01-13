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
with Ortho_Code.Flags;

package body Ortho_Code.Debug is
   procedure Disp_Mode (M : Mode_Type)
   is
      use Ada.Text_IO;
   begin
      case M is
         when Mode_U8 =>
            Put ("U8 ");
         when Mode_U16 =>
            Put ("U16");
         when Mode_U32 =>
            Put ("U32");
         when Mode_U64 =>
            Put ("U64");
         when Mode_I8 =>
            Put ("I8 ");
         when Mode_I16 =>
            Put ("I16");
         when Mode_I32 =>
            Put ("I32");
         when Mode_I64 =>
            Put ("I64");
         when Mode_X1 =>
            Put ("xxx");
         when Mode_Nil =>
            Put ("Nil");
         when Mode_F32 =>
            Put ("F32");
         when Mode_F64 =>
            Put ("F64");
         when Mode_B2 =>
            Put ("B2 ");
         when Mode_Blk =>
            Put ("Blk");
         when Mode_P32 =>
            Put ("P32");
         when Mode_P64 =>
            Put ("P64");
      end case;
   end Disp_Mode;

   procedure Set_Debug_Be_Flag (C : Character)
   is
      use Ada.Text_IO;
   begin
      case C is
         when 'a' =>
            Flag_Debug_Asm := True;
         when 'B' =>
            Flag_Debug_Body := True;
         when 'c' =>
            Flag_Debug_Code := True;
         when 'C' =>
            Flag_Debug_Code2 := True;
         when 'd' =>
            Flag_Debug_Dump := True;
         when 'h' =>
            Flag_Debug_Hex := True;
         when 'H' =>
            Flag_Debug_Hli := True;
         when 'i' =>
            Flag_Debug_Insn := True;
         when 's' =>
            Flag_Debug_Stat := True;
         when 'k' =>
            Flag_Debug_Keep := True;
         when 't' =>
            Flags.Flag_Type_Name := True;
         when others =>
            Put_Line (Standard_Error, "unknown debug be flag '" & C & "'");
      end case;
   end Set_Debug_Be_Flag;

   procedure Set_Be_Flag (Str : String)
   is
      use Ada.Text_IO;

      subtype Str_Type is String (1 .. Str'Length);
      S : Str_Type renames Str;
   begin
      if S'Length > 11 and then S (1 .. 11) = "--be-debug=" then
         for I in 12 .. S'Last loop
            Set_Debug_Be_Flag (S (I));
         end loop;
      elsif S'Length > 10 and then S (1 .. 10) = "--be-dump=" then
         for I in 11 .. S'Last loop
            case S (I) is
               when 'c' =>
                  Flag_Dump_Code := True;
               when others =>
                  Put_Line (Standard_Error,
                            "unknown back-end dump flag '" & S (I) & "'");
            end case;
         end loop;
      elsif S'Length > 10 and then S (1 .. 10) = "--be-disp=" then
         for I in 11 .. S'Last loop
            case S (I) is
               when 'c' =>
                  Flag_Disp_Code := True;
                  Flags.Flag_Type_Name := True;
               when others =>
                  Put_Line (Standard_Error,
                            "unknown back-end disp flag '" & S (I) & "'");
            end case;
         end loop;
      elsif S'Length > 9 and then S (1 .. 9) = "--be-opt=" then
         for I in 10 .. S'Last loop
            case S (I) is
               when 'O' =>
                  Flags.Flag_Optimize := True;
               when 'b' =>
                  Flags.Flag_Opt_BB := True;
               when others =>
                  Put_Line (Standard_Error,
                            "unknown back-end opt flag '" & S (I) & "'");
            end case;
         end loop;
      else
         Put_Line (Standard_Error, "unknown back-end option " & Str);
      end if;
   end Set_Be_Flag;
end Ortho_Code.Debug;
