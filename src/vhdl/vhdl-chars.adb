--  VHDL characters.
--  Copyright (C) 2026 Tristan Gingold
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

with Flags; use Flags;
with Errorout; use Errorout;

package body Vhdl.Chars is
   function Is_Whitespace (C : Character) return Boolean is
   begin
      if C = ' ' then
         return True;
      elsif Vhdl_Std > Vhdl_87 and C = NBSP then
         return True;
      else
         return False;
      end if;
   end Is_Whitespace;

   procedure Convert_Identifier (Str : in out String; Err : out Boolean)
   is
      F : constant Integer := Str'First;

      procedure Error_Bad is
      begin
         Error_Msg_Option ("bad character in identifier");
      end Error_Bad;

      procedure Error_8bit is
      begin
         Error_Msg_Option ("8 bits characters not allowed in vhdl87");
      end Error_8bit;

      C : Character;
   begin
      Err := True;

      if Str'Length = 0 then
         Error_Msg_Option ("identifier required");
         return;
      end if;

      if Str (F) = '\' then
         --  Extended identifier.
         if Vhdl_Std = Vhdl_87 then
            Error_Msg_Option ("extended identifiers not allowed in vhdl87");
            return;
         end if;

         if Str'Last < F + 2 then
            Error_Msg_Option ("extended identifier is too short");
            return;
         end if;
         if Str (Str'Last) /= '\' then
            Error_Msg_Option ("extended identifier must finish with a '\'");
            return;
         end if;
         for I in F + 1 .. Str'Last - 1 loop
            C := Str (I);
            case Characters_Kind (C) is
               when Format_Effector =>
                  Error_Msg_Option ("format effector in extended identifier");
                  return;
               when Graphic_Character =>
                  if C = '\' then
                     if Str (I + 1) /= '\'
                       or else I = Str'Last - 1
                     then
                        Error_Msg_Option ("anti-slash must be doubled "
                                            & "in extended identifier");
                        return;
                     end if;
                  end if;
               when Invalid =>
                  Error_Bad;
                  return;
            end case;
         end loop;
      else
         --  Identifier
         for I in F .. Str'Last loop
            C := Str (I);
            case Characters_Kind (C) is
               when Upper_Case_Letter =>
                  if Vhdl_Std = Vhdl_87 and C > 'Z' then
                     Error_8bit;
                     return;
                  end if;
                  Str (I) := To_Lower_Map (C);
               when Lower_Case_Letter | Digit =>
                  if Vhdl_Std = Vhdl_87 and C > 'z' then
                     Error_8bit;
                     return;
                  end if;
               when Special_Character =>
                  -- The current character is legal in an identifier.
                  if C = '_' then
                     if I = F then
                        Error_Msg_Option
                          ("an identifier cannot start with an underscore");
                        return;
                     end if;
                     if Str (I - 1) = '_' then
                        Error_Msg_Option
                          ("two underscores can't be consecutive");
                        return;
                     end if;
                     if I = Str'Last then
                        Error_Msg_Option
                          ("an identifier cannot finish with an underscore");
                        return;
                     end if;
                  else
                     Error_Bad;
                     return;
                  end if;
               when others =>
                  Error_Bad;
                  return;
            end case;
         end loop;
      end if;
      Err := False;
   end Convert_Identifier;
end Vhdl.Chars;
