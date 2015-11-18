--  GHDL Run Time (GRT) - Backtraces and symbolization.
--  Copyright (C) 2015 Tristan Gingold
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

with System;
with Grt.Types; use Grt.Types;
with Grt.Hooks; use Grt.Hooks;

package body Grt.Backtraces is
   --  If true, disp address in backtraces.
   Flag_Address : Boolean := False;

   subtype Address_Image_String is String (1 .. Integer_Address'Size / 4);

   Hex : constant array (Natural range 0 .. 15) of Character :=
     "0123456789abcdef";

   function Address_Image (Addr : Integer_Address)
                          return Address_Image_String
   is
      V : Integer_Address;
      Res : Address_Image_String;
   begin
      V := Addr;
      for I in reverse Res'Range loop
         Res (I) := Hex (Natural (V mod 16));
         V := V / 16;
      end loop;
      return Res;
   end Address_Image;

   function File_Basename (Name : Ghdl_C_String) return Ghdl_C_String
   is
      Sep : Natural;
   begin
      Sep := 0;
      for I in Name'Range loop
         case Name (I) is
            when '\' | '/' =>
               Sep := I + 1;
            when NUL =>
               exit;
            when others =>
               null;
         end case;
      end loop;
      if Sep /= 0 and then Name (Sep) /= NUL then
         return To_Ghdl_C_String (Name (Sep)'Address);
      else
         return Name;
      end if;
   end File_Basename;

   function Is_Eq (Str : Ghdl_C_String; Ref : String) return Boolean is
   begin
      for I in Ref'Range loop
         if Str (Str'First + I - Ref'First) /= Ref (I) then
            return False;
         end if;
      end loop;
      return Str (Str'First + Ref'Length) = NUL;
   end Is_Eq;

   procedure Demangle_Err (Name : Ghdl_C_String)
   is
      Last_Part : Natural;
      Suffix : Ghdl_C_String;
      Off : Natural;
      C : Character;
      Is_Arch : Boolean;
   begin
      if Name (1) = '_' then
         if Is_Eq (Name, "__ghdl_ELABORATE") then
            Put_Err ("Elaboration of design");
            return;
         end if;
      end if;

      --  Find last suffix (as it indicates processes and elaborator).
      Last_Part := 0;
      for I in Name'Range loop
         exit when Name (I) = NUL;
         if Name (I) = '_' and then Name (I + 1) = '_' then
            Last_Part := I;
         end if;
      end loop;

      if Last_Part /= 0 then
         Suffix := To_Ghdl_C_String (Name (Last_Part)'Address);
         if Is_Eq (Suffix, "__ELAB") then
            Put_Err ("elaboration of ");
         elsif Is_Eq (Suffix, "__PROC") then
            Put_Err ("process ");
         else
            Last_Part := 0;
         end if;
      end if;
      Off := 1;
      Is_Arch := False;
      loop
         exit when Off = Last_Part;
         C := Name (Off);
         Off := Off + 1;
         exit when C = NUL;
         if C = '_' and then Name (Off) = '_' then
            if Name (Off + 1) = 'A'
              and then Name (Off + 2) = 'R'
              and then Name (Off + 3) = 'C'
              and then Name (Off + 4) = 'H'
              and then Name (Off + 5) = '_'
              and then Name (Off + 6) = '_'
            then
               Off := Off + 7;
               Put_Err ('(');
               Is_Arch := True;
            else
               if Is_Arch then
                  Put_Err (')');
                  Is_Arch := False;
               end if;
               Put_Err ('.');
               Off := Off + 1;
            end if;
         else
            Put_Err (C);
         end if;
      end loop;
      if Is_Arch then
         Put_Err (')');
      end if;
   end Demangle_Err;

   procedure Put_Err_Backtrace (Bt : Backtrace_Addrs)
   is
      use System;

      Filename : Address;
      Lineno : Natural;
      Subprg : Address;
      Unknown : Boolean;
   begin
      if Bt.Size = 0
        or else Bt.Skip >= Bt.Size
        or else Symbolizer = null
      then
         --  No backtrace or no symbolizer.
         return;
      end if;

      Unknown := False;
      for I in Bt.Skip .. Bt.Size loop
         Symbolizer.all (To_Address (Bt.Addrs (I)),
                         Filename, Lineno, Subprg);
         if Subprg = Null_Address
           and (Filename = Null_Address or Lineno = 0)
         then
            Unknown := True;
         else
            if Unknown then
               Put_Err ("  from: [unknown caller]");
               Unknown := False;
            end if;
            Put_Err ("  from:");
            if Flag_Address then
               Put_Err (" 0x");
               Put_Err (Address_Image (Bt.Addrs (I)));
            end if;
            if Subprg /= Null_Address then
               Put_Err (' ');
               Demangle_Err (To_Ghdl_C_String (Subprg));
            end if;
            if Filename /= Null_Address and Lineno /= 0 then
               Put_Err (" at ");
               Put_Err (File_Basename (To_Ghdl_C_String (Filename)));
               Put_Err (":");
               Put_Err (Lineno);
            end if;
            Newline_Err;
         end if;
      end loop;
   end Put_Err_Backtrace;

   --  Return TRUE if OPT is an option for backtrace.
   function Backtrace_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
   begin
      if Opt'Length < 10 or else Opt (F .. F + 10) /= "--backtrace" then
         return False;
      end if;
      if Opt'Length = 16 and then Opt (F + 11 .. F + 15) = "-addr" then
         Flag_Address := True;
         return True;
      end if;
      return False;
   end Backtrace_Option;

   Backtrace_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("backtrace: print backtrace on errors"),
      Option => Backtrace_Option'Access,
      Help => null,
      Init => null,
      Start => null,
      Finish => null);

   procedure Register is
   begin
      Register_Hooks (Backtrace_Hooks'Access);
   end Register;

end Grt.Backtraces;
