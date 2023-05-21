--  Filesystem and OS interface.
--  Copyright (C) 2023 Tristan Gingold
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

with Ada.Calendar;
with Ada.Calendar.Time_Zones;

with GNAT.Directory_Operations;

package body Filesystem is
   function ">" (L, R : OS_Time_T) return Boolean
   is
      use GNAT.OS_Lib;
   begin
      return OS_Time (L) > OS_Time (R);
   end ">";

   function Get_Current_Directory return String is
   begin
      return GNAT.Directory_Operations.Get_Current_Dir;
   end Get_Current_Directory;

   function Get_Directory_Separator return Character is
   begin
      return GNAT.OS_Lib.Directory_Separator;
   end Get_Directory_Separator;

   function Is_Absolute_Path (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Absolute_Path (Filename);
   end Is_Absolute_Path;

   function Is_Regular_File (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Regular_File (Filename);
   end Is_Regular_File;

   function Is_Executable_File (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Executable_File (Filename);
   end Is_Executable_File;

   function Is_Directory (Filename : String) return Boolean is
   begin
      return GNAT.OS_Lib.Is_Directory (Filename);
   end Is_Directory;

   procedure Delete_File (Filename : String; Success : out Boolean) is
   begin
      GNAT.OS_Lib.Delete_File (Filename, Success);
   end Delete_File;

   procedure Rename_File (Old_Filename : String;
                          New_Filename : String;
                          Success : out Boolean) is
   begin
      GNAT.OS_Lib.Rename_File (Old_Filename, New_Filename, Success);
   end Rename_File;

   function Get_File_Modification_Time (Filename : String) return OS_Time_T is
   begin
      return File_Time_Stamp (Filename);
   end Get_File_Modification_Time;

   procedure Split_Now_Utc (Year : out Year_Range;
                            Month : out Month_Range;
                            Day : out Day_Range;
                            Sec : out Sec_Range;
                            Ms : out Ms_Range)
   is
      use Ada.Calendar;
      use Ada.Calendar.Time_Zones;

      Now : constant Time := Clock;
      Now_UTC : constant Time := Now - Duration (UTC_Time_Offset (Now) * 60);
      Sec1 : Day_Duration;
      S : Integer;
      M : Integer;
   begin
      --  Use UTC time (like file time stamp).
      Split (Now_UTC, Year, Month, Day, Sec1);

      S := Integer (Sec1);
      if Day_Duration (S) > Sec1 then
         --  We need a truncation.
         Sec := S - 1;
      else
         Sec := S;
      end if;

      Sec1 := Sec1 - Day_Duration (Sec);
      M := Integer (Sec1 * 1000);
      if M = 1000 then
         --  We need truncation.
         Ms := 999;
      else
         Ms := M;
      end if;
   end Split_Now_Utc;

   procedure Open_Read (Fd : out File_Descriptor; Filename : String)
   is
      use GNAT.OS_Lib;
      Filename0 : constant String := Filename & Ascii.NUL;
   begin
      Fd.Fd := GNAT.OS_Lib.Open_Read (Filename0'Address, GNAT.OS_Lib.Binary);
      Fd.Error := Fd.Fd = Invalid_FD;
   end Open_Read;

   procedure Close (Fd : in out File_Descriptor) is
   begin
      GNAT.OS_Lib.Close (Fd.Fd);
      Fd.Fd := GNAT.OS_Lib.Invalid_FD;
   end Close;

   procedure Open_Write (Fd : out File_Descriptor; Filename : String)
   is
      use GNAT.OS_Lib;
      Filename0 : constant String := Filename & Ascii.NUL;
   begin
      Fd.Fd := GNAT.OS_Lib.Create_File (Filename0'Address, GNAT.OS_Lib.Binary);
      Fd.Error := Fd.Fd = Invalid_FD;
   end Open_Write;

   function File_Length (Fd : File_Descriptor) return Long_Integer is
   begin
      return GNAT.OS_Lib.File_Length (Fd.Fd);
   end File_Length;

   function Is_Error (Fd : File_Descriptor) return Boolean is
   begin
      return Fd.Error;
   end Is_Error;

   procedure Read (Fd : in out File_Descriptor;
                   Buffer : System.Address;
                   Length : Long_Integer)
   is
      Len : constant Natural := Natural (Length);
      Res : Integer;
   begin
      Res := GNAT.OS_Lib.Read (Fd.Fd, Buffer, Len);
      if Res /= Len then
         Fd.Error := True;
      end if;
   end Read;

   procedure Write (Fd : in out File_Descriptor;
                    Buffer : System.Address;
                    Length : Long_Integer)
   is
      Len : constant Natural := Natural (Length);
      Res : Integer;
   begin
      Res := GNAT.OS_Lib.Write (Fd.Fd, Buffer, Len);
      if Res /= Len then
         Fd.Error := True;
      end if;
   end Write;

   function Spawn (Command : String; Args : String_Acc_Array) return Integer
   is
      Nargs : GNAT.OS_Lib.Argument_List (1 .. Args'Length);
   begin
      for I in Nargs'Range loop
         Nargs (I) := GNAT.OS_Lib.String_Access (Args (Args'First + I - 1));
      end loop;
      return GNAT.OS_Lib.Spawn (Command, Nargs);
   end Spawn;

   function Strlen (S : Thin_String_Ptr) return Natural;
   pragma Import (C, Strlen);

   function Getenv (Name : String) return String_Acc
   is
      function C_Getenv (Name : Thin_String_Ptr) return Thin_String_Ptr;
      pragma Import (C, C_Getenv, "getenv");

      C_Name : constant String := Name & ASCII.NUL;
      C_Val : Thin_String_Ptr;
      C_Len : Natural;
   begin
      C_Val := C_Getenv (To_Thin_String_Ptr (C_Name'Address));
      if C_Val = null then
         return null;
      end if;
      C_Len := Strlen (C_Val);
      return new String'(C_Val (1 .. C_Len));
   end Getenv;

   function Locate_Executable_On_Path (Command : String) return String_Acc is
   begin
      if True then
         declare
            use GNAT.OS_Lib;
            Tmp : String_Access;
            Res : String_Acc;
         begin
            Tmp := Locate_Exec_On_Path (Command);
            if Tmp = null then
               return null;
            end if;
            Res := new String'(Tmp.all);
            Free (Tmp);
            return Res;
         end;
      else
         declare
            Sep : constant Character := GNAT.OS_Lib.Path_Separator;
            Path : String_Acc;
            F, P : Natural;
         begin
            Path := Getenv ("PATH");
            if Path = null then
               return null;
            end if;

            F := Path'First;
            loop
               --  Skip until path separator or end of PATH.
               P := F;
               while P <= Path'Last and then Path (P) /= Sep loop
                  P := P + 1;
               end loop;

               if P = F then
                  --  Empty path, so look at the current directory.
                  if GNAT.OS_Lib.Is_Executable_File (Command) then
                     Free (Path);
                     return new String'(Command);
                  end if;
               else
                  declare
                     C_Full_Path : String
                       (1 .. (P - F) + 1 + Command'Length + 1);
                  begin
                     C_Full_Path (1 .. P - F) := Path (F .. P - 1);
                     C_Full_Path (P - F + 1) := Get_Directory_Separator;
                     C_Full_Path (P - F + 2 .. C_Full_Path'Last - 1) :=
                       Command;
                     C_Full_Path (C_Full_Path'Last) := ASCII.NUL;
                     if GNAT.OS_Lib.Is_Executable_File (C_Full_Path'Address)
                     then
                        Free (Path);
                        return new String'
                          (C_Full_Path (1 .. C_Full_Path'Last - 1));
                     end if;
                  end;
               end if;

               if P > Path'Last then
                  return null;
               end if;

               --  Skip path separator.
               F := P + 1;
            end loop;
         end;
      end if;
   end Locate_Executable_On_Path;
end Filesystem;
