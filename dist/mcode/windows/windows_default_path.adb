with Interfaces.C; use Interfaces.C;
with System; use System;

package body Windows_Default_Path is

   subtype DWORD is Interfaces.C.Unsigned_Long;
   subtype LPWSTR is String;
   subtype HINSTANCE is Address;
   function GetModuleFileName (Inst : HINSTANCE; Buf : Address; Size : DWORD)
                              return DWORD;
   pragma Import (Stdcall, GetModuleFileName, "GetModuleFileNameA");

   function Get_Windows_Exec_Path return String
   is
      File : String (1 .. 256);
      Size : DWORD;
      P : Natural;
   begin
      --  Get exe file path.
      Size := GetModuleFileName (Null_Address, File'Address, File'Length);
      if Size = 0 or Size = File'Length then
         return "{cannot find install path}\lib";
      end if;

      --  Remove Program file.
      P := Natural (Size);
      while P > 0 loop
         exit when File (P) = '\';
         exit when File (P) = ':' and P = 2;
         P := P - 1;
      end loop;
      if File (P) = '\' and P > 1 then
         --  Remove directory
         P := P - 1;
         while P > 0 loop
            exit when File (P) = '\';
            exit when File (P) = ':' and P = 2;
            P := P - 1;
         end loop;
      end if;

      return File (1 .. P);
   end Get_Windows_Exec_Path;
end Windows_Default_Path;

