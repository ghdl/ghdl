with GNAT.Registry; use GNAT.Registry;

package body Windows_Default_Path is
   function Get_Windows_Default_Path return String
   is
      Key : HKEY;
   begin
      Key := Open_Key (HKEY_LOCAL_MACHINE, "Software\Ghdl");
      declare
	 Res : String := Query_Value (Key, "Install_Dir");
      begin
	 return Res & "\lib\";
      end;
   exception
      when Registry_Error =>
	 --  Do not write an error message, but return a useful default path.
	 return "{missing HKLM\Software\Ghdl\Install_Dir key}\lib\";
   end Get_Windows_Default_Path;
end Windows_Default_Path;
