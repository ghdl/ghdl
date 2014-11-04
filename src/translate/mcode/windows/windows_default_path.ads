package Windows_Default_Path is
   --  Get the default path from executable name.
   --  This function is called during elaboration!
   function Get_Windows_Exec_Path return String;
end Windows_Default_Path;
