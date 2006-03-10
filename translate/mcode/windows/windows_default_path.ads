package Windows_Default_Path is
   --  Get the default path from registry.
   --  This function is called during elaboration!
   function Get_Windows_Default_Path return String;
end Windows_Default_Path;
