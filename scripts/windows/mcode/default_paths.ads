with Windows_Default_Path;
pragma Elaborate_All (Windows_Default_Path);

package Default_Paths is
   Install_Prefix : constant String :=
     Windows_Default_Path.Get_Windows_Exec_Path;
   Lib_Prefix : constant String := "lib";
   Shared_Library_Extension : constant String := ".dll";
end Default_Paths;
