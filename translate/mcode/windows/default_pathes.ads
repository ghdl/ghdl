with Windows_Default_Path;
pragma Elaborate_All (Windows_Default_Path);

package Default_Pathes is
   Prefix : constant String := Windows_Default_Path.Get_Windows_Default_Path;
end Default_Pathes;
