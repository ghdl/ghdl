package repro3_gpkg is
  generic (len : natural);
end;

package repro3_gpkgpkg is
  generic (package pkg is new work.repro3_gpkg generic map (<>));

   attribute attr      : string;
   attribute attr of pkg : package is "val";
end;
