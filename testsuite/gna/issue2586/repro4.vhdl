package repro4_gpkg is
  generic (len : natural);

  package repro4_sub is
  end package;


  attribute attr      : string;
  attribute attr of repro4_sub : package is "val";
end;
