package repro2_gpkg is
  generic (len : natural);

  attribute attr1      : string;
  attribute attr1 of len : constant is "val";

  subtype my_nat is natural range 0 to len;
end;
