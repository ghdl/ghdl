package vc_fakeram_pkg is 
  type memory_type is array (0 to 4294967296) of integer; 
  shared variable memory:memory_type; 
end vc_fakeram_pkg; 
