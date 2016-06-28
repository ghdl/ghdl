package pkg is
  generic ( gen: natural );
  constant test: natural:=gen;
end package;

package body pkg is
end pkg;

package mygpkg is new work.pkg generic map ( gen => 17 );
