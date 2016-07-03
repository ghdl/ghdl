package pkg is
  generic ( gen: natural );
  constant test: natural:=gen;
end package;

package mygpkg is new work.pkg generic map ( gen => 17 );
