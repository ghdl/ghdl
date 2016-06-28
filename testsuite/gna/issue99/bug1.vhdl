package bug1 is
  generic ( gen: natural );
  constant test: natural:=gen;
end package;

package mygbug1 is new work.bug1 generic map ( gen => 17 );

package body bug1 is
end ;

