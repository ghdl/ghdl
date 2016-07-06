package bug2 is
  generic ( gen: natural );
  constant test: natural:=gen;

  function get_val return natural;
end package;

package mygbug2 is new work.bug2 generic map ( gen => 17 );
