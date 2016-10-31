package gen2 is
  generic (package pkg is new work.gen1 generic map (<>));

  function get2 return natural;
end gen2;
