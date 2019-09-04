library ieee;
use ieee.std_logic_1164.all;

package rec03_pkg is
  type myenum is (s0, s1, s2, s3);

  type myrec is record
     a : myenum;
     b : std_logic;
  end record;
end rec03_pkg;
