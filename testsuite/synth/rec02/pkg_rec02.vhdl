library ieee;
use ieee.std_logic_1164.all;

package rec02_pkg is
  type inner_t is record
    a : std_logic;
    b : std_logic;
  end record;

  type outer_t is record
    i : inner_t;
    z : std_logic;
  end record;
end rec02_pkg;
