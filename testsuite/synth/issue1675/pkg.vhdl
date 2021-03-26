library ieee;
use ieee.std_logic_1164.all;

package pkg is
  type bus_rec_out_t is record
    dat : std_logic_vector(7 downto 0);
    stb : std_logic;
    rst : std_logic;
  end record;

  type bus_rec_in_t is record
    dat : std_logic_vector(7 downto 0);
    stb : std_logic;
  end record;
end pkg;

