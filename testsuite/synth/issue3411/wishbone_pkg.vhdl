library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package wishbone_pkg is
  type wb_slave_i is record
    adr : std_logic_vector(2 downto 0);
    dat : std_logic_vector(7 downto 0);
    cyc : std_logic;
    stb : std_logic;
    we : std_logic;
  end record; 

  type wb_slave_o is record
    dat : std_logic_vector(7 downto 0);
    ack : std_logic;
    stall : std_logic;
  end record;
end;
