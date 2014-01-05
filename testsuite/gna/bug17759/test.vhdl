
library ieee;
use ieee.std_logic_1164.all;

package test is
  type array_t is array (1 downto 0) of std_logic_vector(1 downto 0);
end package test;


use work.test.all;
library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (io_a : inout array_t);
end entity b;

architecture rtl of b is
begin  -- architecture dummy
  io_a <= ("HH", "LL");
end architecture rtl;


use work.test.all;
library ieee;
use ieee.std_logic_1164.all;

entity a is
    port (io_v : inout std_logic_vector(1 downto 0);
          io_1 : inout std_logic;
          io_2 : inout std_logic);
end entity a;

architecture rtl of a is
  component b is
    port (io_a : inout array_t);
  end component b;
begin  -- architecture rtl
  b1 : b port map (
    io_a(0) => io_v,
    io_a(1)(0) => io_1,
    io_a(1)(1) => io_2);
end architecture rtl;
