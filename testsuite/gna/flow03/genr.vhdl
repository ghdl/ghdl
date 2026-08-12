library ieee;
use ieee.std_logic_1164.all;

entity cell is
  port (a : in std_logic; y : out std_logic);
end cell;

architecture rtl of cell is
begin
  y <= not a;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity arr is
  generic (N : positive := 4);
  port (din : in std_logic_vector (N-1 downto 0);
        dout : out std_logic_vector (N-1 downto 0));
end arr;

architecture rtl of arr is
begin
  g : for i in 0 to N-1 generate
    u : entity work.cell port map (a => din (i), y => dout (i));
  end generate;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity gtb is
end gtb;

architecture sim of gtb is
  signal di, do : std_logic_vector (2 downto 0);
begin
  dut : entity work.arr generic map (N => 3) port map (din => di, dout => do);
end sim;
