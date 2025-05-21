library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memory is
  port (
    address: in unsigned(0 downto 0);
    output: out std_logic
    );
end entity;

architecture arch of memory is
  constant store: std_logic_vector(0 downto 0) := "0";
begin
  output <= store(to_integer(address));
end arch;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mytst is
  port (
    address: in unsigned(0 downto 0);
    res: out std_logic
    );
end entity;

architecture arch of mytst is
  signal r : std_logic;
begin
  dut: entity work.memory port map (address, r);
  res <= not r;
end arch;
