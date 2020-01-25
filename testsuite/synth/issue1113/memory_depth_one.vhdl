library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memory_depth_one is
  port (
    address: in unsigned(0 downto 0);
    output: out std_logic
    );
end entity;

architecture arch of memory_depth_one is
  constant store: std_logic_vector(0 downto 0) := "0";
begin
  output <= store(to_integer(address));
end arch;
