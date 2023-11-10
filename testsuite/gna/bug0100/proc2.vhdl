library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
    a : in std_logic;
    b : out std_logic
);
end entity;

architecture rtl of test is
  constant c : natural := 4;
begin
  process(c)
  begin
  end process;
end architecture;
