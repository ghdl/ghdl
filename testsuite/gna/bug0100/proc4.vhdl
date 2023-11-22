library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
    a : in std_logic;
    b : out std_logic_vector(3 downto 0)
);
end entity;

architecture rtl of test is
  constant c : natural := 4;
  constant vec : std_logic_vector(7 downto 0) := x"ab";
  signal s : std_logic;
begin
  process(a)
  begin
    if c < 4 then
      s
    end if;
  end process;
end architecture;
