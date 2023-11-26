library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity A is
  generic (
    c : positive );
  port (
    clk : in std_logic;
    b : in integer range 0 to c - 1
    );
end A;

architecture rtl of A is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
  port (
    clk : in std_logic
    );
end top;

architecture str of top is
  constant d : positive := 8;
  signal e : unsigned(d - 1 downto 0);
begin
  A_INST : entity work.A(rtl)
    generic map (
      c => 2**d
      )
    port map (
      clk => clk,
      b => to_integer(e) -- does not work in synthesis
      );
end architecture;
