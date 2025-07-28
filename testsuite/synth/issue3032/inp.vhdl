library ieee;
use ieee.std_logic_1164.all;

entity inp is
  port (
    clk    : in std_logic;
    d   : in std_logic;
    q   : out std_logic);
end;

architecture synth of inp is
  attribute dont_touch        : string;
  attribute dont_touch of d: signal is "true";
begin
  process (clk)
  begin
    if rising_edge(clk) then
      q <= d;
    end if;
  end process;
end architecture;
