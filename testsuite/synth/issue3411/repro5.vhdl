library ieee;
use ieee.std_logic_1164.all;

entity top is
port
(
  Clock: in std_logic;
  nReset: in std_logic;

  stb : std_logic;
  d_dff : out std_logic;
  d_latch : out std_logic
);
end top;

architecture rtl of top is
begin
  readout: process(nReset, Clock)
  begin
    if nReset = '0' then
      d_latch <= '0';
    elsif rising_edge(Clock) then
      d_dff <= stb;
    end if;
  end process;
end architecture;
