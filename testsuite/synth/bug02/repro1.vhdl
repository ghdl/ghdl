library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  port (clk : std_logic;
        o1 : out std_logic;
        o2 : out std_logic);
end repro1;

architecture behav of repro1 is
  signal v : natural range 0 to 0;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      o1 <= '1';
      v <= 0;
    end if;
  end process;

  o2 <= '1' when v = 0 else '0';
end behav;
