library ieee;
use ieee.std_logic_1164.all;

entity repro4 is
  generic (
    num : natural := 1);
  port (
    clk : std_logic;
    o : out std_logic);
end;

architecture behav of repro4 is
  signal s : natural range 0 to num - 1 := 0;
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if s = 0 then
        o <= '1';
      else
        o <= '0';
      end if;
      if s = num - 1 then
        s <= 0;
      else
        s <= s + 1;
      end if;
    end if;
  end process;
end behav;
