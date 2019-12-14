library ieee;
use ieee.std_logic_1164.all;

entity asgn08 is
  port (clk : std_logic;
        ce : std_logic;
        s0 : std_logic;
        r : out std_logic_vector (65 downto 0));
end asgn08;

architecture behav of asgn08 is
begin
  r (0) <= '1';

  process (clk) is
  begin
    if rising_edge(clk) and ce = '1' then
      if s0 = '1' then
        r (64 downto 1) <= x"ffff_eeee_dddd_cccc";
        r (65) <= '1';
      else
        r (8 downto 5) <= x"7";
        r (65) <= '0';
      end if;
    end if;
  end process;
end behav;
