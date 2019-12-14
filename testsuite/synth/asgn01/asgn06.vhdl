library ieee;
use ieee.std_logic_1164.all;

entity asgn06 is
  port (clk : std_logic;
        s0 : std_logic;
        r : out std_logic_vector (65 downto 0));
end asgn06;

architecture behav of asgn06 is
begin
  process (clk) is
  begin
    if rising_edge(clk) then
       if s0 = '1' then
         r (0) <= '0';
         r (8 downto 5) <= x"9";
         r (65) <= '0';
       else
         r (0) <= '1';
         r (64 downto 1) <= x"ffff_eeee_dddd_cccc";
         r (65) <= '1';
       end if;
    end if;
  end process;
end behav;
