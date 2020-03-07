library ieee;
use ieee.std_logic_1164.all;

entity output07 is
  port (clk : std_logic;
        i : std_logic;
        o : out std_logic_vector (1 downto 0) := "10");
end output07;

architecture behav of output07 is
begin
  process (clk)
  begin
    if rising_edge(clk) then
      o (0) <= i;
      o (1) <= not i;
    end if;
  end process;
end behav;
