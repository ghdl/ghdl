library ieee;
use ieee.std_logic_1164.all;

entity var01 is
  port (clk : std_logic;
        sel : std_logic_vector (3 downto 0);
        res : out std_logic_vector (7 downto 0));
end var01;

architecture behav of var01 is
begin
  process (clk)
    variable r : std_logic_vector(7 downto 0);
  begin
    if rising_edge (clk) then
      with sel select
        r := x"85" when x"2" | x"4",
        x"ab" when x"0",
        x"ef" when x"6",
        x"00" when others;
      res <= r;
    end if;
  end process;
end behav;
