library ieee;
use ieee.std_logic_1164.all;

entity dff2 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic;
        en1, en2 : std_logic);
end dff2;

architecture behav of dff2 is
  signal ff : std_logic := '0';
begin
  process (clk, en1, en2) is
  begin
    if en1 = '1' and (rising_edge (clk) and en2 = '1') then
      ff <= d;
    end if;
  end process;
  q <= ff;
end behav;
