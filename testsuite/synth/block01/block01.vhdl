library ieee;
use ieee.std_logic_1164.all;

entity block01 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end block01;

architecture behav of block01 is
begin
  b1 : block
  begin
    process (clk) is
    begin
      if rising_edge (clk) then
        q <= d;
      end if;
    end process;
  end block b1;
end behav;
