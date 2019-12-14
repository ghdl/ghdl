library ieee;
use ieee.std_logic_1164.all;

entity block02 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end block02;

architecture behav of block02 is
begin
  b1 : block
    signal s : std_logic;
  begin
    process (clk) is
    begin
      if rising_edge (clk) then
        s <= d;
      end if;
    end process;

    q <= s;
  end block b1;
end behav;
