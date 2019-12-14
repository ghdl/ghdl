library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmp02 is
  port (l : std_logic_vector(3 downto 0);
        r :  natural;
        eq : out std_logic;
        ne : out std_logic;
        lt : out std_logic;
        le : out std_logic;
        ge : out std_logic;
        gt : out std_logic);
end cmp02;

architecture behav of cmp02 is
begin
  eq <= '1' when unsigned(l) =  r else '0';
  ne <= '1' when unsigned(l) /= r else '0';
  lt <= '1' when unsigned(l) <  r else '0';
  le <= '1' when unsigned(l) <= r else '0';
  gt <= '1' when unsigned(l) >  r else '0';
  ge <= '1' when unsigned(l) >= r else '0';
end behav;
