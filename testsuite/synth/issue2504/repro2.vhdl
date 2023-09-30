library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
  port (n : natural;
        d1 : std_logic_vector(15 downto 0);
        q : out std_logic_vector(31 downto 0));
end;

architecture behav of repro2 is
begin
  comb : process(d1, n) is
     variable h : unsigned(31 downto 0);
  begin
    h := resize(unsigned(d1), 32) and to_unsigned(3 ** n-1, 32);
    q <= std_logic_vector(h);
  end process;
end;
