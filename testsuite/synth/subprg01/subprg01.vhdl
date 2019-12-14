library ieee;
use ieee.std_logic_1164.all;

entity subprg01 is
  port (a : std_logic_vector (3 downto 0);
        na : out std_logic_vector (3 downto 0));
end subprg01;

architecture behav of subprg01 is
  procedure neg (v : inout std_logic_vector(3 downto 0)) is
  begin
    v := not v;
  end neg;

begin
  process(A)
    variable t : std_logic_vector(3 downto 0);
  begin
    t := a;
    neg (t);
    na <= t;
  end process;
end behav;
