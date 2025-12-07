library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice11 is
  port (dat : std_logic_vector (7 downto 0);
        addr : std_logic_vector (1 downto 0);
        res : out std_logic_vector (1 downto 0));
end;

architecture behav of slice11 is
  function extr(vec : std_logic_vector; lo, hi: natural) return std_logic_vector
  is
  begin
    return vec(lo to hi);
  end extr;
begin
  process(addr, dat)
    variable lo : integer;
  begin
    lo := to_integer(unsigned(addr));
    res <= extr(dat, lo, lo + 1);
  end process;
end behav;
