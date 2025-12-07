library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice12 is
  port (dat : std_logic_vector (7 downto 0);
        addr : std_logic_vector (1 downto 0);
        res : out std_logic_vector (1 downto 0));
end;

architecture behav of slice12 is
begin
  process(addr, dat)
    variable lo : integer;
  begin
    lo := to_integer(unsigned(addr));
    res <= dat(lo downto 0);
  end process;
end behav;
