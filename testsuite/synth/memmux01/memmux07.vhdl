library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memmux07 is
  port (ad : std_logic;
        val : std_logic_vector (1 downto 0);
        dat : std_logic_vector (7 downto 0);
        res : out std_logic_vector (7 downto 0));
end memmux07;

architecture behav of memmux07 is
begin
  process (ad, val, dat)
    variable lo : natural;
    variable t : std_logic_vector(7 downto 0);
  begin
    lo := to_integer(unsigned'(0 => ad));
    t := dat;
    t (4 * lo + 1 downto 4 * lo) := val;
    res <= t;
  end process;
end behav;
