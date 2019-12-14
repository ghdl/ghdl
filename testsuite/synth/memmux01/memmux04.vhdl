library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memmux04 is
  port (ad : std_logic_vector (1 downto 0);
        val : std_logic;
        dat : std_logic_vector (3 downto 0);
        res : out std_logic_vector (3 downto 0));
end memmux04;

architecture behav of memmux04 is
begin
  process (ad, val)
    variable hi, lo : natural;
    variable t : std_logic_vector(3 downto 0);
  begin
    lo := to_integer(unsigned(ad));
    t := dat;
    t (lo) := val;
    res <= t;
  end process;
end behav;
