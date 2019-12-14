library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memmux05 is
  port (ad : std_logic;
        val : std_logic_vector (1 downto 0);
        dat : std_logic_vector (2 downto 0);
        res : out std_logic_vector (2 downto 0));
end memmux05;

architecture behav of memmux05 is
begin
  process (ad, val)
    variable lo : natural;
    variable t : std_logic_vector(2 downto 0);
  begin
    lo := to_integer(unsigned'(0 => ad));
    t := dat;
    t (lo + 1 downto lo) := val;
    res <= t;
  end process;
end behav;

--  Expected synthesis:
--  v15 := ad == 0 ? val : dat[1:0]
--  res[2:1] := ad == 1 ? val : {dat[2], v15[1]}
--  res[0] := v15[0]
