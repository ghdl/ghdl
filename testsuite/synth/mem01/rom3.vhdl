library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom3 is
  port (addr : std_logic_vector (0 downto 0);
        val : out std_logic);
end;

architecture behav of rom3 is
  constant myrom : std_logic_vector := x"5c";
begin
  process (addr)
    variable idx : integer range -1 to 1;
  begin
    idx := to_integer(unsigned(addr));
    val <= myrom (idx);
  end process;
end behav;
