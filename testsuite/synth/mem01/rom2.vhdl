library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom2 is
  port (addr : std_logic_vector (0 downto 0);
        val : out std_logic_vector (7 downto 0));
end rom2;

architecture behav of rom2 is
  type t_myrom is array (0 to 3) of std_logic_vector (7 downto 0);
  constant myrom : t_myrom :=
    (x"00", x"01", x"02", x"03");
begin
  process (addr)
    variable idx : natural range 0 to 1;
  begin
    idx := to_integer(unsigned(addr));
    val <= myrom (idx);
  end process;
end behav;
