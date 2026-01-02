library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom01 is
  port (clk : std_logic;
        addr : std_logic_vector (2 downto 0);
        val : out std_logic_vector (3 downto 0));
end rom01;

architecture behav of rom01 is
  type t_myram is array (0 to 7) of std_logic_vector (3 downto 0);
begin
  process (clk)
    variable myram : t_myram :=
      ("ZZZZ", x"1", x"2", x"3", x"8", x"9", x"a", x"b");
  begin
    if rising_edge (clk) then
      val <= myram (to_integer(unsigned(addr)));
    end if;
  end process;
end behav;
