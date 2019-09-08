library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom1 is
  port (addr : std_logic_vector (3 downto 0);
        val : out std_logic_vector (7 downto 0));
end rom1;

architecture behav of rom1 is
  type t_myrom is array (0 to 15) of std_logic_vector (7 downto 0);
  constant myrom : t_myrom :=
    (x"00", x"01", x"02", x"03",
     x"40", x"41", x"42", x"43",
     x"f8", x"f9", x"fa", x"fb",
     x"fc", x"fd", x"fe", x"ff");
begin
  val <= myrom (to_integer(unsigned(addr)));
end behav;
