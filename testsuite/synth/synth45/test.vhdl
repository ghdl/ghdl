library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is

  type t_register is array(0 to 7) of std_logic_vector(7 downto 0);
  signal s_register : t_register;

begin

  -- the problem is the next line
  s_register  <= (others => (others => '0'));

end architecture beh;
