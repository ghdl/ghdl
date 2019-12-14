library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
  port (val : out std_logic_vector (63 downto 0));
end entity test2;

architecture beh of test2 is

  type t_register is array(0 to 7) of std_logic_vector(7 downto 0);
  signal s_register : t_register;

begin

  -- the problem is the next line
  s_register  <= (0 => x"f0", 1 => x"e1", 2 => x"d2", 3 => x"c3",
                  4 => x"b4", 5 => x"a5", 6 => x"96", 7 => x"87");
  val <= s_register(7) & s_register(6) & s_register(5) & s_register(4)
         & s_register(3) & s_register(2) & s_register(1) & s_register(0);

end architecture beh;
