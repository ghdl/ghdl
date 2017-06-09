library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity Bar is
  port (
    a : std_logic
  );
end entity Bar;

architecture RTL of Bar is

  signal s_test : std_logic_vector(3 downto 0) := "1111";

begin

end architecture;
