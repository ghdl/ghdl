library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue93 is
    port (foo    : out std_logic;
          bar    : out std_logic);
end ;

architecture beh of issue93 is
begin
    (foo, bar) <= "10" + "01"; -- crashes
end architecture;
