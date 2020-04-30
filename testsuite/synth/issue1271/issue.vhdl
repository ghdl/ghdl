library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port
      (i_foo : in  std_logic;
       o_foo : out std_logic);
end entity issue;

architecture beh of issue is
    constant k_foo  : std_logic := i_foo;
begin
    o_foo <= k_foo xor '0';
end architecture;
