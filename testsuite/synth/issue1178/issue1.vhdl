library ieee;
use ieee.std_logic_1164.all;

entity issue1 is
    port (foobar : in  std_logic_vector(3 downto 0);
          foo    : out std_logic_vector(1 downto 0);
          bar    : out std_logic_vector(1 downto 0));
end issue1;

architecture behav of issue1 is
begin
    (foo, bar) <= foobar;
end architecture;

