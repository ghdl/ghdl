library ieee;
use ieee.std_logic_1164.all;

entity issue3 is
    port (foo  : in  std_logic_vector(32-1 downto 0);
          bar  : out std_logic);
end issue3;

architecture rtl of issue3 is
        alias a_bar is foo(foo'high);
begin
    bar <= a_bar;
end architecture;
