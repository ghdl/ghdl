library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port (foo  : in  std_logic_vector(32-1 downto 0);
          bar  : out std_logic);
end issue;

architecture rtl of issue is
        alias a_bar is foo(foo'high);
begin
    bar <= a_bar;
end architecture;
