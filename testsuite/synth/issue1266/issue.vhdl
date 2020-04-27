library ieee;
use ieee.std_logic_1164.all;

entity issue is
    generic (constant N : integer := 8);
    port (foo : in  std_logic;
          bar : out std_logic_vector(8-1 downto 0));
end issue;

architecture beh of issue is
begin
    bar <= (bar'high=>foo, others=>'0');
end architecture;
