library ieee;
use ieee.std_logic_1164.all;

entity issue is
    generic (constant N : integer := 3);
    port (foo : in  std_logic;
          bar : out std_logic_vector(7 downto 0));
end issue;

architecture beh of issue is
begin
    bar <= (N=>foo, others=>'0');
end architecture;
