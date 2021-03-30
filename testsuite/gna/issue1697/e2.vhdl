library ieee;
use ieee.std_logic_1164.all;

entity e is
    port (o : out std_logic_vector(2 downto 0));
end entity;

architecture a of e is
begin
    o <= ('1','0');
end architecture;
