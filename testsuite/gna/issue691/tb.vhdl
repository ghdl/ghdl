library ieee, std;
use ieee.std_logic_1164.all;

entity e1 is port(number: in std_logic_vector(15 downto 0)); end e1;

library ieee;
use ieee.std_logic_1164.all;

entity tb is end entity;
architecture arch of tb is begin
    DS: entity work.e1 port map (number => std_logic_vector(1, 15));
end arch;
