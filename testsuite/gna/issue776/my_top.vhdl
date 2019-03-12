library ieee;
use ieee.std_logic_1164.all;

entity my_top is end entity;

architecture my_top_impl of my_top is
begin
        HA_Entity_instance: entity work.HA_Entity port map('0', '0');
end architecture;
