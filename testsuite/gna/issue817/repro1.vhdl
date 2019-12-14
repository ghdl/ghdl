library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture arch of ent is
begin
   process
       variable valid_tmp : std_logic_vector := (others => '0'); 
   begin
       valid_tmp := (others => '0'); 
       wait;
   end process;
end architecture;
