library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity ent is
end entity;

architecture a of ent is

signal scale_product_twice_biased :  std_logic_vector(3 downto 0);
signal shift_value :  std_logic_vector(3 downto 0);

begin

   shift_value <= (scale_product_twice_biased) - (-1);

--  process begin
--    report "Hello world" severity note;
--    wait;
--  end process;
end;
