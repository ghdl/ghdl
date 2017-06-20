library ieee;
use ieee.std_logic_1164.all;

entity Dummy is
end entity;

architecture arch of Dummy is
  subtype t_null is std_logic_vector(-1 downto 0);
  type array_of_nulls is array(1 downto 0) of t_null;
begin
end architecture;
