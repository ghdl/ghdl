package gen is
  generic (type t);
end gen;

entity e is
end entity;

library ieee;
use     ieee.std_logic_1164.all;

architecture a of e is
  subtype T_DATA		is std_logic_vector(31 downto 0);
  type T_DATA_VECTOR		is array(natural range <>) of T_DATA;

  package pkg is new work.gen (t => t_data_vector);
begin
end architecture;
