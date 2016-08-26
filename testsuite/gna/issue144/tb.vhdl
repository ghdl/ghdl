library ieee;
use ieee.std_logic_1164.all;

entity A_tb is
end entity;

architecture A_tb_impl of A_tb is
begin
  a : work.a(a) port map(a => a);  -- syntax error here, missing 'entity'
end architecture;
