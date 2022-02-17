library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (output : out std_ulogic);
end entity;

architecture rtl of ent is
  signal sr : std_ulogic_vector(0 downto 1);
begin
  output <= sr(1);
end architecture;
