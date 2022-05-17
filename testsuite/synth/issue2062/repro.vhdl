library ieee;
use ieee.std_logic_1164.all;

entity repro is port (
   a : in  std_logic_vector(5 downto 0);
   y : out std_ulogic_vector(3 downto -2));
end entity;

architecture beh of repro is
begin
  y <= to_stdulogicvector(a);
end beh;
