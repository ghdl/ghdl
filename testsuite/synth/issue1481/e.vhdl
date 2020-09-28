library ieee;
use ieee.std_logic_1164.all;

entity e is
    port (i : in  std_logic_vector(3 to 0);
          o : out std_logic);
end entity;

architecture a of e is
begin
    o <= xor(i);
end architecture;
