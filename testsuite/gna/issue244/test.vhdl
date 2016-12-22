entity test is
end entity test;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

architecture rtl of test is

  FUNCTION test
    RETURN std_ulogic_vector IS
    SUBTYPE vector_t IS std_ulogic_vector(0 TO 3);
  BEGIN
    RETURN vector_t'(OTHERS => '0');
  END test;

begin



end architecture rtl;
