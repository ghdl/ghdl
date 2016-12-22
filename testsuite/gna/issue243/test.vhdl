LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE test_pkg IS

  SUBTYPE test_t IS std_ulogic_vector(7 DOWNTO 0);

  TYPE test_array_t IS ARRAY (natural RANGE <>) OF test_t;

END PACKAGE test_pkg;

LIBRARY work;
USE work.test_pkg.ALL;

ENTITY test IS
  PORT (
    a : IN test_array_t(0 TO 4) := (OTHERS => (OTHERS => '0'));
    b : IN test_array_t(0 TO 4) := ((OTHERS => (OTHERS => '0'))));
END ENTITY test;

ARCHITECTURE rtl OF test IS

BEGIN

END ARCHITECTURE rtl;
