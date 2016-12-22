LIBRARY ieee;
USE ieee.std_logic_1164.ALL;


ENTITY test IS
END ENTITY test;

ARCHITECTURE rtl OF test IS

  TYPE test_data_t IS ARRAY (0 TO 2) OF natural;

  TYPE test_vector_t IS ARRAY (0 TO 7) OF test_data_t;

  CONSTANT C_TEST_VECTOR : test_vector_t := (OTHERS => test_data_t'(0, 0, 0));

BEGIN

END ARCHITECTURE rtl;
