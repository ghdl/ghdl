LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;

PACKAGE test_pkg IS

  PROCEDURE test (
    arg : IN std_ulogic_vector := std_ulogic_vector(conv_unsigned(-1, 8)));

END PACKAGE test_pkg;

PACKAGE BODY test_pkg IS

  PROCEDURE test (
    arg : IN std_ulogic_vector := std_ulogic_vector(conv_unsigned(-1, 8))) IS
  BEGIN
    
  END PROCEDURE test;

END PACKAGE BODY test_pkg;
