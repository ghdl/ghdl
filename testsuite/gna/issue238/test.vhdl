LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY test IS
  TYPE foo_t IS RECORD
    bar : unsigned;
  END RECORD foo_t;
END ENTITY test;

ARCHITECTURE bar OF test IS
  SIGNAL baz : foo_t(bar(1 DOWNTO 0));
BEGIN

END ARCHITECTURE bar;
