PACKAGE test_pkg IS

  TYPE string_array_t IS ARRAY (natural RANGE <>) OF string;

END test_pkg;

ENTITY test IS
END ENTITY test;

LIBRARY work;
USE work.test_pkg.string_array_t;

ARCHITECTURE rtl OF test IS
  

BEGIN

END ARCHITECTURE rtl;
