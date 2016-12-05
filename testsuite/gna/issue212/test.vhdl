PACKAGE test_pkg IS

  TYPE test_record_t IS RECORD
    number : integer;
  END RECORD test_record_t;

  FUNCTION set_test_record_default
    RETURN test_record_t;

  FUNCTION set_test_record (
    CONSTANT C_TEST : test_record_t := set_test_record_default)
    RETURN test_record_t;

END PACKAGE test_pkg;

PACKAGE BODY test_pkg IS

  FUNCTION set_test_record_default
    RETURN test_record_t IS
    VARIABLE result : test_record_t;
  BEGIN
    result.number := 0;
    RETURN result;
  END set_test_record_default;

  FUNCTION set_test_record (
    CONSTANT C_TEST : test_record_t := set_test_record_default)
    RETURN test_record_t IS
  BEGIN
    RETURN C_TEST;
  END set_test_record;

END PACKAGE BODY test_pkg;

ENTITY test IS
END ENTITY test;

LIBRARY work;
USE work.test_pkg.set_test_record;

ARCHITECTURE rtl OF test IS

BEGIN

END ARCHITECTURE rtl;
