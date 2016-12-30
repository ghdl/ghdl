ENTITY test IS
END ENTITY test;

ARCHITECTURE rtl OF test IS

  TYPE test_type_t IS (
    TEST_1,
    TEST_2);

  TYPE test_type_value_t IS ARRAY (test_type_t) OF natural;

  CONSTANT C_TEST_TYPE_VALUE : test_type_value_t := (
    TEST_1 => 0,
    TEST_2 => 1);

  CONSTANT C_TEST_TYPE_TEST_1 : natural := C_TEST_TYPE_VALUE(TEST_1);
  CONSTANT C_TEST_TYPE_TEST_2 : natural := C_TEST_TYPE_VALUE(TEST_2);

  FUNCTION get_priority (
    arg : natural)
    RETURN natural IS
    VARIABLE result_v : natural;
  BEGIN
    CASE arg IS
      WHEN C_TEST_TYPE_TEST_1 =>
        result_v := 3;
      WHEN C_TEST_TYPE_TEST_2 =>
        result_v := 0;
      WHEN OTHERS =>
        REPORT "Unknown Sector Type"
          SEVERITY error;
        result_v := 4;
    END CASE;
    RETURN result_v;
  END FUNCTION get_priority;

BEGIN

END ARCHITECTURE rtl;
