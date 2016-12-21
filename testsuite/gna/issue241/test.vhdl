LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

PACKAGE test_ctrl_parameter_pkg IS

  TYPE device_t IS (
    DEVICE_NONAME,                     
    DEVICE_TEST
    );

-----------------------------------------------------------------------------
-- DDR SDRAM device definitions
-----------------------------------------------------------------------------

  TYPE mem_device_t IS (
    TEST_1,
    TEST_2,
    TEST_3
    );

  TYPE mem_device_array_t IS ARRAY (natural RANGE <>) OF mem_device_t;

  CONSTANT C_TEST_BUS_ARRAY_SIZE : natural := 10;

  SUBTYPE test_device_type_t IS
    mem_device_array_t(0 TO C_TEST_BUS_ARRAY_SIZE - 1);

  CONSTANT DEFAULT_TEST_DEVICE_TYPE_ARRAY : test_device_type_t :=
    (OTHERS => TEST_1);

  TYPE device_test_parameters_t IS RECORD
    test_devices  : test_device_type_t;
  END RECORD;

  CONSTANT DEFAULT_DEVICE_TEST_PARAMETERS :
    device_test_parameters_t := (
      test_devices  => DEFAULT_TEST_DEVICE_TYPE_ARRAY);

  TYPE device_test_parameters_array_t IS ARRAY (natural RANGE <>) OF
    device_test_parameters_t;

  SUBTYPE constrained_device_test_parameters_array_t IS
    device_test_parameters_array_t(0 TO device_t'pos(device_t'high));

  CONSTANT DEVICE_TEST_PARAMETERS :
    constrained_device_test_parameters_array_t := (
      device_t'pos(DEVICE_TEST) =>
      (
        test_devices  => (1 | 2 => TEST_1, 3 => TEST_2, OTHERS => TEST_3)
        ),
      OTHERS          => DEFAULT_DEVICE_TEST_PARAMETERS
      );

END test_ctrl_parameter_pkg;


ENTITY test IS
END ENTITY test;

ARCHITECTURE rtl OF test IS

BEGIN

END ARCHITECTURE rtl;
