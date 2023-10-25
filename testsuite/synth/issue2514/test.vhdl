LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY test IS
    PORT (
        test_vec1 : IN std_logic_vector(2 DOWNTO 0);
        test_vec2 : IN std_logic_vector(2 DOWNTO 0);
        test_bit  : OUT std_logic
    );
END ENTITY;

ARCHITECTURE rtl OF test IS

BEGIN

    test_bit <= test_vec1 ?= test_vec2;

END ARCHITECTURE;
