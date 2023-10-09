ENTITY test IS
END ENTITY;

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std_unsigned.ALL;

ARCHITECTURE rtl OF test IS

    SIGNAL test_vec : std_logic_vector(2 DOWNTO 0);
    SIGNAL test_bit : std_logic;

BEGIN

    test_bit <= '0';
    test_vec <= resize('0' & test_bit, test_vec'length);

END ARCHITECTURE;

