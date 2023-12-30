LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.pkg.ALL;

ENTITY ent_use_constant IS
END ENTITY ent_use_constant;

ARCHITECTURE arch OF ent_use_constant IS
    CONSTANT test : test_record := (test => '0');
BEGIN
    bug : ENTITY work.ent_with_record_input
        PORT MAP(
            a => test
        );
END ARCHITECTURE arch;
