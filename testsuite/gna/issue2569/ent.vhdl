LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE work.pkg.ALL;

ENTITY ent_with_record_input IS
    PORT (
        a : IN test_record
    );
END ENTITY ent_with_record_input;

ARCHITECTURE arch OF ent_with_record_input IS
BEGIN
END ARCHITECTURE arch;
