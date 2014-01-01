-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.all;
-------------------------------------------------------------------------------
--| c | a | b | s | c
--|---+---+---+---+--
--| 0 | 0 | 0 | 0 | 0
--| 0 | 0 | 1 | 1 | 0
--| 0 | 1 | 0 | 1 | 0
--| 0 | 1 | 1 | 0 | 1
--| 1 | 0 | 0 | 1 | 0
--| 1 | 0 | 1 | 0 | 1
--| 1 | 1 | 0 | 0 | 1
--| 1 | 1 | 1 | 1 | 1

ENTITY addern IS
    GENERIC ( n : INTEGER );
    PORT ( a, b : IN STD_LOGIC_VECTOR ( n-1 DOWNTO 0 );
            cin : IN STD_LOGIC;
            sum : OUT STD_LOGIC_VECTOR ( n DOWNTO 0 ) );
END addern;

ARCHITECTURE behave OF addern IS

    SIGNAL carry : STD_LOGIC;
BEGIN
    carry <= cin;
    suma : FOR i IN 0 TO n - 1 GENERATE
        sum(i) <= ( a(i) XOR b(i) ) XOR carry ;
        carry  <= ( a(i) AND b(i) ) OR (carry AND ( a(i) XOR b(i) ));
    END GENERATE;
    sum(n) <= carry;
END behave;
