entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is

begin  -- only
  process
    variable x : std_logic;
  begin  -- process
    assert std_logic'pos('U') = 0 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('X') = 1 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('0') = 2 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('1') = 3 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('Z') = 4 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('W') = 5 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('L') = 6 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('H') = 7 report "TEST FAILED" severity FAILURE;
    assert std_logic'pos('-') = 8 report "TEST FAILED" severity FAILURE;

    assert x'left = 'U' report "TEST FAILED" severity FAILURE;
    assert x'right = '-' report "TEST FAILED" severity FAILURE;
    assert x'high = '-' report "TEST FAILED" severity FAILURE;
    assert x'low = 'U' report "TEST FAILED" severity FAILURE;
    assert x'ascending = true report "TEST FAILED" severity FAILURE;

    assert std_logic'image('U') = "'U'" report "TEST FAILED" severity FAILURE;
    assert std_logic'value("'U'") = 'U' report "TEST FAILED" severity FAILURE;

    assert std_logic'val(0) = 'U' report "TEST FAILED" severity FAILURE;

    assert std_logic'succ('U') = 'X' report "TEST FAILED" severity FAILURE;
    assert std_logic'pred('-') = 'H' report "TEST FAILED" severity FAILURE;

    assert std_logic'leftof('-') = 'H' report "TEST FAILED" severity FAILURE;
    assert std_logic'rightof('U') = 'X' report "TEST FAILED" severity FAILURE;
    
    report "TEST PASSED" severity note;
    wait;
  end process;
end only;
