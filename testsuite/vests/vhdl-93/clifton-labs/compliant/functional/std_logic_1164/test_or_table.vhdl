entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

package foo is
  TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;
    -- truth table for "or" function
    CONSTANT or_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
            ( 'U', 'U', 'U', '1', 'U', 'U', 'U', '1', 'U' ),  -- | U |
            ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | X |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
            ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ),  -- | 1 |
            ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | Z |
            ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | W |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
            ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ),  -- | H |
            ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' )   -- | - |
    );
end foo;

use work.foo.all;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is

begin  -- only
  process
  begin  -- process
    assert or_table( 'U', 'U' ) = 'U' report "TEST FAILED-UxU";
    assert or_table( 'U', 'X' ) = 'U' report "TEST FAILED-UxX";
    assert or_table( 'X', '-' ) = 'X' report "TEST FAILED-Xx-";
    assert or_table( '0', '1' ) = '1' report "TEST FAILED-0x1";
    assert or_table( 'H', 'Z' ) = '1' report "TEST FAILED-HxZ";
    assert or_table( 'Z', 'W' ) = 'X' report "TEST FAILED-ZxW";
    assert or_table( 'L', '1' ) = '1' report "TEST FAILED-Lx1";
    assert or_table( 'H', '1' ) = '1' report "TEST FAILED-Hx1";
    assert or_table( '0', 'L' ) = '0' report "TEST FAILED-0xL";
    assert or_table( 'Z', 'L' ) = 'X' report "TEST FAILED-ZxL";
    assert or_table( 'Z', 'H' ) = '1' report "TEST FAILED-ZxH";
    wait;
  end process;
end only;
