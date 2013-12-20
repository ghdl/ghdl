entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

package foo is
  TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;
    CONSTANT and_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
            ( 'U', 'U', '0', 'U', 'U', 'U', '0', 'U', 'U' ),  -- | U |
            ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | X |
            ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ),  -- | 0 |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 1 |
            ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | Z |
            ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | W |
            ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ),  -- | L |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | H |
            ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' )   -- | - |
    );
end foo;

use work.foo.all;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is

begin  -- only
  process
  begin  -- process
    assert and_table( 'U', 'U' ) = 'U' report "TEST FAILED-UxU";
    assert and_table( 'U', 'X' ) = 'U' report "TEST FAILED-UxX";
    assert and_table( 'X', '-' ) = 'X' report "TEST FAILED-Xx-";
    assert and_table( '0', '1' ) = '0' report "TEST FAILED-0x1";
    assert and_table( 'H', 'Z' ) = 'X' report "TEST FAILED-HxZ";
    assert and_table( 'Z', 'W' ) = 'X' report "TEST FAILED-ZxW";
    assert and_table( 'L', '1' ) = '0' report "TEST FAILED-Lx1";
    assert and_table( 'H', '1' ) = '1' report "TEST FAILED-Hx1";
    assert and_table( '0', 'L' ) = '0' report "TEST FAILED-0xL";
    assert and_table( 'Z', 'L' ) = '0' report "TEST FAILED-ZxL";
    assert and_table( 'Z', 'H' ) = 'X' report "TEST FAILED-ZxH";
    wait;
  end process;
end only;
