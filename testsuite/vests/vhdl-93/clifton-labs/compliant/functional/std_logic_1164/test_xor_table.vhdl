entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

package foo is
  TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;
    -- truth table for "xor" function
    CONSTANT xor_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
            ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ),  -- | U |
            ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | X |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
            ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | 1 |
            ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | Z |
            ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | W |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
            ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | H |
            ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )   -- | - |
    );
end foo;

use work.foo.all;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is

begin  -- only
  process
  begin  -- process
    assert xor_table( 'U', 'U' ) = 'U' report "TEST FAILED-UxU";
    assert xor_table( 'U', 'X' ) = 'U' report "TEST FAILED-UxX";
    assert xor_table( 'X', '-' ) = 'X' report "TEST FAILED-Xx-";
    assert xor_table( '0', '1' ) = '1' report "TEST FAILED-0x1";
    assert xor_table( 'H', 'Z' ) = 'X' report "TEST FAILED-HxZ";
    assert xor_table( 'Z', 'W' ) = 'X' report "TEST FAILED-ZxW";
    assert xor_table( 'L', '1' ) = '1' report "TEST FAILED-Lx1";
    assert xor_table( 'H', '1' ) = '0' report "TEST FAILED-Hx1";
    assert xor_table( '0', 'L' ) = '0' report "TEST FAILED-0xL";
    assert xor_table( 'Z', 'L' ) = 'X' report "TEST FAILED-ZxL";
    assert xor_table( 'Z', 'H' ) = 'X' report "TEST FAILED-ZxH";
    wait;
  end process;
end only;
