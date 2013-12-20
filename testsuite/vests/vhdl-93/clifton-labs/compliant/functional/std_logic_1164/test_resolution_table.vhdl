entity test is
end test;

library ieee;
use ieee.std_logic_1164.all;

package foo is
  TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;
  CONSTANT resolution_table : stdlogic_table := (
--  ---------------------------------------------------------
--  |  U    X    0    1    Z    W    L    H    -        |   |
--  ---------------------------------------------------------
    ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ), -- | U |
    ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ), -- | X |
    ( 'U', 'X', '0', 'X', '0', '0', '0', '0', 'X' ), -- | 0 |
    ( 'U', 'X', 'X', '1', '1', '1', '1', '1', 'X' ), -- | 1 |
    ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', 'X' ), -- | Z |
    ( 'U', 'X', '0', '1', 'W', 'W', 'W', 'W', 'X' ), -- | W |
    ( 'U', 'X', '0', '1', 'L', 'W', 'L', 'W', 'X' ), -- | L |
    ( 'U', 'X', '0', '1', 'H', 'W', 'W', 'H', 'X' ), -- | H |
    ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )  -- | - |
    );
end foo;

use work.foo.all;

library ieee;
use ieee.std_logic_1164.all;

architecture only of test is

begin  -- only
  process
  begin  -- process
    assert resolution_table( 'U', 'U' ) = 'U' report "TEST FAILED-UxU";
    assert resolution_table( 'U', 'X' ) = 'U' report "TEST FAILED-UxX";
    assert resolution_table( 'X', '-' ) = 'X' report "TEST FAILED-Xx-";
    assert resolution_table( '0', '1' ) = 'X' report "TEST FAILED-0x1";
    assert resolution_table( 'H', 'Z' ) = 'H' report "TEST FAILED-HxZ";
    assert resolution_table( 'Z', 'W' ) = 'W' report "TEST FAILED-ZxW";
    assert resolution_table( 'L', '1' ) = '1' report "TEST FAILED-Lx1";
    assert resolution_table( '0', 'L' ) = '0' report "TEST FAILED-0xL";
    assert resolution_table( 'Z', 'L' ) = 'L' report "TEST FAILED-ZxL";
    assert resolution_table( 'Z', 'H' ) = 'H' report "TEST FAILED-ZxH";
    wait;
  end process;
end only;
