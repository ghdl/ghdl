library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package B is
  generic (
    A : positive
  );
  subtype addr_t is std_logic_vector(A - 1 downto 0);
end package;

package C is new work.B
 generic map (
   A => 10
 );

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package D is
  generic (
    A : positive
  );
  subtype addr_t is std_logic_vector(A - 1 downto 0);
end package;

use work.C.all;

package E is new work.D
  generic map (
    A => work.C.A 
  );

use work.E.all;

entity F is
end F;

architecture sim of F is
begin
end architecture;
