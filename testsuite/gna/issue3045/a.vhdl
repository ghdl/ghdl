library ieee;
use ieee.math_real.all;
use std.env.finish;

entity A is
end A;

architecture sim of A is

  procedure B(D : string) is
  begin

  end procedure;

  constant C : real range 0.0 to 1.0 := 0.5;

begin

  process
    
  begin
    wait for 10 ns;

    B(to_string(C, 2));

    finish;
  end process;

end architecture;
