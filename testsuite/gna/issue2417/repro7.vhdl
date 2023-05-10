library ieee;
use ieee.std_logic_1164.all;

package repro7_pkg is
  procedure p1(signal ack: bit; constant v:  std_logic_vector);
end repro7_pkg;

package body repro7_pkg is
  procedure p1(signal ack: bit; constant v:  std_logic_vector) is
  begin
    for i in 1 to 4 loop
      wait until ack = '1';
      report "v'left=" & integer'image(v'left)
        & ", v'right=" & integer'image(v'right);
      assert (v'left = 31 or v'left = 29) and v'right = 0 severity failure;
    end loop;
  end;
end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.repro7_pkg.all;

entity repro7 is
end;

architecture arch of repro7 is
  signal ack1, ack2 : bit := '0';

begin
  process
  begin
    p1 (ack1, std_logic_vector(to_unsigned(25, 32)));
    wait;
  end process;

  process
  begin
    p1 (ack1, std_logic_vector(to_unsigned(25, 30)));
    wait;
  end process;

  process
  begin
    for i in 1 to 5 loop 
      ack1 <= '1';
      wait for 1 ns;
      ack1 <= '0';
      ack2 <= '1';
      wait for 1 ns;
      ack2 <= '0';
    end loop;
    report "end of test";
    wait;
  end process;
end;
