library ieee;
use ieee.std_logic_1164.all;

package repro1_pkg is
  procedure p(signal ack: bit; variable v: inout std_logic_vector);
end repro1_pkg;

package body repro1_pkg is
  procedure p(signal ack: bit; variable v: inout std_logic_vector) is
  begin
    wait until ack = '1';
    v := not v;
  end p;
end;

library ieee;
use ieee.std_logic_1164.all;
use work.repro1_pkg.all;

entity repro1 is
end;

architecture arch of repro1 is
  signal ack1, ack2 : bit := '0';
begin
  process
    variable v : std_ulogic_vector(7 downto 0);
  begin
    v := x"c3";
    p (ack1, v);
    assert v = x"3c" severity failure;
    wait;
  end process;

  process
    variable v : std_ulogic_vector(7 downto 0);
  begin
    v := x"e1";
    p (ack2, v);
    assert v = x"1e" severity failure;
    wait;
  end process;

  process
  begin
    ack1 <= '1';
    wait for 1 ns;
    ack2 <= '1';
    wait for 1 ns;
    report "end of test";
    wait;
  end process;
end;
