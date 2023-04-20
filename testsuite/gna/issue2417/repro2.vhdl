package repro2_pkg is
  function resolver(v : bit_vector) return bit;

  subtype sbit_vector is (resolver) bit_vector;

  procedure p(signal ack: bit; variable v: inout sbit_vector);
end repro2_pkg;

package body repro2_pkg is
  function resolver(v : bit_vector) return bit is
  begin
    if v = (v'range => '0') then
      return '0';
    else
      return '1';
    end if;
  end resolver;

  procedure p(signal ack: bit; variable v: inout sbit_vector) is
  begin
    wait until ack = '1';
    v := not v;
  end p;
end;

use work.repro2_pkg.all;

entity repro2 is
end;

architecture arch of repro2 is
  signal ack1, ack2 : bit := '0';
begin
  process
    variable v : bit_vector(7 downto 0);
  begin
    v := x"c3";
    p (ack1, v);
    assert v = x"3c" severity failure;
    wait;
  end process;

  process
    variable v : bit_vector(7 downto 0);
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
