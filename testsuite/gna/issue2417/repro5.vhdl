package repro5_pkg is
  type bit_vec_vec is array (natural range <>) of bit_vector;

  procedure p1(signal ack: bit; variable v: inout bit_vec_vec);
  procedure p2(signal ack: bit; variable v: inout bit_vec_vec(1 downto 0));
  procedure p3(signal ack: bit; variable v: inout bit_vec_vec(open)(3 downto 0));
end repro5_pkg;

package body repro5_pkg is
  procedure p1(signal ack: bit; variable v: inout bit_vec_vec) is
  begin
    wait until ack = '1';
    for i in v'range loop
      v(i) := not v(i);
    end loop;
  end;

  procedure p2(signal ack: bit; variable v: inout bit_vec_vec(1 downto 0)) is
  begin
    wait until ack = '1';
    for i in v'range loop
      v(i) := not v(i);
    end loop;
  end;

  procedure p3(signal ack: bit;
               variable v: inout bit_vec_vec(open)(3 downto 0)) is
  begin
    wait until ack = '1';
    for i in v'range loop
      v(i) := not v(i);
    end loop;
  end;
end;

use work.repro5_pkg.all;

entity repro5 is
end;

architecture arch of repro5 is
  signal ack1, ack2 : bit := '0';
begin
  process
    variable v : bit_vec_vec(1 downto 0)(3 downto 0);
  begin
    v (0) := x"3";
    v (1) := x"c";
    p3 (ack1, v);
    assert v(0) = x"c" and v(1) = x"3" severity failure;
    wait;
  end process;

  process
    variable v : bit_vec_vec(1 downto 0)(3 downto 0);
  begin
    v (0) := x"1";
    v (1) := x"d";
    p3 (ack1, v);
    assert v(0) = x"e" and v(1) = x"2" severity failure;
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
