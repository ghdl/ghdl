package enum_based_trit_arith is
  type trit is (N, O, P);
  function "+" (a, b: trit) return trit;
  function carry(a, b: trit) return trit;
  function to_string(a: trit) return string;
end package;

package body enum_based_trit_arith is
  type trit_tbl_1d is array (trit) of trit;
  type trit_tbl_2d is array (trit, trit) of trit;
  constant sum_tbl: trit_tbl_2d := (
  -- N  O  P
    (P, N, O), -- N
    (N, O, P), -- O
    (O, P, N)  -- P
  );
  constant carry_tbl: trit_tbl_2d := (
  -- N  O  P
    (N, O, O), -- N
    (O, O, O), -- 0
    (O, O, P)  -- P
  );
  function "+" (a, b: trit) return trit is
  begin
    return sum_tbl(a, b);
  end function;
  function carry(a, b: trit) return trit is
  begin
    return carry_tbl(a, b);
  end function;
  function to_string(a: trit) return string is
  begin
   return trit'image(a);
  end function;
end package body;

package generic_ternary_arith is
  generic (
    type trit;
    constant N, O, P: trit;
    function "+" (a, b: trit) return trit is <>;
    function carry(a, b: trit) return trit is <>;
    function to_string (a: trit) return string is <>
  );
  type trit_vector is array (integer range <>) of trit;
  function "+" (a, b: trit_vector) return trit_vector;
  function "+" (a: trit_vector; b: trit) return trit_vector;
  function to_string (a: trit_vector) return string;
end package;

package body generic_ternary_arith is
  function "+" (a, b: trit_vector) return trit_vector is
    constant width: natural := maximum(a'length, b'length);
    variable x, y: trit_vector (width-1 downto 0) := (others => O);
    variable s, c, ripple_carry: trit := O;
  begin
    x(a'length-1 downto 0) := a;
    y(b'length-1 downto 0) := b;
    for i in x'reverse_range loop
      s := x(i) + y(i);
      c := carry(x(i), y(i));
      x(i) := s + ripple_carry;
      ripple_carry := c + carry(s, ripple_carry);
    end loop;
    return x;
  end function;
  function "+" (a: trit_vector; b: trit) return trit_vector is
  begin
    return a + trit_vector'(1 => b);
  end function;
  function to_string (a: trit_vector) return string is
    variable s: string (1 to a'length);
    alias b: trit_vector(s'range) is a;
  begin
    for i in s'range loop
      s(i) := to_string(b(i))(1);
    end loop;
    return s;
  end function;
end package body;

use work.enum_based_trit_arith.all;
package enum_based_ternary_arith is new work.generic_ternary_arith
  generic map (
    trit => trit, N => N, O => O, P => P
  );

use work.enum_based_ternary_arith.all;
entity ternary_summator is
  generic (
    constant width: positive := 1
  );
  port (
    c_in  : in  trit;
    x, y  : in  trit_vector (width-1 downto 0);
    sum   : out trit_vector (width-1 downto 0);
    c_out : out trit
  );
end entity;

architecture dataflow of ternary_summator is
begin
  (c_out, sum) <= (O & x) + y + c_in;
end architecture;

use work.enum_based_ternary_arith.all;
entity tb is end entity;

architecture mixed of tb is
  constant width: natural := 3;
  signal a, b, c, d: trit := O;
  signal x, y, z: trit_vector (width-1 downto 0) := (others => O);
begin
  a <= O, P after 1 ns, N after 2 ns, P after 3 ns, N after 4 ns;
  b <= O, N after 1 ns, O after 2 ns, P after 3 ns, N after 4 ns;
  x <= (O, O, O), (O, O, P) after 1 ns, (O, N, P) after 2 ns, (O, N, P) after 3 ns, (N, P, N) after 4 ns;
  y <= (O, O, O), (O, O, N) after 1 ns, (O, N, O) after 2 ns, (P, N, P) after 3 ns, (N, P, N) after 4 ns;
  c <= a + b;
  summator: entity work.ternary_summator(dataflow)
    generic map (width => width)
    port map (c_in => O, x => x, y => y, sum => z, c_out => d);
  log: postponed process is
    alias s is to_string [trit return string];
    alias s is to_string [trit_vector return string];
  begin
    wait on c, z;
    report s(a) & " + " & s(b) & " = " & s(c);
    report s(x) & " + " & s(y) & " = " & s(z);
  end process;
end architecture;

