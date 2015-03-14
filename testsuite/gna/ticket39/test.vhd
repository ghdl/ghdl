entity test is
end entity test;

library ieee;
use ieee.std_logic_1164.all;

architecture test of test is

  type state_t is record
    a : real;
  end record state_t;

  procedure p1 (
    variable state : inout state_t;
    a : in std_ulogic_vector(1 downto 0)) is
  begin
    report "test " & std_ulogic'image(a(1)) & std_ulogic'image(a(0)) severity note;
  end procedure p1;

  procedure p2 (
    variable state : inout state_t;
    n : in natural) is
    variable b : std_ulogic;
  begin
    b := '0';
    for i in 0 to n loop
      p1(state => state, a(0) => b, a(1) => 'X');
      b := not b;
      state.a := state.a + 1.0;
    end loop;
  end procedure p2;

begin

  p_p: process is
    variable state : state_t;
  begin
    state.a := 0.0;
    p2(state, 2);
    p2(state, 2);
    wait;
  end process p_p;

end architecture test;
