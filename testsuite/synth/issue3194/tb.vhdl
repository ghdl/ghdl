library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.float16_pkg.all;

entity tb is end entity;

architecture sim of tb is
  signal a, b, y : std_logic_vector(15 downto 0) := (others => '0');

  type test_vec_t is record
    a, b, y : real;
  end record;
  type test_vec_arr_t is array (natural range<>) of test_vec_t;
  constant TEST_VEC : test_vec_arr_t := (
    (0.0, 0.0, 0.0),
    (1.0, 0.0, 1.0),
    (0.0, 1.0, 1.0)
  );
begin
  dut : entity work.fadd port map (a => a, b => b, y => y);

  test : process
    variable v : real;
  begin
    for i in TEST_VEC'range loop
      a <= to_slv(to_float(TEST_VEC(i).a));
      b <= to_slv(to_float(TEST_VEC(i).b));
      wait for 1 ns;
      v := to_real(to_float(y));
      assert v = TEST_VEC(i).y
        report to_string(TEST_VEC(i).a) & " + " & to_string(TEST_VEC(i).b) &
          " != " & to_string(TEST_VEC(i).y) & ", got: " & to_string(v)
        severity error;
    end loop;

    wait;
  end process;
end architecture;
