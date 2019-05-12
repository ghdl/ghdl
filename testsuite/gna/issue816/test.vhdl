library ieee;
context ieee.ieee_std_context;

library vunit_lib;
context vunit_lib.vunit_context;

entity tb_test is
  generic ( runner_cfg : string );
end entity;

architecture tb of tb_test is

  constant params: integer_vector_ptr_t := new_integer_vector_ptr(2, -1);

  constant clk_period    : time    := 20 ns;

  type time_t is array (natural range 0 to 1) of natural;

  procedure set_time(t: time_t) is begin
    set(params, 0, t(0));
    set(params, 1, t(1));
  end;

  procedure get_time(variable t: inout time_t) is begin
    t(0) := get(params, 0);
    t(1) := get(params, 1);
  end;

  impure function get_time return time_t is
    variable t: time_t;
  begin
--    t(0) := get(params, 0);
--    t(1) := get(params, 1);
    return t;
  end;

  constant null_time: time_t := (others => 0);

  impure function to_string(t: time_t) return string is begin
    return to_string(t(0)) & " " & to_string(t(1));
  end;

  signal clk: std_logic;

begin

  clk <= not clk after (clk_period/2);

  run: process(all)
    variable r: time_t;
  begin
    if rising_edge(clk) then
--      info("Time: " & to_string(get_time));
--      get_time(r);
    end if;
  end process;

  main: process
    variable t: time_t;
  begin
    test_runner_setup(runner, runner_cfg);
      get_time(t);
      if t = null_time then
        set_time((others=>natural'high));
      end if;
      get_time(t);
      info("Init test | time: " & to_string(t));
      info("Test done");
    test_runner_cleanup(runner);
    wait;
  end process;
  test_runner_watchdog(runner, 500 ms);

end architecture;
