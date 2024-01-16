library ieee;
use ieee.std_logic_1164.all;

entity tb_playground is
end tb_playground;

architecture sim of tb_playground is

  signal s_test_signal : std_logic := '1';

  procedure test (constant stable_time : time := 100 us) is
    constant c_tmp : time := stable_time; -- local constant to circumvent "static" error message
  begin
    assert s_test_signal'stable(c_tmp) report "Signal was not stable!" severity failure;
    --                              ^ this causes the crash
  end procedure;

begin

  main: process
  begin
    wait for 10 us;
    test;
  end process;

end architecture;

