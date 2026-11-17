library ieee;
use ieee.std_logic_1164.all;

entity myentity is
end entity;

architecture sim of myentity is
  signal clk : std_logic := '0';
  signal src : std_logic := '0';
  signal x   : std_logic := '0';
begin
  --  Rising edge at 5 ns; the process resumes next at 7 ns.
  clkp : process
  begin
    wait for 5 ns;
    clk <= '1';
    wait for 2 ns;
    clk <= '0';
    wait;
  end process;

  src <= '1' after 1 ns;

  --  At the rising edge of clk (5 ns), a transaction of x for 6 ns is pending.
  x <= transport src after 5 ns;

  check : process
  begin
    wait on x;
    assert now = 6 ns
      report "x changed at " & time'image (now) & ", expected 6 ns"
      severity failure;
    wait;
  end process;
end architecture;
