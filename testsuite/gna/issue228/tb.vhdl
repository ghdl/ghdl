library ieee;
use ieee.std_logic_1164.all;

entity foo is
  port (
    clk : in std_ulogic;
    a0 : in std_ulogic
  );
end entity;

architecture bar of foo is
begin
  -- psl default clock is rising_edge(clk);
  -- psl sequence rising_a0 is {not(a0); a0};
  -- psl sequence falling_a0 is {a0; not(a0)};
  -- psl cover {rising_a0};
  -- psl cover {falling_a0} report "falling_a0 custom report";
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo_tb is
end entity;

architecture tb of foo_tb is
  signal clk : std_ulogic := '0';
  signal a0 : std_ulogic;
begin

  clk_gen:
    process
    begin
      for i in 0 to 10 loop
        clk <= not(clk);
        wait for 10 ns;
      end loop;
      wait;
    end process;

  test_driver:
    process
    begin
      a0 <= '1';
      wait until rising_edge(clk);
      a0 <= '0';
      wait until rising_edge(clk);
      a0 <= '1';
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      wait;
    end process;

  dut:
    entity work.foo(bar)
    port map (
      clk => clk,
      a0 => a0);

end architecture;
