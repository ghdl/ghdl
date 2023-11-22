library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_inner is
end entity;

architecture rtl of test_inner is
  signal sig  : std_logic;
begin
  p_proc : process
  begin
    sig <= '0';
    wait for 1 us;
    sig <= '1';
    wait;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
begin
  i_test : entity work.test_inner;

  p_proc : process
    alias sig is <<signal i_test.sig : std_logic>>;
  begin
    wait for 1 ns;
    wait until sig'event for 1 us;
    assert now = 1 us severity failure;
    wait;
  end process;
end architecture;
