library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
  signal sig  : std_logic;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture rtl of test1 is
begin
  g_test : for i in 0 to 1 generate
  begin
    i_test : entity work.test;
  end generate;
  p_proc : process
    alias sig    is <<signal g_test(0).i_test.sig : std_logic>>;
  begin
    wait;
  end process;
end architecture;
