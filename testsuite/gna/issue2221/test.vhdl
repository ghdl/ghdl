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
  alias sig is << signal i_test.sig : std_logic >>;
  procedure p_test(signal sig1 : in std_logic) is
  begin
  end procedure;
begin
  i_test : entity work.test;

  p_proc : process
  begin
    p_test(sig);
    wait;
  end process;
end architecture;
