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
  i_test : entity work.test;

  p_proc : process
    alias sig    is <<signal g_test.i_test.sig : std_logic>>;
    procedure check(
      signal   clock      : std_logic
    ) is
    begin
    end procedure;
  begin
    check(sig);
    wait;
  end process;
end architecture;
