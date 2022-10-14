library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture beh of test1 is
  signal sig  : std_logic;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture beh of test is
begin
  i_test : entity work.test1;

  p_proc : process
  begin
    <<signal i_test.sig : std_logic >> <= force '0';
    <<signal i_test.sig : std_logic >> <= release;
    wait;
  end process;
end architecture;
