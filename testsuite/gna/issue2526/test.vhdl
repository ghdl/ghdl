library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
  signal sig  : std_logic;
begin
  p_proc : process
  begin
    sig <= '1';
    wait for 100 us;
    sig <= '0';
    wait;
  end process;
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
    alias sig is <<signal i_test.sig : std_logic>>;
  begin
    wait for 1 us;
    while sig = '0' loop
      wait for 1 us;
    end loop;
    wait;
  end process;
end architecture;
