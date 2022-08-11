library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
  signal sig  : unsigned(1 downto 0);
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
    alias sig    is <<signal i_test.sig : unsigned>>;
  begin
    sig <= force "11";
    wait;
  end process;
end architecture;
