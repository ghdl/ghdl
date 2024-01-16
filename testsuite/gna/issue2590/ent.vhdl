library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture beh of test1 is
  signal sig  : std_logic_vector(7 downto 0);
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
    alias sig is <<signal i_test.sig : std_logic_vector(7 downto 0)>>;
    constant C_VAL : std_logic_vector(7 downto 0) := (others => '1');
  begin
    sig(C_VAL'range) <= force C_VAL;
    wait;
  end process;
end architecture;
