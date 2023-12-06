

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture rtl of test1 is
  function xorWidth (datwidth : INTEGER range 4 to 64) RETURN integer IS
  begin
    return 1;
  end function;
  procedure xorWidth (datwidth : INTEGER range 4 to 64) IS
  begin
  end procedure;
  signal sig : unsigned(15 downto 0);
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
begin
  i_test1 : entity work.test1;
  p_proc : process
    alias sig    is <<signal i_test1.sig : unsigned>>;
  begin
    wait;
  end process;
end architecture;

