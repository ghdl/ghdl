library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2s is
end entity;

architecture rtl of test2s is
  signal sig  : std_logic;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
end entity;

architecture rtl of test2 is
begin
  i_test : entity work.test2s;

  p_proc : process
    alias sig    is <<signal i_test.sig : std_logic>>;
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
