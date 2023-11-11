library ieee;
use ieee.std_logic_1164.all;

library zork;
use zork.foo.all;

entity foobar_tb is
end;

architecture testbench of foobar_tb is
  signal g1a, g1b, g1q_n : std_logic;
begin

  uut : foobar port map(
    g1a => g1a, g1b => g1b, g1q_n => g1q_n
    );

  process
  begin
    wait for 5 ns;
    g1a <= '0'; g1b <= '0'; wait for 5 ns; assert g1q_n = '1';
    wait;
  end process;

end;
