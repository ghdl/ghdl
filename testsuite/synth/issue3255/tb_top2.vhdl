library ieee;
use ieee.std_logic_1164.all;

entity tb_top2 is
end tb_top2;

architecture sim of tb_top2 is
  signal led : std_logic;
begin

  DUT : entity work.top2
  port map (
    led => led
  );

  TB_PROC : process
  begin
    wait for 10 ns;
    assert led = '1' report "The LED is on." severity failure;
    wait;
  end process;
end architecture;

