library ieee;
use ieee.std_logic_1164.all;

entity tb_top is
end tb_top;

architecture sim of tb_top is

  signal led : std_logic;
 
begin

  DUT : entity work.top
  port map (
    led => led
  );

  TB_PROC : process
  begin
    wait for 10 ns;
    assert led = '1'
      report "The LED is off."
      severity failure;
    wait;
  end process;
end architecture;

