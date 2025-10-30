library ieee;
use ieee.std_logic_1164.all;

entity ps2_cursor is
  port (
    clk_i : in std_logic;
    but_i : in std_logic;
    led_o : out std_logic
    );
end entity;

architecture rtl of ps2_cursor is
begin
  led_o <= '0';

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      led_o <= but_i;
    end if;
  end process;
end architecture;
