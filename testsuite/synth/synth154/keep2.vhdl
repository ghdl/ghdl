library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity leds is
  port (clk : in std_logic;
        led1, led2, led3, led4, led5 : out std_logic);
end leds;

architecture blink of leds is

  signal clk_4hz: std_logic := '0';
  constant gates: integer := 3 - 1;
  constant max: integer := 3e6;
  signal A: std_logic_vector(0 to gates) := ( others => '0');

  signal B: std_logic := '1';
  signal C: std_logic := '1';
  signal val: std_logic := '0';
  signal data: std_logic := '0';

  attribute keep: string;
  attribute keep of A: signal is "true";
  attribute keep of B: signal is "TRue";
  attribute keep of C: signal is "false";
begin
  process (clk)
    variable counter : unsigned (23 downto 0) := (others => '0');
  begin
    if rising_edge(clk) then
      if counter >= max then
        counter := x"000000";
        clk_4hz <= not clk_4hz;
      else
        counter := counter + 1;
      end if;
    end if;
  end process;

GEN:
    for i in 0 to gates generate
        A(i) <= not A(gates - i);
    end generate GEN;

  led1 <= '0';
  led2 <= clk_4hz;
  led3 <= clk_4hz;
  led4 <= clk_4hz;
  led5 <= clk_4hz;
end;
