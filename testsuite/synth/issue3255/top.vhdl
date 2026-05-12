library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (
    digit : in integer range 0 to 9;
    led : out std_logic
  );
end a;

architecture rtl of a is

 begin
  A_PROC : process (digit)
  begin
    case digit is

      when 0 =>
        led <= '1';

      when others =>
        led <= '0';

    end case;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity top is
    port (
    led : out std_logic
  );
end top;

architecture rtl of top is
  signal digit : integer range 0 to 9;
begin

  A_INST : entity work.a(rtl)
  port map (
    digit => digit,
    led => led
  );
end architecture;
