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

entity top2_drv is
  port (digit : out integer range 0 to 9);
end;

architecture rtl of top2_drv is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity top2 is
    port (
    led : out std_logic
  );
end top2;

architecture rtl of top2 is
  signal digit : integer range 0 to 9;
begin
  drv_inst : entity work.top2_drv
    port map (digit => digit);

  A_INST : entity work.a(rtl)
  port map (
    digit => digit,
    led => led
  );
end architecture;
