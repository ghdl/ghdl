--  Multi-field record inout port forwarded to a sub-instance via
--  individual (field-by-field) port association: pins.a => pin_a,
--  pins.b => pin_b, rather than pins => pins.
library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity inner is
  port (
    en   : in    std_logic;
    pins : inout t_pins
  );
end entity inner;

architecture rtl of inner is
begin
  pins.a <= '1' when en = '1' else 'Z';
  pins.b <= '0' when en = '1' else 'Z';
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity repro3 is
  port (
    en    : in    std_logic;
    pin_a : inout std_logic;
    pin_b : inout std_logic
  );
end entity repro3;

architecture rtl of repro3 is
begin
  i_inner : entity work.inner
    port map (en => en, pins.a => pin_a, pins.b => pin_b);
end architecture rtl;
