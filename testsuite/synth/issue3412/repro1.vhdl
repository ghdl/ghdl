--  Multi-field record inout port forwarded, unmodified, through one
--  level of hierarchy to a sub-instance (the original report).
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

entity repro1 is
  port (
    en   : in    std_logic;
    pins : inout t_pins
  );
end entity repro1;

architecture rtl of repro1 is
begin
  i_inner : entity work.inner
    port map (en => en, pins => pins);
end architecture rtl;
