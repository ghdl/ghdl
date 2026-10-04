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

