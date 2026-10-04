
library ieee;
  use ieee.std_logic_1164.all;
library work;
  use work.pkg.all;

entity top is
  port (
    en   : in    std_logic;
    pins : inout t_pins
  );
end entity top;

architecture rtl of top is
begin
  i_inner : entity work.inner
    port map (en => en, pins => pins);
end architecture rtl;
