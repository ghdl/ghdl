--  Multi-field record inout port with a default value and no explicit
--  assignment in the architecture at all.
library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity repro4 is
  port (
    pins : inout t_pins := (a => '1', b => '0')
  );
end entity repro4;

architecture rtl of repro4 is
begin
end architecture rtl;
