--  A record inout port both driven and read back by the entity that owns
--  it must see contributions from outside that entity, not just its own
--  drive.  Unlike repro1-repro4, this does not check for invalid vhdl
--  output; it checks the read-back value itself is correct.
library ieee;
use ieee.std_logic_1164.all;
library work;
use work.pkg.all;

entity repro5 is
  port (
    en   : in    std_logic;
    pins : inout t_pins;
    rd_a : out   std_logic;
    rd_b : out   std_logic
  );
end entity repro5;

architecture rtl of repro5 is
begin
  pins.a <= '0' when en = '1' else 'Z';
  pins.b <= '1' when en = '1' else 'Z';
  rd_a <= pins.a;
  rd_b <= pins.b;
end architecture rtl;
