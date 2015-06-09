package physical is
  type FREQ is range 0 to INTEGER'high units
    Hz;
    kHz = 1000 Hz;
    MHz = 1000 kHz;
    GHz = 1000 MHz;
--    THz = 1000 GHz;
  end units;
end package;

entity tb is
end;

use work.physical.all;

architecture test of tb is
  constant CLOCK_FREQ    : FREQ    := 100.0 MHz;
  procedure p (a : freq := 1.0 Mhz) is
  begin
  end p;
begin
  p (clock_freq);
  -- empty
end architecture;
