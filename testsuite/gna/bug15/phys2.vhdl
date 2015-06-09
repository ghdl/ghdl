entity tb2 is
end;

use work.physical.all;

architecture test of tb2 is
  constant CLOCK_FREQ    : FREQ    := MHz;
  procedure p (a : freq) is
  begin
  end p;
begin
  p (clock_freq);
  -- empty
end architecture;
