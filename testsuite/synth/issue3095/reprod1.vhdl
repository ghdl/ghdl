library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reprod1 is
  port (iclk   : in  std_ulogic;
        res : out std_ulogic);
end;

architecture rtl of reprod1 is
  type typea is record
    el  : std_ulogic;
  end record;

  procedure setx(d: out std_ulogic) is
  begin
    d := '0';
  end setx;
begin
  icomb: process(iclk)
    variable vi: typea;
  begin
    setx(vi.el);
    res <= vi.el;
  end process;
end;
