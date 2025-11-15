library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reprod is
  generic (works : integer := 0);
  port (iclk   : in  std_ulogic);
end;

architecture rtl of reprod is

  type typea is record
    el  : std_ulogic;
  end record;


  procedure setx(d: out std_ulogic) is
  begin
    d := '0';
  --pragma translate_off
    d := 'X';
  --pragma translate_on
  end setx;


begin

  icomb: process(iclk)
    variable vi: typea;
    variable el  : std_ulogic;
  begin
    if works = 0 then
      setx(vi.el);
    else
      setx(el);
    end if;
  end process;

end;
