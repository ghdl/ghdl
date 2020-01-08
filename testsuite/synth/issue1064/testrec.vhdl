library ieee;
use ieee.std_logic_1164.all;

entity testrec is
  port (i : std_ulogic;
        o : out std_ulogic);
end entity testrec;

architecture behaviour of testrec is
  type rec is record
    v : std_ulogic;
    t : std_ulogic;
  end record;

  procedure zot(e: inout rec) is
  begin
    e.v := '0';
  end;

begin
  execute1_1: process(i)
    variable v : rec;
  begin
    v.v := i;
    v.t := i;
    zot(v);
    o <= v.v;
  end process;
end architecture behaviour;
