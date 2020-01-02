library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (i : std_ulogic;
        o : out std_ulogic);
end entity test;

architecture behaviour of test is
    procedure zot(e: inout std_ulogic) is
    begin
        e := '0';
    end;

begin
    execute1_1: process(all)
        variable blah: std_ulogic;
    begin
      blah := i;
      zot(blah);
      o <= blah;
    end process;
end architecture behaviour;
