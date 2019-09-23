library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func08 is
  port (v : std_ulogic_vector (31 downto 0);
        r : out integer);
end func08;

architecture behav of func08 is
  function fls (val: std_ulogic_vector(31 downto 0)) return integer is
    variable ret: integer;
  begin
    ret := 32;
    for i in val'range loop
      if val(i) = '1' then
        ret := 31 - i;
        exit;
      end if;
    end loop;

    return ret;
  end;
begin
  r <= fls(v);
end behav;
