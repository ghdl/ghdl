library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func08b is
  port (v : std_ulogic_vector (3 downto 0);
        r : out integer);
end func08b;

architecture behav of func08b is
  function fls (val: std_ulogic_vector(3 downto 0)) return integer is
    variable ret: integer;
  begin
    ret := 4;
    for i in val'range loop
      if val(i) = '1' then
        ret := 3 - i;
        exit;
      end if;
    end loop;

    return ret;
  end;
begin
  r <= fls(v);
end behav;
