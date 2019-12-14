library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func07 is
  port (v : std_ulogic_vector (15 downto 8);
        r : out std_ulogic_vector (7 downto 0));
end func07;

architecture behav of func07 is
  function cnt (val: std_ulogic_vector(7 downto 0)) return std_ulogic_vector is
    variable ret: unsigned(3 downto 0) := (others => '0');
  begin
    for i in val'range loop
      ret := ret + ("000" & val(i));
    end loop;

    return std_ulogic_vector(resize(ret, val'length));
  end;
begin
  r <= cnt (v);
end behav;
