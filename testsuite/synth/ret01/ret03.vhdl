library ieee;
use ieee.std_logic_1164.all;

entity ret03 is
  port (di : std_logic_vector (7 downto 0);
        res : out integer);
end ret03;

architecture behav of ret03 is
  function ffs (v : std_logic_vector (7 downto 0)) return integer is
  begin
    for i in v'range loop
      if v (i) = '1' then
        return i;
      end if;
    end loop;
    return -1;
  end ffs;
begin
  res <= ffs (di);
end behav;
