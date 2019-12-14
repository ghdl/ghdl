library ieee;
use ieee.std_logic_1164.all;

entity exit02 is
  port (val : std_logic_vector (3 downto 0);
        res : out integer);
end exit02;

architecture behav of exit02 is
  function ffs (v : std_logic_vector (3 downto 0)) return natural
  is
    variable r : natural;
  begin
    r := 4;
    for i in v'reverse_range loop
      if v (i) = '1' then
        r := i;
        exit;
      end if;
    end loop;
    return r;
  end ffs;
begin
  res <= ffs (val);
end behav;
