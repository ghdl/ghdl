library ieee;
use ieee.std_logic_1164.all;

entity exit01 is
  port (val : std_logic_vector (3 downto 0);
        res : out integer);
end exit01;

architecture behav of exit01 is
begin
  process(val)
  begin
    res <= 4;
    for i in val'reverse_range loop
      if val (i) = '1' then
        res <= i;
        exit;
      end if;
    end loop;
  end process;
end behav;
