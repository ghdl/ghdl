library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity arr10 is
  port (val : std_logic_vector(3 downto 0);
        res : out natural);
end arr10;

architecture behav of arr10 is
  function find (s : string; c : character) return natural is
  begin
    for i in s'range loop
      if s (i) = c then
        return i;
      end if;
    end loop;
    return 0;
  end find;

  constant str1 : string := "hello world";
  constant pos1 : natural := find (str1, 'w');

  alias str2 : string (str1'length downto 1) is str1;
  constant pos2 : natural := find (str2, 'w');
begin
  assert pos1 = 7;
  assert pos2 = 5;
  
  res <= pos1;
end behav;
