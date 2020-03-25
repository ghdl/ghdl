library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity arr11 is
  port (val : std_logic_vector(3 downto 0);
        res : out natural);
end arr11;

architecture behav of arr11 is
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
  constant str3 : string := str2;
  
  constant pos3 : natural := find (str3, 'w');
begin
  assert pos1 = 7;
  assert pos3 = 5;
  
  res <= pos1;
end behav;
