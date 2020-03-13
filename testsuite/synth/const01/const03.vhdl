library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity const03 is
  port (v : std_logic_vector (15 downto 0);
        o : out std_logic_vector (3 downto 0));
end const03;

architecture behav of const03 is
  function count (vec : std_logic_vector) return std_logic_vector
  is
    variable res : std_logic_vector (3 downto 0) := x"0";
  begin
    for i in vec'range loop
      if vec (i) = '1' then
        res := std_logic_vector (unsigned(res) + 1);
      end if;
    end loop;
    return res;
  end count;
begin
  o <= count(v);
end behav;
