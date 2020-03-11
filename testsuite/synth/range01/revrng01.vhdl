library ieee;
use ieee.std_logic_1164.all;

entity revrng01 is
  port (a : std_logic_vector (7 downto 0);
        o : out std_logic_vector (7 downto 0));
end revrng01;

architecture behav of revrng01 is
  function rev (v : std_logic_vector) return std_logic_vector
  is
    variable temp : std_logic_vector(v'reverse_range);
  begin
    for i in v'range loop
      temp (i) := v (i);
    end loop;
    return temp;
  end rev;
begin
  o <= rev (a);
end behav;
