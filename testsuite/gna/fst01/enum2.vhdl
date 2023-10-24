library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity enum2 is
end;

architecture behav of enum2 is
  type state1_t is (s_start, s_cont, s_end);
  type state1_arr is array (3 downto 0) of state1_t;

  signal s1 : state1_arr;
begin
  process
  begin
    s1 <= (s_start, s_cont, s_cont, s_end);
    wait for 1 ns;
    for i in 0 to 16 loop
      for j in s1'range loop
        if s1(j) = state1_t'right then
          s1(j) <= state1_t'left;
        else
          s1(j) <= state1_t'rightof (s1(j));
        end if;
      end loop;
      wait for 1 ns;
    end loop;
    wait;
  end process;
end behav;
