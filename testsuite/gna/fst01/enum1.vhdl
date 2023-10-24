library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity enum1 is
end;

architecture behav of enum1 is
  type state1_t is (s_start, s_cont, s_end);
  type state2_t is ('a', 'b', 'c', 'd');

  signal s1 : state1_t;
  signal s2 : state2_t;
  signal c : character;
begin
  process
  begin
    for i in 0 to 16 loop
      if s1 = state1_t'right then
        s1 <= state1_t'left;
      else
        s1 <= state1_t'rightof (s1);
      end if;

      if s2 = state2_t'right then
        s2 <= state2_t'left;
      else
        s2 <= state2_t'rightof (s2);
      end if;

      if c = character'right then
        c <= character'left;
      else
        c <= character'rightof (c);
      end if;
      wait for 1 ns;
    end loop;
    wait;
  end process;
end behav;
