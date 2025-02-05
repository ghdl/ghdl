library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
end;

architecture behav of repro2 is
  signal s   : std_logic_vector(31 downto 0);
begin


  process
    variable period : time;
    constant len : time := 5 ns;
  begin
    wait until falling_edge(s(0));
    period := s(0)'delayed(len)'last_active;
    report "cycle period measured " & time'image(period);
  end process;

  process
  begin
    for i in 1 to 5 loop
      s <= x"aa55aa55";
      wait for 1 ns;
      s <= x"55aa55aa";
      wait for 1 ns;
    end loop;

    wait;
  end process;
end behav;
