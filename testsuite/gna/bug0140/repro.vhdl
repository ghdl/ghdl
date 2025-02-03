library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture behav of repro is
  signal s   : std_logic_vector(31 downto 0);
begin


  process
    variable period : time;
  begin
    wait until falling_edge(s(0));
    period := s(0)'delayed'last_active;
    report "cycle period measured " & time'image(period);
  end process;

  process
  begin
    wait for 1 ns;
    s <= x"aa55aa55";
    wait for 1 ns;
    s <= x"55aa55aa";
    wait for 1 ns;

    wait;
  end process;
end behav;

