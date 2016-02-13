library ieee;
  use ieee.std_logic_1164.all;

library std;
  use std.env.all;



entity cover_report2 is
end entity cover_report2;



architecture test of cover_report2 is


  signal s_a   : std_logic;
  signal s_b   : std_logic;
  signal s_c   : std_logic;
  signal s_clk : std_logic := '0';


begin


  s_clk <= not(s_clk) after 5 ns;


  process is
  begin
    s_a <= '0';
    s_b <= '0';
    s_c <= '0';
    wait until rising_edge(s_clk);
    s_a <= '1';
    wait until rising_edge(s_clk);
    s_a <= '0';
    s_b <= '1';
    wait until rising_edge(s_clk);
    s_b <= '0';
    s_c <= '1';
    wait until rising_edge(s_clk);
    s_c <= '0';
    stop(0);
  end process;


  -- psl default clock is rising_edge(s_clk);
  --
  -- psl sequence test_p is {s_a; s_b};
  --
  -- DOES WORK
  -- -- psl TEST : cover test_p;
  --
  -- DOESN'T WORK:
  -- psl cover test_p report "Covered";


end architecture test;
