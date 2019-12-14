library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity ent is
end ent;


architecture sim of ent is

  signal s_clk  : std_logic := '1';
  signal s_test : std_logic := '0';

begin

  s_clk <= not s_clk after 5 ns;
  s_test <= '1' after 30 ns,
            '0' after 40 ns;

  process is
  begin
    wait until rising_edge(s_clk);
    -- This works
    if s_test'stable(10 ns) then
      report "s_test stable";
    else
      report "s_test changed";
    end if;
  end process;
 
  -- This works
  -- psl assert always (s_test'stable)@rising_edge(s_clk);
 
  -- This leads to an compile error
  -- psl assert always (s_test'stable(10 ns))@rising_edge(s_clk);
 
end architecture sim;
