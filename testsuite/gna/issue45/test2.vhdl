library ieee;
use ieee.std_logic_1164.all;

entity psl_test2_endpoint is
end entity psl_test2_endpoint;

architecture test of psl_test2_endpoint is
  signal s_clk   : std_logic := '0';
  signal req, grant : std_logic;
begin

  grant <= '0';
  
  process
  begin
    for i in 1 to 10 loop
      s_clk   <= not s_clk;
      if i = 5 then
        req <= '1';
      end if;
      wait for 10 ns;
    end loop;
    wait;
  end process;

  -- psl endpoint e_test is {req; not(grant)} @rising_edge (s_clk);
  
  process
  begin
    wait until e_test;
    report "e_test hit" severity error;
    wait;
  end process;
end architecture test;
