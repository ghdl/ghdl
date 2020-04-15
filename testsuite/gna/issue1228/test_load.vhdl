library ieee ;
use ieee.std_logic_1164.all;

entity test_load is
end test_load;

architecture RTL of test_load is
  signal w        :   std_ulogic_vector(0 to 47);
  signal \extend.id\ : std_ulogic := 'H';
  signal clk : std_logic;
begin
  process
  begin
    for i in 1 to 10 loop
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;
  
  process(clk)
  begin
    if (clk'event and clk = '1') then
      w <= not w;
    end if;
  end process;
end RTL;
