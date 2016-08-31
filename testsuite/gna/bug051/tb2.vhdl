entity tb2 is
end tb2;

architecture behav of tb2 is
  signal s : bit;
  signal clk : bit;
begin
 -- psl default clock is (clk'event and clk = '1');
 postponed assert always {s = '0'; s = '1'} severity failure;
 process
 begin
   s <= '1';
   wait for 0 ns;
   s <= '0';
   wait;
 end process;
end behav;
