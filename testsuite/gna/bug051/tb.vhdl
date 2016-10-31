entity tb is
end tb;

architecture behav of tb is
  signal s : bit;
begin
 postponed assert s = '0' severity failure;
 process
 begin
   s <= '1';
   wait for 0 ns;
   s <= '0';
   wait;
 end process;
end behav;
