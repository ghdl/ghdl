entity wait2 is
end;

architecture behav of wait2 is
  signal t : natural;
begin
  process
  begin
    report "hello";
    wait until t = 2;
    report "SUCCESS";
    wait;
  end process;
 
  t <= 1 after 2 ns, 2 after 4 ns;
end behav;
