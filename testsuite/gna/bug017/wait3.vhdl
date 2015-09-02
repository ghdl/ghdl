entity wait3 is
end;

architecture behav of wait3 is
  signal t : natural;
begin
  process
  begin
    report "hello";
    wait on t;
    report "hello2";
    wait for 0 ns;
    report "SUCCESS";
    wait;
  end process;
 
  t <= 1 after 2 ns, 2 after 4 ns;
end behav;
