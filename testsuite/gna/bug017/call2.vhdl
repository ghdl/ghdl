entity call2 is
end;

architecture behav of call2 is
  procedure p (n : natural) is
  begin
    for i in 1 to n loop
      report "hello";
      wait for 1 ns;
    end loop;
  end p;
begin
  process
  begin
    p (5);
    report "SUCCESS";
    wait;
  end process;
 
end behav;
