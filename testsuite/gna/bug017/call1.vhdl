entity call1 is
end;

architecture behav of call1 is
  procedure p is
  begin
    for i in 1 to 10 loop
      report "hello";
      wait for 1 ns;
    end loop;
  end p;
begin
  process
  begin
    p;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
