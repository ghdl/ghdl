entity call3 is
end;

architecture behav of call3 is
  procedure p2 (n : natural) is
  begin
    for i in 1 to n loop
      report "hello 2";
      wait for 1 ns;
    end loop;
  end p2;

  procedure p (n : natural) is
  begin
    for i in 1 to n loop
      p2 (i);
      report "hello 1";
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
