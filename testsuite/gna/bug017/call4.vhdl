entity call4 is
end;

architecture behav of call4 is
  procedure p (n : natural) is
    procedure p2 (m : natural) is
    begin
      for i in 1 to m loop
        report "hello 2";
        wait for 1 ns;
      end loop;
    end p2;
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
