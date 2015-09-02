entity ret1 is
end;

architecture behav of ret1 is
  procedure p (n : natural) is
    variable i : natural := 0;
  begin
    loop
      report "hello 1";
      wait for 1 ns;
      if i = n then
        return;
      end if;
      i := i + 1;
    end loop;
  end p;
begin
  process
  begin
    p (5);
    report "SUCCESS: hello3";
    wait;
  end process;
 
end behav;
