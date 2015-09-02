entity if2 is
end;

architecture behav of if2 is
begin
  process
    variable i : natural := 0;
  begin
    report "hello";
    loop
      if i = 10 then
        exit;
      else
        report "hello2";
        wait for 1 ns;
      end if;
      i := i + 1;
    end loop;
    report "SUCCESS";
    wait;
  end process;
 
end behav;
