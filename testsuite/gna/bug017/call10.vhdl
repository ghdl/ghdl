entity call10 is
end;

architecture behav of call10 is
  procedure check2 (msg : string) is
  begin
     assert msg = "checking: abcedfghijklmnopqrstuvwxyz"
        severity failure;
     report "SUCCESS" severity note;
  end check2;
  
  procedure check1 (msg : string) is
  begin
    check2 ("checking: " & msg);
  end check1;
begin
   process
   begin
      check1 ("abcedfghijklmnopqrstuvwxyz");
      wait;
   end process;
end behav;
