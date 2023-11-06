entity repro2 is
end;

architecture behav of repro2 is
begin
  process
    variable x : real := real (2.0 / 2);  
    variable y : real := real (2.0 * 2);  
    variable z : real := real (2 * 2.0);  
  begin
    wait;
  end process;
end;
