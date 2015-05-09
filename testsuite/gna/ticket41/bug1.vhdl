entity bug1 is
  
end bug1;

architecture behav of bug1 is
  constant c : natural := 5;
  function c return natural is
  begin
    return 7;
  end;
begin  -- behav


end behav;
