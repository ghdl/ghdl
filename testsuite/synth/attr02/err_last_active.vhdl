entity err_last_active is
  port (i : bit; o : out boolean);
end;

architecture behav of err_last_active is
begin
  o <= i'last_active = 0 ns;
end;
