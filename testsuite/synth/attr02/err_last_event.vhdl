entity err_last_event is
  port (i : bit; o : out boolean);
end;

architecture behav of err_last_event is
begin
  o <= i'last_event = 0 ns;
end;
