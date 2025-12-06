entity err_last_val is
  port (i : bit; o : out bit);
end;

architecture behav of err_last_val is
begin
  o <= i'last_value;
end;
