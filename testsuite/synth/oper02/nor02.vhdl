entity nor02 is
  port (a, b : boolean;
        o : out boolean);
end;

architecture behav of nor02 is
begin
  o <= a nor b;
end behav;
