entity nor01 is
  port (a, b : bit;
        o : out bit);
end;

architecture behav of nor01 is
begin
  o <= a nor b;
end behav;
