entity name1 is
end name1;

architecture behav of name1 is
begin
  assert false report "hello" severity note;
end behav;
