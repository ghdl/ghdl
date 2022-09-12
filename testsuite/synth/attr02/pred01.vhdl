entity pred01 is
  port (i : integer;
        o : out integer);
end pred01;

architecture behav of pred01 is
begin
  o <= integer'pred(i);
end behav;
