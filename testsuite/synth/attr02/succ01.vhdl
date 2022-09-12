entity succ01 is
  port (i : integer;
        o : out integer);
end succ01;

architecture behav of succ01 is
begin
  o <= integer'succ(i);
end behav;
