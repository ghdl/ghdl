entity rightof01 is
  port (i : integer;
        o : out integer);
end rightof01;

architecture behav of rightof01 is
begin
  o <= integer'rightof(i);
end behav;
