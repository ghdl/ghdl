entity leftof01 is
  port (i : integer;
        o : out integer);
end leftof01;

architecture behav of leftof01 is
begin
  o <= integer'leftof(i);
end behav;
