entity nand02 is
  port (a, b : boolean;
        o : out boolean);
end;

architecture behav of nand02 is
begin
  o <= a nand b;
end behav;
