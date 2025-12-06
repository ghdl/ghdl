entity nand01 is
  port (a, b : bit;
        o : out bit);
end;

architecture behav of nand01 is
begin
  o <= a nand b;
end behav;
