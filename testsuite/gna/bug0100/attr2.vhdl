entity attr2 is
end attr2;

architecture behav of attr2 is
  attribute my_attr : boolean;
  signal sig : bit;
  attribute my_attr of sig;
begin
end behav;
