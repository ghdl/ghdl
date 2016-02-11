entity repro is
end repro;

architecture behav of repro is
  constant c1 : character := character'value("ack");
  constant c2 : character := character'value("'Z'");
  constant c3 : character := character'value("'a'");
  constant c4 : boolean := boolean'value("TruE");
  constant c5 : boolean := boolean'value("TruE  ");
  constant c6 : boolean := boolean'value(" TruE ");
begin
  assert c1 = ack report "value incorrect for ack" severity failure;
  assert c2 = 'Z' report "value incorrect for 'Z'" severity failure;
  assert c3 = 'a' report "value incorrect for 'a'" severity failure;
  assert c4 and c5 and c6 severity failure;
end behav;
