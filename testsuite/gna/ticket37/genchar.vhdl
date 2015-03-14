entity genchar is
  generic (val : character := CR);
end genchar;

architecture behav of genchar is
begin
  assert val = Ack or val = 'A' or val = 'z' severity failure;
end behav;
