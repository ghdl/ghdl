entity e1 is
end e1;

architecture behav of e1 is
begin
  assert false report "arch" severity note;
end behav;
