entity hello is
end hello;

architecture behav of hello is
begin
  assert false report "Hello VHDL world" severity note;
end behav;
