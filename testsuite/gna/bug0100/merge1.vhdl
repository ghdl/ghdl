entity hello is
end hello;

architecture behav of hello is
begin
 <<<<<<< HEAD
  assert false report "Hello VHDL world" severity note;
 =======
  assert false report "Hello VHDL old world" severity note;
 >>>>>>> bad
end behav;
