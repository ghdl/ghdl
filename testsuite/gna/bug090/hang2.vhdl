entity hello is
disconnect

architecture behav of hello is
begin
   assert false report "Hello world" severity note;
end behav;
