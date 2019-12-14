entity hello is
end hello;

architecture behav of hello is
begin
   assert false report "Hello world" severity note;
end behav;library
