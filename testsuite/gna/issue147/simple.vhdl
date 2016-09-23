entity simple is
end;

architecture behav of simple is
begin
   assert false report "Hello" severity note;
end;

