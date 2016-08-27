entity tb is
end tb;

architecture behav of tb is
  signal s : bit;
begin
  postponed assert s = 0;
end behav;
