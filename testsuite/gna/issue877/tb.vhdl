entity tb is
end tb;

architecture behav of tb is
  constant msg : string := "hello world";
begin
  assert msg (positive range 1 to 5) = "hello" severity failure;
  assert msg (positive range 7 to 11) = "world";
end behav;
