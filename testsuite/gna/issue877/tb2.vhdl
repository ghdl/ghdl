entity tb is
end tb;

architecture behav of tb is
  constant msg : string := "hello world";
  procedure chk (b : natural)
  is
    subtype my_rng is natural range 1 to b;
  begin
    assert msg (chk.my_rng) = "hello";
  end chk;
begin
  chk(5);
end behav;
