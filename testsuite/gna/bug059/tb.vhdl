entity tb is
end;

architecture behav of tb is
begin
  b : block
    generic (c : natural);
    generic map (c => c);
  begin
    assert true;
  end block b;
end behav;
