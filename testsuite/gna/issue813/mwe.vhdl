entity flip_flop_7474 is
    Port ( j: in bit);
begin
    assert j'stable (6 ns);
end flip_flop_7474;
