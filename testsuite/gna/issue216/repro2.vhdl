entity repro2 is
end repro2;

architecture behav of repro2 is
  constant c : natural := 2;
  constant cmap : string (1 to 5) :=
    (1 => 'a', 2 => 'b', 3 => 'c', 4 => 'd', 5 => 'e');
begin
  assert cmap (c) = 'b';
  assert cmap & 'f' = "abcdef";
end behav;
