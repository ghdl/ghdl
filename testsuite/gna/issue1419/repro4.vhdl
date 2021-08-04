entity repro4 is
end;

architecture behav of repro4 is
  type natural_array is array(natural range <>) of natural;

  constant k : natural_array := (0, 1, -2);
begin
end;
