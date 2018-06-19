entity repro1a is
  generic (depth : natural := 3);
end repro1a;

architecture behav of repro1a is
  component repro1b is
    generic (depth : natural);
  end component;
begin
  assert false report "repro1a: depth = " & natural'image (depth)
    severity note;

  comp1a : repro1b
    generic map (depth => depth);
end behav;
