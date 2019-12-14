entity repro1b is
  generic (depth : natural);
end repro1b;

architecture behav of repro1b is
  component repro1a is
    generic (depth : natural);
  end component;
begin
  assert false report "repro1b: depth = " & natural'image (depth)
    severity note;

  g : if depth > 0 generate
    comp1b : repro1a
      generic map (depth => depth - 1);
  end generate;
end behav;
