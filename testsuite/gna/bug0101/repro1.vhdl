entity repro1 is
end repro1;

architecture behav of repro1 is
  signal sig : bit_vector (7 downto 0);
begin
  g : for i in sig1'range generate
    sig (i) <= sig (i) and '1';
  end generate;
end behav;
