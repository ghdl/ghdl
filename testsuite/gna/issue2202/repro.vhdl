entity repro_ch is
  port (a : out bit);
end repro_ch;

architecture syn of repro_ch is
begin
  a <= '1';
end syn;

entity repro is
  port (b : out bit);
end repro;

architecture syn of repro is
  component repro_ch is
    port (a : out bit);
  end component;
begin
  dut: repro_ch
    port map (a => b);
end syn;

configuration repro_cfg of repro is
  for syn
    for dut : repro_ch
      for syn
      end for;
    end for;
  end for;
end configuration;
