entity repro1b is
  port (inp : bit);
end;

architecture behav of repro1b is
begin
end;

entity repro1 is
  generic (p : natural := 5);
end;

architecture behav of repro1 is
  function mix (a : natural) return natural is
  begin
    return 0;
  end mix;

  signal s : bit_vector (0 to 5) := "100000";
begin
  dut: entity work.repro1b
    port map (inp => s (mix (p)));
end behav;
