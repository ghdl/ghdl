entity repro1_gen is
  generic(type tyi);
  port(i1  : in tyi;
       po  : out tyi);
end entity;

architecture bhv of repro1_gen is
begin
  po <= i1;
end bhv;

entity repro1 is
end entity;

architecture bhv of repro1 is
  signal is1 : bit_vector(7 downto 0) := "00111100";
  signal cout2 : bit_vector(7 downto 0);
begin
  dut2: entity work.repro1_gen
    generic map(tyi => bit_vector)
    port map(
      i1  => is1,
      po  => cout2);
end bhv;

