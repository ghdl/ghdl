entity repro1b is
  port (b : bit_vector);
end entity;

architecture a of repro1b is
  signal c : b'subtype;
begin
  c <= b;
  assert c'left = 7;
  assert c'right = 0;
end architecture;

entity repro1 is
end entity;

architecture tb of repro1 is
  signal s : bit_vector(7 downto 0);
begin
  DUT: entity work.repro1b
    port map (b => s);
end architecture;

