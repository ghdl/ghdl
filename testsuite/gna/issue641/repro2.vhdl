entity repro2b is
  port (b : bit_vector);
end entity;

architecture a of repro2b is
  signal c, d : b'subtype;
begin
  c <= b;
  d <= b;
end architecture;

entity repro2 is
end entity;

architecture tb of repro2 is
  signal s : bit_vector(7 downto 0);
begin
  DUT: entity work.repro2b
    port map (b => s);
end architecture;

