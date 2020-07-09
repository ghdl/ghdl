entity repro4b is
  port (b : bit_vector);
end entity;

architecture a of repro4b is
  signal c : b'subtype;
begin
  c <= (others => '0');
end architecture;

entity repro4 is
end entity;

architecture tb of repro4 is
  signal s : bit_vector(7 downto 0);
begin
  DUT: entity work.repro4b
    port map (b => s);
end architecture;

