entity repro3b is
  port (b : bit_vector);
end entity;

architecture a of repro3b is
  signal c : b'subtype;
begin
  process
    variable d : c'subtype;
  begin
    d := b;
    d := (others => '0');
    assert d'left = 7;
    assert d'right = 0;
    wait;
  end process;
end architecture;

entity repro3 is
end entity;

architecture tb of repro3 is
  signal s : bit_vector(7 downto 0);
begin
  DUT: entity work.repro3b
    port map (b => s);
end architecture;

