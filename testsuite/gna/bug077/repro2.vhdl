entity child2 is
  port (i : bit_vector);
end;

architecture behav of child2 is
begin
  assert i = "10";
end behav;

entity repro2 is
end repro2;

architecture behav of repro2 is
  signal s : bit_vector (7 downto 0);
begin
  inst : entity work.child2
    port map(
      i(0) => s(1),
      i(1) => s(0));

  process
  begin
    s <= x"01";
    wait;
  end process;
end;
