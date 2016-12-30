entity repro1 is
  generic (c : natural := 4);
end repro1;

architecture behav of repro1 is
  constant cmap : string (1 to 5) :=
    (1 => 'a', 2 => 'b', 3 => 'c', 4 => 'd', 5 => 'e');
begin
  process
    variable v : character;
  begin
    v := cmap (c);
    assert v = 'd' report "bad value" severity error;
    wait;
  end process;
end behav;
