entity sig is
end;

architecture behav of sig is
  signal s : natural;
  signal last : time;
begin

  s <= 1 after 20 ns;

  b: block
    port (q : boolean);
    port map (q => s'quiet(10 ns));
  begin
    process (q)
    begin
      report "q is " & boolean'image (q);
      last <= now;
    end process;
  end block;

  process
  begin
    wait for 100 ns;
    assert last = 30 ns severity failure;
    wait;
  end process;
end;
