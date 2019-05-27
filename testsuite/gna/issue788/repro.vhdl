entity repro is
end repro;

architecture behav of repro is
  type word_vector is array (natural range <>) of bit_vector (7 downto 0);
  
  type trans is record
    header : natural;
    bod : word_vector;
  end record;

  signal s : trans (bod(0 to 3));

  procedure check (t1 : trans) is
  begin
    assert t1.header = 0;
  end check;

  procedure check2 (signal t : trans) is
  begin
    check (t);
  end check2;
begin
  process
  begin
    check2 (s);
    wait;
  end process;
end behav;
