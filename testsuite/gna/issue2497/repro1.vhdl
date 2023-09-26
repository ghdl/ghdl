entity repro1 is
end;

architecture behav of repro1 is
  function f return natural is
  begin
    return 24;
  end;

  constant w : natural := f;
  constant z : bit_vector := (w -1 downto 0 => '0');
begin
  process
  begin
    assert z'length = 24 severity failure;
    wait;
  end process;
end behav;
