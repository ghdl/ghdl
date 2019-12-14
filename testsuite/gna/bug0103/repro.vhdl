entity repro is
end repro;

entity buf is
  port (i : bit; o : out bit);
end buf;

architecture behav of buf is
begin
  o <= i;
end behav;

architecture behav of repro is
  signal a, b : bit;
  signal r : bit;
begin
  dut: entity work.buf port map (i => a xor b, o => r);
  process
  begin
    a <= '0';
    b <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    wait;
  end process;
end behav;
