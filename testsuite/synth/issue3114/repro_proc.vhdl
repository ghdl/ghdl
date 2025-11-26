entity sub is
  port (a : bit;
        o : out bit);
end sub;

architecture behav of sub is
  signal t : bit;
begin
  t <= not a;
  o <= t;
end behav;

entity repro is
  generic (gb: boolean := true);
  port (a : bit;
        b : out bit;
        c : out bit);
end repro;

architecture behav of repro is
begin
  dut : entity work.sub port map (a, b);

  blk: block
  begin
  process(all)
    alias t is << signal ^.dut.t : bit>>;
  begin
    c <= t;
  end process;
  end block;
end behav;
