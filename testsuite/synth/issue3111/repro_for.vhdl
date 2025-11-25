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
  port (a : bit_vector(7 downto 0);
        b : out bit_vector(7 downto 0));
end repro;

architecture behav of repro is
  signal t : bit_vector(7 downto 0);
begin
  gen: for i in a'range generate
    dut : entity work.sub port map (a(i), b(i));
    t(i) <= << signal ^.gen(i).dut.t : bit>>;
  end generate;
end behav;
