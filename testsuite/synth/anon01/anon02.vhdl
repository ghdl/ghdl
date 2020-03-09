entity anon02_sub is
  port (i : bit_vector (7 downto 0);
        o : out bit_vector (7 downto 0));
end anon02_sub;

architecture behav of anon02_sub is
begin
  o <= i xor x"a5";
end behav;

entity anon02 is
  port (i : bit_vector (6 downto 0);
        o : out bit_vector (6 downto 0));
end anon02;

architecture behav of anon02 is
  signal res : bit_vector (7 downto 0);
begin
  dut: entity work.anon02_sub
    port map (i => '0' & i,
              o => res);
  o <= res (6 downto 0);
  gen: for i in 1 to 2 generate
    assert i < 3;
  end generate;
end behav;
