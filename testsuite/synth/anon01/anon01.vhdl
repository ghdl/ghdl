entity anon01_sub is
  port (i : bit_vector (7 downto 0);
        o : out bit_vector (7 downto 0));
end anon01_sub;

architecture behav of anon01_sub is
begin
  o <= i xor x"a5";
end behav;

entity anon01 is
  port (i : bit_vector (6 downto 0);
        o : out bit_vector (6 downto 0));
end anon01;

architecture behav of anon01 is
  signal res : bit_vector (7 downto 0);
begin
  dut: entity work.anon01_sub
    port map (i => '0' & i,
              o => res);
  o <= res (6 downto 0);
end behav;
