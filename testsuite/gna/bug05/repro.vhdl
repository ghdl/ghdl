entity sub is
  generic (l : natural);
  port (a : out bit;
        b : bit_vector (0 to 3);
        c : bit_vector (0 to l - 1));
end sub;

architecture behav of sub is
begin
  a <= b (0) xor c (0);
end behav;

entity tb is
end tb;

architecture behav of tb is
  signal a : bit;
  signal b: bit_vector (0 to 3);
  signal c: bit_vector (0 to 7);
begin
  my_sub: entity work.sub
    generic map (l => c'length)
    port map (a => a, b => b, c => c);

  process
  begin
    wait for 1 ns;
    assert a = '0';
    b <= x"0";
    c <= x"80";
    wait for 1 ns;
    assert a = '1';
    wait;
  end process;
end behav;
