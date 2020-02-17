entity ent2 is
  port (v : out bit_vector (7 downto 0);
        b : in bit);
end ent2;

architecture behav of ent2 is
begin
  v <= (others => b);
end behav;

entity top2 is
end top2;

architecture behav of top2 is
  signal s : bit_vector (7 downto 0);
  signal b : bit;
begin
  dut : entity work.ent2
    port map (
      -- ERROR: missing 1 downto 0!
      v (3 downto 2) => s (3 downto 2),
      v (7 downto 4) => s (7 downto 4),
      b => b);
  b <= '0';
end behav;
