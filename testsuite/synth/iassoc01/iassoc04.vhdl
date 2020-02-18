entity riassoc04 is
  port (v : bit_vector (7 downto 0);
        res : out bit);
end riassoc04;

architecture behav of riassoc04 is
begin
  res <= v(0) or v(4);
end behav;

entity iassoc04 is
  port (a, b : bit_vector (3 downto 0);
        res : out bit);
end iassoc04;

architecture behav of iassoc04 is
begin
  inst : entity work.riassoc04
    port map (v (7 downto 4) => a, v (3 downto 0) => b, res => res);
end behav;
