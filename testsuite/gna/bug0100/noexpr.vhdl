entity noexpr is
end noexpr;

architecture behav of noexpr is
  signal s : bit_vector (7 downto 0);
begin
  s (7 downto 1) <= ();
end behav;
