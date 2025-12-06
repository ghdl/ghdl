entity err_idx02 is
  generic (idx : natural := 4);
  port (o : out bit);
end;

architecture behav of err_idx02 is
  constant c : bit_vector(3 downto 0) := x"5";
begin
  o <= c(idx);
end;
