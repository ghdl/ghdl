entity err_idx01 is
  generic (idx : natural := 4);
  port (o : out bit);
end;

architecture behav of err_idx01 is
  constant c : bit_vector(0 to 3) := x"5";
begin
  o <= c(idx);
end;
