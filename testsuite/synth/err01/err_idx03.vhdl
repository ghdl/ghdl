entity err_idx03 is
  generic (idx : natural := 4);
  port (i : in bit_vector(3 downto 0);
        o : out bit);
end;

architecture behav of err_idx03 is
begin
  o <= i(idx);
end;
