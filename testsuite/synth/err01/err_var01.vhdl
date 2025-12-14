entity err_var01 is
  generic (idx : natural := 4);
  port (a : bit; o : out bit);
end;

architecture behav of err_var01 is
  constant c : bit_vector(0 to 3) := x"5";
begin
  process (a)
    variable v : bit := c(idx);
  begin
    o <= a xor v;
  end process;
end;
