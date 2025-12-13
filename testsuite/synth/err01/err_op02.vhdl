entity err_op02 is
  generic (idx : natural := 5);
  port (a : bit_vector(3 downto 0); o : out bit);
end;

architecture rtl of err_op02 is
begin
  o <= not a(idx);
end;
  
