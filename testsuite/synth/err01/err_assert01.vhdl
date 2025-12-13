entity err_assert01 is
  generic (idx : natural := 5);
  port (a : bit_vector(3 downto 0); o : out bit);
end;

architecture rtl of err_assert01 is
begin
  assert a(idx) = '1';
end;
  
