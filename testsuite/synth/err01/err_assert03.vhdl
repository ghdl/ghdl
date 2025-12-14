entity err_assert03 is
  generic (idx : natural := 5);
  port (a : bit_vector(3 downto 0); o : out bit);
end;

architecture rtl of err_assert03 is
begin
  process (a)
  begin
    assert a(idx) = '1';
  end process;
end;
  
