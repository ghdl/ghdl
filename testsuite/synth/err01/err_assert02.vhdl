entity err_assert02 is
  generic (idx : natural := 5);
  port (a : bit_vector(3 downto 0); o : out bit);
end;

architecture rtl of err_assert02 is
begin
  process
  begin
    report "try" severity severity_level'val(idx);
    wait;
  end process;
end;
  
