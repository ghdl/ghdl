entity err_elab02 is
  port (idx : natural;
        o : out bit);
end;

architecture arch of err_elab02 is
  signal mem : bit_vector(7 downto 0) := b"0010_1101";
  constant dft : bit := mem(0);
begin
  o <= mem(idx) when idx < 7 else dft;
end;
