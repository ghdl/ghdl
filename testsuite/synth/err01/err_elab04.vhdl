entity err_elab04 is
  port (idx : natural;
        o : out bit);
end;

architecture arch of err_elab04 is
  type my_rec is record
    v : bit_vector(7 downto 0);
  end record;
  signal mem : my_rec;
  constant dft : bit := mem.v(0);
begin
  o <= mem.v(idx) when idx < 7 else dft;
end;
