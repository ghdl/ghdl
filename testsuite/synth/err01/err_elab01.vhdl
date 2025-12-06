entity err_elab01 is
  port (idx : natural;
        o : out bit);
end;

architecture arch of err_elab01 is
  signal mem : bit_vector(7 downto 0) := b"0010_1101";
  impure function compute return bit is
  begin
    return mem(0);
  end compute;
  constant dft : bit := compute;
begin
  o <= mem(idx) when idx < 7 else dft;
end;
