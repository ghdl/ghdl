--  The same 32-bit size overflow as repro.vhd, with nothing to allocate:
--  "big" is 2**30 integers, ie 2**32 bytes, and no object of that subtype
--  is declared.  Only its layout is computed, so the design needs no memory
--  at all and every backend reaches the diagnostic in a few milliseconds.
entity ovf is
  generic (n : natural := 2**30);
end ovf;

architecture a of ovf is
  type row is array (natural range <>) of integer;
  subtype big is row (0 to n - 1);
begin
  process
  begin
    report "elaborated";
    wait;
  end process;
end a;
