entity unc_tbase3_err is
end entity;

architecture test of unc_tbase3_err is

  type mx_ivec is array (natural range <>) of integer;
  subtype ints8 is mx_ivec(7 downto 0);
  signal sig : ints8;

  --  LRM08 16.2: 'BASE is allowed only as the prefix of the name of another
  --  attribute, so this is illegal and must be reported as such.
  signal s : ints8'base;

begin

  chk: process
  begin

    --  Legal use of 'BASE, but the base type of an array subtype is
    --  unconstrained, so 'LEFT is not defined on it.
    report integer'image(ints8'base'left);

    --  The prefix of 'BASE must denote a type.
    report integer'image(sig'base'element'left);

    --  'BASE is never a value.
    report integer'image(integer'base);

    wait;
  end process chk;

end test;
