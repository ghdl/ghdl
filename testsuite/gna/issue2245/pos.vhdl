use std.env.all;

entity unc_tbase3_pos is
end entity;

architecture test of unc_tbase3_pos is

  type mx_ivec is array (natural range <>) of integer;
  subtype ints8 is mx_ivec(7 downto 0);
  type mx_mat is array (natural range <>) of ints8;
  subtype mat2 is mx_mat(1 downto 0);

begin

  chk: process
    --  'BASE'ELEMENT denotes a type, so it is a legal type mark.
    variable v : ints8'base'element;
  begin

    --  The LRM's own example of the only legal use of 'BASE, on a scalar.
    report integer'image(integer'base'high);

    --  Same attribute without 'BASE, as a control.
    report integer'image(ints8'element'succ(10));

    report integer'image(ints8'base'element'succ(20));
    report integer'image(mat2'base'element'element'succ(30));
    report integer'image(integer'base'base'succ(40));

    v := 55;
    report integer'image(v);

    finish(0);
  end process chk;

end test;
