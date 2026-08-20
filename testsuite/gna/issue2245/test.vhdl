use std.env.all;

entity unc_tbase3 is
end entity;

architecture test of unc_tbase3 is

  type mx_ivec is array (natural range <>) of integer;
  type mx_ivec_p is access mx_ivec;
  subtype ints8 is mx_ivec(7 downto 0);

begin

  fill_check: process
    variable ivec : mx_ivec_p;
  begin

    ivec := new ints8;

    report "" & integer'image(ints8'base'element'left);
    report "" & integer'image(ints8'base'element'pos(22));
    report "" & integer'image(ints8'base'element'succ(22));

    finish(0);
  end process fill_check;

end test;
