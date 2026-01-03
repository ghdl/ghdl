entity length01 is
  port (inp : bit;
        q : out bit);
end;

architecture behav of length01 is
  type my_int is range 1023 downto 0;
  type my_bv is array(my_int range <>) of bit;

  function get_2 (a : my_bv) return my_bv is
  begin
    return a & a;
  end get_2;

  signal s : my_bv(3 downto 0);
begin
  s <= get_2("") & "000" & inp;
  q <= s(3);
end behav;
