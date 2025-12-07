entity convarr03 is
  port (a : bit_vector(3 downto 0);
        q : out bit_vector(3 downto 0));
end;

architecture behav of convarr03 is
  type arr2d is array (natural range <>, natural range <>) of bit;
  signal s, s1: arr2d(0 to 1, 0 to 1);
begin
  s <= ((a(3), a(2)), (a(1), a(0)));
  s1 <= arr2d(s);
  q <= (s(0,0),s(1,0), s(0,1), s(1,1));
end;
