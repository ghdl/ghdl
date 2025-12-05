entity right01 is
  generic (r : natural := 23);
  port (i : integer;
        o : out integer);
end right01;

architecture behav of right01 is
  subtype myint is integer range -100 to r;
  signal s : myint;
begin
  s <= 0 when i > s'subtype'right else i;

  o <= s;
end behav;
