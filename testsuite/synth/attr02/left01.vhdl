entity left01 is
  generic (l : natural := 3);
  port (i : integer;
        o : out integer);
end left01;

architecture behav of left01 is
  subtype myint is natural range l to natural'right;
  signal s : myint;
begin
  o <= 0 when i < myint'left else i;
end behav;
