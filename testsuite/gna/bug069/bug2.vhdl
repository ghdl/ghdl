entity bug2 is
end entity bug2;

architecture x of bug2 is

  type bit_position is (msb);
  signal test : integer;

begin

  test <= msb;

end architecture x;
