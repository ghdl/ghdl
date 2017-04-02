entity bug8 is
end entity bug8;

architecture x of bug8 is

  type bit_position is (msb);
  signal test : real;

begin

  test <= msb;

end architecture x;
