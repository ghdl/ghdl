entity bug7 is
end entity bug7;

architecture x of bug7 is

  constant cst : real := 5.5;
  signal test : integer;

begin

  test <= cst;

end architecture x;
