entity test is
end entity test;

entity internal is
end entity internal;
architecture arch of internal is begin
end architecture arch;

architecture first of test is begin
  test_instantiation : entity work.internal;
end architecture first;

architecture second of test is begin
end architecture second;

