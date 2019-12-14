entity test_arr is
end entity;

architecture behavior of test_arr is
    type T is array(natural range <>) of bit_vector;
    signal a: T(0 to 1)(0 to 1) := ("00", "11");
begin
end architecture;
