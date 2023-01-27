entity test is
    port(
        a: in bit_vector(7 downto 0);
        b: in bit;
        c: out bit_vector(7 downto 0));
end test;

architecture behavior of test is
begin
    c <= a and b;
end behavior;
