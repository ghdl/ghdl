entity scale2 is
    port(v : in bit_vector(0 to 1);
         a : in bit;
         w : out bit_vector(0 to 1));
end entity scale2;

architecture boop of scale2 is
begin
    w <= a and v;
end architecture boop;
