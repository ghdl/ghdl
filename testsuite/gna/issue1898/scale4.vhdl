entity scale4 is
    port(b : in bit;
         w : out bit_vector(0 to 1));
end entity scale4;

architecture boop of scale4 is
begin
    w <= b and "01";
end architecture boop;
