entity scale3 is
    port(v : in bit_vector(0 to 1);
         w : out bit_vector(0 to 1));
end entity scale3;

architecture boop of scale3 is
begin
    w <= '1' and v;
end architecture boop;
