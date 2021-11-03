entity scale is
    port(w : out bit_vector(0 to 1));
end entity scale;

architecture boop of scale is
begin
    w <= '1' and b"01";
end architecture boop;
