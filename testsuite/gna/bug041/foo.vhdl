entity foo is
end entity;

architecture fum of foo is
    signal a:  bit_vector (1 to 1);
    signal b:  bit_vector (1 to 1);
begin
    
    a(1 to 1) <= b(1);
    
end architecture;
