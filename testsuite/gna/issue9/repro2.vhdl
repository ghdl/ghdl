entity tmp is end entity;
architecture arch of tmp is
    subtype nat2 is natural range 0 to 3;
    signal b : bit;
begin
    with 2 select b <=
     '0' when 0 to 2,
     '1' when 2 to 3;
end architecture;
