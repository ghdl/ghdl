entity tmp is end entity;
architecture arch of tmp is
    signal b : bit;
begin
    with true select b <=
     '0' when false | true,
     '1' when true;
end architecture;
