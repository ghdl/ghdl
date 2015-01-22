
entity univ2 is
end entity;

architecture foo of univ2 is

begin
    assert False 
    report "Time'HIGH = " & Time'IMAGE(Time'VAL(Time'POS(Time'HIGH)))
    severity NOTE;
    assert False
    report "should produce 9223372036854775807"
    severity NOTE;
end architecture;

-- 'POS returns universal integer
--  'VAL parameter is any integer type (including universal integer)