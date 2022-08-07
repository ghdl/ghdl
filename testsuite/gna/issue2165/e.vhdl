entity e is end; 
architecture a of e is

    -- Body mismatches declaration because 'arg' is not marked constant. 
    -- However constant is the default value when not specified
    function f1(constant arg : bit) return bit;
    function f1(         arg : bit) return bit
    is
    begin
        return '0';
    end;
begin 
end;
