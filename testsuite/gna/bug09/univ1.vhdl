entity univ1 is
end entity;

architecture foo of univ1 is
    signal i:   integer := Time'POS(Time'High);  -- should produce error
    
    -- 6.4.2.3 Signal declarations, para 5, first sentence:
    -- If the signal declaration includes the assignment symbol followed by an
    --  expression, it shall be of the same type as the signal. Such an
    --  expression is said to be a default expression.
    --
    -- -1993 4.3.1.2 Signal declarations, para 6, first sentence:
    -- If the signal declaration includes the assignment symbol followed by
    --  an expression, it must be of the same type as the signal.
    
    -- 'POS returns universal integer
    -- 
begin
    assert False 
    report "i = " & integer'IMAGE(i)  -- shows left clipping instead of error
    severity ERROR;
end architecture;