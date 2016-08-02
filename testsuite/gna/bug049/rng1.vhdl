use std.textio.all;

entity sliding_index is
end entity;

architecture foo of sliding_index is
    type integer_vector is array (natural range <>) of integer;
    
    function to_string(inp: integer_vector) return string is
       variable retn: line;
    begin
        for i in inp'range loop
            if i = inp'RIGHT then
                write (retn, integer'image(inp(i)));
            else 
                write (retn, integer'image(inp(i)) & ',');
            end if;
        end loop;
        return retn(1 to retn'length);  -- the string value of the line
    end function;

    constant ivec: integer_vector := (1,2,3,4,5,6,7);
    signal sum:    integer_vector (ivec'range);
    -- signal sum:     integer_vector (0 to 6);
    
begin
    
    sum <= (
                    0 => (          ivec(0) + ivec(1)),
                    1 => (ivec(0) + ivec(1) + ivec(2)),
                    2 => (ivec(1) + ivec(2) + ivec(3)),
                    3 => (ivec(2) + ivec(3) + ivec(4)),
                    4 => (ivec(3) + ivec(4) + ivec(5)),
                    5 => (ivec(4) + ivec(5) + ivec(6)),
                    6 => (ivec(5) + ivec(6)          )
           );

    process
    begin
        wait for 0 ns;
        report "ivec = " & to_string(ivec);
        report "sum = " & to_string(sum);
        wait;
    end process;
end architecture;
