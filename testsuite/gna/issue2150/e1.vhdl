entity e1 is end;

architecture a of e1 is
begin

    ------------------------------------------------------------------------
    -- There is no infinite loop when std.env.stop is unconditionally executed
    ------------------------------------------------------------------------
    process
    begin
        std.env.stop;
    end process;
end;
