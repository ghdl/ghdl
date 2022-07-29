entity e is end;

architecture a of e is
begin

    ------------------------------------------------------------------------
    -- There is no infinite loop when std.env.stop is unconditionally executed
    ------------------------------------------------------------------------
    process
    begin
        std.env.stop;
    end process;

    ------------------------------------------------------------------------
    -- There is no infinite loop when there is an assertion with boolean 
    -- literal false and severity failiure that is unconditioinally executed
    ------------------------------------------------------------------------
    process
    begin
        assert false severity failure;
    end process;

    ------------------------------------------------------------------------
    -- There is no infinite loop when a report statement with severity 
    -- failure is unconditionally executed
    ------------------------------------------------------------------------
    process
    begin
        report "" severity failure;
    end process;
end;
