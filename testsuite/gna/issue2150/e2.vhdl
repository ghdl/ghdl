entity e2 is end;

architecture a of e2 is
begin
    ------------------------------------------------------------------------
    -- There is no infinite loop when there is an assertion with boolean 
    -- literal false and severity failiure that is unconditioinally executed
    ------------------------------------------------------------------------
    process
    begin
        assert false severity failure;
    end process;
end;
