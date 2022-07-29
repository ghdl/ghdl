entity e3 is end;

architecture a of e3 is
begin
    ------------------------------------------------------------------------
    -- There is no infinite loop when a report statement with severity 
    -- failure is unconditionally executed
    ------------------------------------------------------------------------
    process
    begin
        report "" severity failure;
    end process;
end;
