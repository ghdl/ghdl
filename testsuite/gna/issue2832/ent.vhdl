entity ent is
end entity;

architecture a of ent is
    signal sig : integer := 0;
    begin
        process is
        begin
            while 1 = 1 loop
                report "expect overflow";
                wait on sig for 1000 ms;
            end loop;
        end process;
end architecture;
