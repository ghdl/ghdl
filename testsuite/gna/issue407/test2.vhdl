entity test2 is
begin
end entity;

architecture arch of test2 is
begin
        process(all)
        begin
                loop
                        exit;
                end loop;     
        end process;
end architecture;
