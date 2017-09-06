entity test is
begin
end entity;

architecture arch of test is
begin
        process(all)
        begin
                report "compilation crashes here";
        end process;
end architecture;
