entity test1 is
begin
end entity;

architecture arch of test1 is
begin
        process(all)
        begin
                assert false report "compilation crashes here";
                assert false;
        end process;
end architecture;
