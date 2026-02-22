entity test is
end entity ;

architecture arch of test is

    signal clock : bit ;
    alias aclock is clock ;

begin

    tb : process
    begin
        report clock'instance_name ;
        report aclock'instance_name ;
        wait;
    end process ;

end architecture ;
