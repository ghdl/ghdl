entity repro is
end repro;

architecture behaviour of repro is
    signal selector : bit_vector(1 downto 0) := "10";
    signal result   : bit_vector(7 downto 0);

    signal op_1     : bit_vector(7 downto 0);
    signal op_2     : bit_vector(7 downto 0);
begin
    with selector select
        result <= op_1 and op_2 when "00",
                  (others => '0') when others;
end behaviour;
