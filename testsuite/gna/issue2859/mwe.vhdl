entity mwe is
end entity;

architecture test of mwe is
    constant VAL : bit_vector(1 downto 0) := "00";
begin
    GenCase : case VAL generate
        when "00" =>
        when others =>
    end generate;
end architecture;

