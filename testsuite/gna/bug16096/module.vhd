entity module is end entity;
architecture arch of module is
    function func(a:natural) return natural is
    begin
        if    a=32 then return 2;
        elsif a=16 then return 1;
        else            return 0;
    end if;
    end function;
    constant DATAPATH :natural := 32;
    constant LSLICES  :natural := func(DATAPATH);
--  constant LSLICES  :natural := 2;
    signal a :bit;
    signal s :bit_vector(LSLICES-1 downto 0);
begin
    DATA8: if DATAPATH=8 generate
        a <= '0';
    end generate;
--  DATA16: if DATAPATH=16 generate
--      with s select a <=
--          '1' when "0",
--          '0' when others;
--  end generate;
    DATA32: if DATAPATH=32 generate
        with s select a <=
            '1' when "00",
            '0' when "01",
            '1' when "10",
            '0' when others;
    end generate;
end architecture;
