entity repro is
end entity repro;

architecture TB of repro is

begin

    DM: process
        type t_ram is array(natural range <>) of bit_vector;
        type p_ram is access t_ram;
        variable myram : p_ram;
    begin
        myram := new t_ram(0 to 31)(15 downto 0);
        for i in myram'range loop
            myram(i) := (others => '0');
        end loop;
        wait;
    end process DM;
    
end architecture TB;
