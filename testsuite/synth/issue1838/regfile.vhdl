library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity register_file is
    port(
        clk        : in std_logic;

        write_en   : in std_ulogic;
        write_addr : in std_ulogic_vector(6 downto 0);
        write_data : in std_ulogic_vector(63 downto 0);

        read_addr  : in std_ulogic_vector(6 downto 0);
        read_data  : out std_ulogic_vector(63 downto 0)
    );
end entity register_file;

architecture behaviour of register_file is
    type regfile is array(0 to 127) of std_ulogic_vector(63 downto 0);
    signal registers : regfile;
begin
    -- synchronous writes
    register_write_0: process(clk)
    begin
        if rising_edge(clk) then
            if write_en = '1' then
                registers(to_integer(unsigned(write_addr))) <= write_data;
            end if;
        end if;
    end process register_write_0;

    -- asynchronous reads
    register_read_0: process(all)
    begin
        read_data <= registers(to_integer(unsigned(read_addr)));
    end process register_read_0;

end architecture behaviour;
