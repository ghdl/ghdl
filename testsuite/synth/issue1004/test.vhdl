library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity test is
    generic(
        MEMORY_SIZE   : natural := 4096;
        RAM_INIT_FILE : string := "firmware.hex"
        );
end entity test;

architecture behaviour of test is
    type ram_t is array(0 to (MEMORY_SIZE / 8) - 1) of std_logic_vector(63 downto 0);

    impure function init_ram(name : STRING) return ram_t is
        file ram_file : text open read_mode is name;
        variable ram_line : line;
        variable temp_word : std_logic_vector(63 downto 0);
        variable temp_ram : ram_t := (others => (others => '0'));
    begin
        for i in 0 to (MEMORY_SIZE/8)-1 loop
            exit when endfile(ram_file);
            readline(ram_file, ram_line);
            report "read: " & ram_line.all;
            hread(ram_line, temp_word);
            temp_ram(i) := temp_word;
        end loop;

        return temp_ram;
    end function;

    signal memory : ram_t := init_ram(RAM_INIT_FILE);
begin
end architecture behaviour;
