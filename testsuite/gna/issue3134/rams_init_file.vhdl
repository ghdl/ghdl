-- Initializing Block ram from external data file
-- File: rams_init_file.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity rams_init_file is
    generic (
        AW : positive := 6;
        DW : positive := 32
    );
    port(
        clk  : in  std_logic;
        ce   : in  std_logic;
        we   : in  std_logic;
        addr : in  std_logic_vector(AW-1 downto 0);
        din  : in  std_logic_vector(DW-1 downto 0);
        dout : out std_logic_vector(DW-1 downto 0)
    );
end rams_init_file;

architecture rtl of rams_init_file is
    type RamType is array (natural range <>) of bit_vector(DW-1 downto 0);

    impure function InitRamFromFile(RamFileName : in string) return RamType is
        FILE RamFile : text;
        variable RamFileLine : line;
        variable ram : RamType(0 to 2**AW-1);
    begin
        file_open(RamFile, RamFileName, READ_MODE);
        for i in ram'range loop
            readline(RamFile, RamFileLine);
            read(RamFileLine, ram(i));
        end loop;
        deallocate(RamFileLine);
        return ram;
        file_close(RamFile);
    end function;

    procedure DumpRamToFile(RamFileName : in string; ram : RamType) is
        FILE RamFile : text;
        variable RamFileLine : line;
    begin
        file_open(RamFile, RamFileName, WRITE_MODE);
        for i in ram'range loop
            write(RamFileLine, ram(i));
            writeline(RamFile, RamFileLine);
        end loop;
        deallocate(RamFileLine);
        file_close(RamFile);
    end procedure DumpRamToFile;

--    signal ram : RamType(0 to 2**AW-1) := InitRamFromFile("rams_init_file.data");
    signal ram : RamType(0 to 2**AW-1) := InitRamFromFile("missing.data");
begin

    dump : process
    begin
        -- delta cycle avoids race condition with initialization
        wait for 0 ns;
        DumpRamToFile("rams_dump_file.data", ram);
        wait;
    end process dump;

    rw: process(clk)
    begin
        if clk'event and clk = '1' then
            if ce = '1' then
                if we = '1' then
                    ram(to_integer(unsigned(addr))) <= to_bitvector(din);
                else
                    dout <= to_stdlogicvector(ram(to_integer(unsigned(addr))));
                end if;
            end if;
        end if;
    end process rw;

end rtl;
