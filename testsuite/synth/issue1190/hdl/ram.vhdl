-- A RAM initialized with an external file

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use STD.textio.all;

entity ram is
    port(
        clk_i  : in  std_logic;
        we_i   : in  std_logic;
        addr_i : in  std_logic_vector(5 downto 0);
        data_i : in  std_logic_vector(31 downto 0);
        data_o : out std_logic_vector(31 downto 0)
    );
end ram;

architecture RTL of ram is
    type mem_t is array (0 to 63) of bit_vector(31 downto 0);

    impure function init(filename : in string) return mem_t is
        file     fh  : text is in filename;
        variable l   : line;
        variable mem : mem_t;
    begin
        for i in mem_t'range loop
            readline(fh, l);
            read(l, mem(i));
        end loop;
        return mem;
    end function;

    signal ram : mem_t := init("data/memory.dat");
begin
    memory:
    process(clk_i)
    begin
        if rising_edge(clk_i) then
            if we_i = '1' then
                ram(to_integer(unsigned(addr_i))) <= to_bitvector(data_i);
            end if;
            data_o <= to_stdlogicvector(ram(to_integer(unsigned(addr_i))));
        end if;
    end process;
end architecture RTL;
