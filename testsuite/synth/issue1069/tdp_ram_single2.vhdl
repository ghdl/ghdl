library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity tdp_ram is
    generic (
        ADDRWIDTH_A : positive := 10;
        WIDTH_A     : positive := 32;

        ADDRWIDTH_B : positive := 10;
        WIDTH_B     : positive := 32;

        COL_WIDTH : positive := 8
    );
    port (
        clk_a        : in  std_logic;
        read_a       : in  std_logic;
        write_a      : in  std_logic;
        byteen_a     : in  std_logic_vector(WIDTH_A/COL_WIDTH - 1 downto 0);
        addr_a       : in  std_logic_vector(ADDRWIDTH_A - 1 downto 0);
        data_read_a  : out std_logic_vector(WIDTH_A - 1 downto 0);
        data_write_a : in  std_logic_vector(WIDTH_A - 1 downto 0)
    );
end tdp_ram;

architecture behavioral of tdp_ram is
    function log2(val : INTEGER) return natural is
        variable res : natural;
    begin
        for i in 0 to 31 loop
            if (val <= (2 ** i)) then
                res := i;
                exit;
            end if;
        end loop;

        return res;
    end function log2;

    function eq_assert(x : integer; y : integer) return integer is
    begin
        assert x = y;
        return x;
    end function eq_assert;
    
    constant COLS_A : positive := WIDTH_A / COL_WIDTH;

    constant TOTAL_COLS : positive := COLS_A * 2 ** ADDRWIDTH_A;

    constant EXTRA_ADDR_BITS_A : positive := log2(COLS_A);

    type ram_t is array(0 to TOTAL_COLS - 1) of std_logic_vector(COL_WIDTH - 1 downto 0);
    shared variable store : ram_t := (others => (others => '0'));

    signal reg_a : std_logic_vector(WIDTH_A - 1 downto 0);
begin
    assert WIDTH_A mod COL_WIDTH = 0 and
           2 ** (ADDRWIDTH_A + EXTRA_ADDR_BITS_A) = TOTAL_COLS
        report "WIDTH_A have to be a power-of-two multiple of COL_WIDTH"
        severity failure;

    process(clk_a)
    begin
        if rising_edge(clk_a) then
            for i in 0 to COLS_A - 1 loop
                if write_a = '1' and byteen_a(i) = '1' then
                    store(to_integer(unsigned(addr_a) & to_unsigned(i, EXTRA_ADDR_BITS_A))) :=
                        data_write_a((i+1) * COL_WIDTH - 1 downto i * COL_WIDTH);
                end if;

                if read_a = '1' then
                    reg_a((i+1) * COL_WIDTH - 1 downto i * COL_WIDTH) <=
                        store(to_integer(unsigned(addr_a) & to_unsigned(i, EXTRA_ADDR_BITS_A)));
                end if;
            end loop;

            data_read_a <= reg_a;
        end if;
    end process;
end behavioral;
