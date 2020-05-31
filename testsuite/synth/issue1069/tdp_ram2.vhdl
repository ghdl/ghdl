library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity tdp_ram2 is
    generic (
        ADDRWIDTH_A : positive := 12;
        WIDTH_A     : positive := 8;

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
        data_write_a : in  std_logic_vector(WIDTH_A - 1 downto 0);

        clk_b        : in  std_logic;
        read_b       : in  std_logic;
        write_b      : in  std_logic;
        byteen_b     : in  std_logic_vector(WIDTH_B/COL_WIDTH - 1 downto 0);
        addr_b       : in  std_logic_vector(ADDRWIDTH_B - 1 downto 0);
        data_read_b  : out std_logic_vector(WIDTH_B - 1 downto 0);
        data_write_b : in  std_logic_vector(WIDTH_B - 1 downto 0)
    );
end tdp_ram2;

architecture behavioral of tdp_ram2 is
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
    constant COLS_B : positive := WIDTH_B / COL_WIDTH;

    constant TOTAL_COLS : positive := eq_assert(COLS_A * 2 ** ADDRWIDTH_A, COLS_B * 2 ** ADDRWIDTH_B);

    constant EXTRA_ADDR_BITS_A : natural := log2(COLS_A);
    constant EXTRA_ADDR_BITS_B : natural := log2(COLS_B);

    signal reg_a : std_logic_vector(WIDTH_A - 1 downto 0);
    signal reg_b : std_logic_vector(WIDTH_B - 1 downto 0);
begin
    assert WIDTH_A mod COL_WIDTH = 0 and
           WIDTH_B mod COL_WIDTH = 0 and
           2 ** (ADDRWIDTH_A + EXTRA_ADDR_BITS_A) = TOTAL_COLS and
           2 ** (ADDRWIDTH_B + EXTRA_ADDR_BITS_B) = TOTAL_COLS
        report "Both WIDTH_A and WIDTH_B have to be a power-of-two multiple of COL_WIDTH"
        severity failure;

    process(clk_a, clk_b)
      type ram_t is array(0 to TOTAL_COLS - 1) of std_logic_vector(COL_WIDTH - 1 downto 0);
      variable store : ram_t := (others => (others => '0'));

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

        if rising_edge(clk_b) then
            for i in 0 to COLS_B - 1 loop
                if write_b = '1' and byteen_b(i) = '1' then
                    store(to_integer(unsigned(addr_b) & to_unsigned(i, EXTRA_ADDR_BITS_B))) :=
                        data_write_b((i+1) * COL_WIDTH - 1 downto i * COL_WIDTH);
                end if;

                if read_b = '1' then
                    reg_b((i+1) * COL_WIDTH - 1 downto i * COL_WIDTH) <=
                        store(to_integer(unsigned(addr_b) & to_unsigned(i, EXTRA_ADDR_BITS_B)));
                end if;
            end loop;

            data_read_b <= reg_b;
        end if;
    end process;
end behavioral;
