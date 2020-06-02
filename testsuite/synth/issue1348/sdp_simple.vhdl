library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity sdp_simple is
    port (
        clk          : in  std_logic;

        read_a       : in  std_logic;
        write_a      : in  std_logic;
        byteen_a     : in  std_logic_vector(0 downto 0);
        addr_a       : in  std_logic_vector(11 downto 0);
        data_read_a  : out std_logic_vector(7 downto 0);
        data_write_a : in  std_logic_vector(7 downto 0);

        read_b       : in  std_logic;
        write_b      : in  std_logic;
        byteen_b     : in  std_logic_vector(3 downto 0);
        addr_b       : in  std_logic_vector(9 downto 0);
        data_read_b  : out std_logic_vector(31 downto 0);
        data_write_b : in  std_logic_vector(31 downto 0)
    );
end sdp_simple;

architecture behavioral of sdp_simple is
    type ram_t is array(0 to 4095) of std_logic_vector(7 downto 0);
    signal store : ram_t := (others => (others => '0'));
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if write_a = '1' and byteen_a(0) = '1' then
                store(to_integer(unsigned(addr_a))) <= data_write_a;
            end if;

            if read_a = '1' then
                data_read_a <= store(to_integer(unsigned(addr_a)));
            end if;

            for i in 0 to 3 loop
                if write_b = '1' and byteen_b(i) = '1' then
                    store(to_integer(unsigned(addr_b) & to_unsigned(i, 2))) <=
                        data_write_b((i+1) * 8 - 1 downto i * 8);
                end if;

                if read_b = '1' then
                    data_read_b((i+1) * 8 - 1 downto i * 8) <=
                        store(to_integer(unsigned(addr_b) & to_unsigned(i, 2)));
                end if;
            end loop;
        end if;
    end process;
end behavioral;
