library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity ram41 is
    generic (
        ADDRWIDTH : positive := 7;
        WIDTH     : positive := 8
    );
    port (
        clk_a        : in  std_logic;
        read_a       : in  std_logic;
        write_a      : in  std_logic;
        addr_a       : in  std_logic_vector(ADDRWIDTH - 1 downto 0);
        data_read_a  : out std_logic_vector(WIDTH - 1 downto 0);
        data_write_a : in  std_logic_vector(WIDTH - 1 downto 0);

        clk_b        : in  std_logic;
        read_b       : in  std_logic;
        write_b      : in  std_logic;
        addr_b       : in  std_logic_vector(ADDRWIDTH - 1 downto 0);
        data_read_b  : out std_logic_vector(WIDTH - 1 downto 0);
        data_write_b : in  std_logic_vector(WIDTH - 1 downto 0)
    );
end ram41;

architecture behavioral of ram41 is
begin
    process(clk_a, clk_b)
      type ram_t is array(0 to 2**ADDRWIDTH - 1) of std_logic_vector(WIDTH - 1 downto 0);
      variable store : ram_t := (others => (others => '0'));

    begin
        if rising_edge(clk_a) then
          if read_a = '1' then
            data_read_a <= store(to_integer(unsigned(addr_a)));
          end if;

          if write_a = '1' then
            store(to_integer(unsigned(addr_a))) := data_write_a;
          end if;
        elsif rising_edge(clk_b) then
          if read_b = '1' then
            data_read_b <= store(to_integer(unsigned(addr_b)));
          end if;

          if write_b = '1' then
            store(to_integer(unsigned(addr_b))) := data_write_b;
          end if;
        end if;
    end process;
end behavioral;
