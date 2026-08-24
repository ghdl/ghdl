library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo is
generic(
    DEPTH    : integer := 256;
    WDTH     : integer := 16;
    TRESHOLD : integer := 32
);
port(
    reset   : in std_logic;
    clk     : in std_logic;

    data_in    : in std_logic_vector(WDTH-1 downto 0);
    data_out   : out std_logic_vector(WDTH-1 downto 0);
    empty      : out std_logic;
    full       : out std_logic;
    half_full  : out std_logic;
    wr         : in std_logic;
    rd         : in std_logic
);
end entity;

architecture ar_fifo of fifo is
    type rcells_memory_t is array(0 to DEPTH-1) of std_logic_vector(WDTH-1 downto 0);
    signal rcells_memory : rcells_memory_t;
    signal write_ptr  : integer range 0 to DEPTH-1 :=0;
    signal read_ptr   : integer range 0 to DEPTH-1 :=0;
    signal data_count : integer :=0;
    signal full_sig   : std_logic :='0';
    signal empty_sig   : std_logic :='0';
begin

    empty <= empty_sig;
    full  <= full_sig;
    half_full  <= '1' when data_count >= TRESHOLD else '0';
    empty_sig <= '1' when data_count = 0 else '0';
    full_sig <= '1' when data_count = DEPTH-1 else '0';

    process(clk, reset)
    begin
        if reset = '0' then
            data_count <= 0;
        elsif clk'event and clk = '1' then
            if wr = '1' and rd = '0'  and data_count /= DEPTH then
                data_count <= data_count + 1;
            elsif rd = '1' and wr = '0' and data_count /= 0 then
                data_count <= data_count - 1;
            end if;
        end if;
    end process;


    process(clk, reset)
    begin
        if reset = '0' then
            write_ptr <= 0;
        elsif clk'event and clk = '1' then
            if wr = '1' and full_sig = '0' then
                write_ptr <= (write_ptr + 1) mod (DEPTH-1);
            end if;
        end if;
    end process;
    rcells_memory(write_ptr) <= data_in;

    process(clk, reset)
    begin
        if reset = '0' then
            read_ptr <= 0;
        elsif clk'event and clk = '1' then
            if rd = '1' and empty_sig = '0' then
                read_ptr <= (read_ptr + 1) mod (DEPTH-1);
            end if;
        end if;
    end process;
    data_out <= rcells_memory(read_ptr);

end architecture ar_fifo;
