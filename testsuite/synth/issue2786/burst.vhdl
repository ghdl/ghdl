library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity burst_construct is
    Port ( 
           clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           preamble : in STD_LOGIC_VECTOR (255 downto 0); -- 128 QPSK symbols, 2 bits per symbol
           zc : in STD_LOGIC_VECTOR (255 downto 0); -- 128 QPSK symbols, 2 bits per symbol
           data : in STD_LOGIC_VECTOR (4095 downto 0); -- 2048 QPSK symbols, 2 bits per symbol
           chunk_length : in STD_LOGIC_VECTOR (11 downto 0); -- 12-bit integer
           transmit : out STD_LOGIC_VECTOR (255 downto 0); -- 128 QPSK symbols, 2 bits per symbol
           valid : out STD_LOGIC
         );
end burst_construct;

architecture Behavioral of burst_construct is
    type state_type is (IDLE, SEND_PREAMBLE, SEND_ZC, SEND_DATA);
    signal state : state_type;
    signal data_index : unsigned (11 downto 0);
    signal transmit_index : unsigned (7 downto 0);
    signal transmit_buffer : STD_LOGIC_VECTOR (255 downto 0);
    signal transmit_reg : STD_LOGIC_VECTOR (255 downto 0);
    signal valid_reg : STD_LOGIC;
begin
    process(clk)
        variable new_transmit_buffer : STD_LOGIC_VECTOR (255 downto 0);
        variable new_valid : STD_LOGIC;
    begin
        if rising_edge(clk) then
            if reset = '1' then
                state <= IDLE;
                data_index <= (others => '0');
                transmit_index <= (others => '0');
                transmit_buffer <= (others => '0');
                valid_reg <= '0';
                transmit_reg <= (others => '0');
            else
                case state is
                    when IDLE =>
                        if unsigned(chunk_length) /= 0 then
                            state <= SEND_PREAMBLE;
                            transmit_index <= (others => '0');
                            new_transmit_buffer := preamble;
                            new_valid := '1';
                        else
                            new_transmit_buffer := transmit_buffer;
                            new_valid := valid_reg;
                        end if;
                    when SEND_PREAMBLE =>
                        if transmit_index = 127 then
                            state <= SEND_ZC;
                            transmit_index <= (others => '0');
                            new_transmit_buffer := zc;
                        else
                            transmit_index <= transmit_index + 1;
                            new_transmit_buffer := transmit_buffer(247 downto 0) & preamble(255 downto 248);
                        end if;
                        new_valid := '1';
                    when SEND_ZC =>
                        if transmit_index = 127 then
                            state <= SEND_DATA;
                            transmit_index <= (others => '0');
                            data_index <= (others => '0');
                            new_transmit_buffer := data(255 downto 0);
                        else
                            transmit_index <= transmit_index + 1;
                            new_transmit_buffer := transmit_buffer(247 downto 0) & zc(255 downto 248);
                        end if;
                        new_valid := '1';
                    when SEND_DATA =>
                        if data_index = unsigned(chunk_length) - 1 then
                            state <= SEND_ZC;
                            transmit_index <= (others => '0');
                            new_transmit_buffer := zc;
                        else
                            data_index <= data_index + 1;
                            transmit_index <= transmit_index + 1;
                            new_transmit_buffer := transmit_buffer(247 downto 0) & data(255 + to_integer(data_index) * 256 downto 248 + to_integer(data_index) * 256);
                        end if;
                        new_valid := '1';
                end case;
                transmit_buffer <= new_transmit_buffer;
                transmit_reg <= new_transmit_buffer;
                valid_reg <= new_valid;
            end if;
        end if;
    end process;
    transmit <= transmit_reg;
    valid <= valid_reg;
end Behavioral;
