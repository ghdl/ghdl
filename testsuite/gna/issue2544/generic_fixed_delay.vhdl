library ieee;
use ieee.std_logic_1164.all;

entity generic_fixed_delay is
    generic(
        NUM_DELAY_CYCLES : natural;     --! Number of delay cycles or registers. Setting this to 0 short-circuits the input with the output.
        type Data_Type_t                  --! The data type of the input and output.
    );
    port(
        clk    : in  std_logic;         --! Clock input
        input  : in  Data_Type_t;         --! Data input
        output : out Data_Type_t          --! Data output
    );
end entity generic_fixed_delay;

architecture rtl of generic_fixed_delay is
begin
    LOGIC_BLOCK_GEN_INST : if NUM_DELAY_CYCLES = 0 generate
        --! Act as short-circuit
        output <= input;
    elsif NUM_DELAY_CYCLES = 1 generate
        name : process is
        begin
            wait until rising_edge(clk);
            --! Output is registered
            output <= input;
        end process name;
    elsif NUM_DELAY_CYCLES > 1 generate
        type Data_Tpe_Arr_t is array (0 to NUM_DELAY_CYCLES - 2) of Data_Type_t;
        signal reg_arr : Data_Tpe_Arr_t;
    begin
        process is
        begin
            --! Delay of NUM_DELAY_CYCLES
            wait until rising_edge(clk);
            reg_arr   <= input & reg_arr(reg_arr'low to reg_arr'high - 1);
            output <= reg_arr(reg_arr'high);
        end process;
    end generate LOGIC_BLOCK_GEN_INST;
end architecture rtl;
