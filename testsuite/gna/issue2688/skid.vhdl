library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity skid_buffer_in is
    port (
        aclk    : in  std_logic;
        aresetn : in  std_logic;
        r_valid : in  std_logic;
        r_ready : out std_logic;
        r_data  : in  std_logic_vector;
        c_valid : out std_logic;
        c_ready : in  std_logic;
        c_data  : out std_logic_vector
    );
end entity skid_buffer_in;

architecture rtl of skid_buffer_in is
    constant data_width : natural := r_data'length;
    signal b_data  : std_logic_vector(r_data'range);
    signal b_valid : std_logic;
begin

    skid : process(aclk, aresetn)
    begin
        if (aresetn = '0') then
            r_ready <= '0';
            b_data  <= (others => '0');
            b_valid <= '0';
        elsif rising_edge(aclk) then
            if c_ready then
                b_valid <= '0';
            end if;
            if (r_valid and r_ready) = '1' then
                if c_ready = '0' then
                    b_data <= r_data;
                end if;
                b_valid <= not c_ready;
                r_ready <= c_ready;
            else
                r_ready <= c_ready or not b_valid;
            end if;
        end if;
    end process skid;

    with to_bit(b_valid) select c_data <=
        r_data when '0',
        b_data when '1';
    with to_bit(b_valid) select c_valid <=
        r_valid and r_ready when '0',
        '1' when '1';

end architecture rtl;
