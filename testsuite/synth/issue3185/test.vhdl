library ieee;
use ieee.std_logic_1164.all;

entity shift_reg is
    generic (
        type g_data_type;
        g_size: positive
    );
    port (
        i_clk: in std_ulogic;
        i_data: in g_data_type;
        o_data: out g_data_type
    );
end entity;

architecture rtl of shift_reg is
    type t_reg is array (g_size-1 downto 0) of g_data_type;
    signal s_reg: t_reg;
begin
    o_data <= s_reg(s_reg'high);

    process (i_clk) is
    begin
        if rising_edge(i_clk) then
            for i in s_reg'range loop
                if i = s_reg'low then
                    s_reg(i) <= i_data;
                else
                    s_reg(i) <= s_reg(i - 1);
                end if;
            end loop;
        end if;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port (
        i_clk: in std_ulogic;
        i_data1: in std_ulogic;
        o_data1: out std_ulogic;
        i_data2: in std_ulogic;
        o_data2: out std_ulogic;
        i_data3: in unsigned(7 downto 0);
        o_data3: out unsigned(7 downto 0)
    );
end entity;

architecture rtl of test is
begin
    inst1: entity work.shift_reg
    generic map (
        g_data_type => std_ulogic,
        g_size => 2
    )
    port map (
        i_clk => i_clk,
        i_data => i_data1,
        o_data => o_data1
    );

    inst2: entity work.shift_reg
    generic map (
        g_data_type => std_ulogic,
        g_size => 2
    )
    port map (
        i_clk => i_clk,
        i_data => i_data2,
        o_data => o_data2
    );

    inst3: entity work.shift_reg
    generic map (
        g_data_type => unsigned(7 downto 0),
        g_size => 2
    )
    port map (
        i_clk => i_clk,
        i_data => i_data3,
        o_data => o_data3
    );
end architecture;
