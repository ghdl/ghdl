library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity multiply_karatsuba_step is
    generic (
        constant g_operand_width : natural := 16
    );
    port (
        factor_1_i : in  unsigned(g_operand_width-1 downto 0);
        factor_2_i : in  unsigned(g_operand_width-1 downto 0);
        product_o  : out unsigned(2*g_operand_width-1 downto 0)
    );
end entity multiply_karatsuba_step;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
architecture struct of multiply_karatsuba_step is
    signal factor_1_high  : unsigned(g_operand_width/2-1 downto 0);
    signal factor_2_high  : unsigned(g_operand_width/2-1 downto 0);
    signal product_high   : unsigned(g_operand_width-1 downto 0);
    component multiply_karatsuba_step is
        generic (
            constant g_operand_width : natural := 16 -- must be a power of 2
        );
        port (
            factor_1_i : in  unsigned(g_operand_width-1 downto 0);
            factor_2_i : in  unsigned(g_operand_width-1 downto 0);
            product_o  : out unsigned(2*g_operand_width-1 downto 0)
        );
    end component;
begin
    karatsuba_g: if g_operand_width/=2 generate
        multiply_karatsuba_step_high_inst : multiply_karatsuba_step
            generic map (
                g_operand_width => g_operand_width/2
            )
            port map (
                factor_1_i => factor_1_high,
                factor_2_i => factor_2_high,
                product_o  => product_high
            );
    end generate karatsuba_g;
    last_g: if g_operand_width=2 generate
    end generate last_g;
    process
    begin
        report "path-name = " & multiply_karatsuba_step'path_name ;
        wait;
    end process;
end architecture;
