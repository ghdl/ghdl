library ieee;
use ieee.fixed_pkg.all;

library work;
use work.pacote_aux.all;

entity norm is
    generic (
        gen_n_caracteristicas : integer

    );
    port (
        i_clk     : in bit;
        i_init    : in bit;
        i_reset   : in bit;
        i_x       : in vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        i_max_x   : in vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        i_min_x   : in vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        o_x_norm  : out vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        o_ocupado : out bit
    );
end norm;

architecture redimensionamento of norm is
    signal iniciar   : bit := '0';
    signal resetar   : bit := '0';
    signal contador  : integer := 0;
    signal valores   : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal maiores   : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal menores   : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal resultado : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);

begin
    calcula : process (i_clk) begin
        if resetar = '1' then
            o_ocupado <= '0';
            contador <= 0;
        end if;

        if iniciar = '1' then
            o_ocupado <= '1';
            contador <= 0;
            
            -- Recebe entradas
            valores <= i_x;
            maiores <= i_max_x;
            menores <= i_min_x;

            -- Reinicia saída
            resultado <= (others=>to_sfixed(0, resultado(0)));


        end if;

        if o_ocupado = '1' and contador < gen_n_caracteristicas then
            resultado(contador) <= resize(arg => (
                valores(contador) - menores(contador)) / (maiores(contador) - menores(contador)),
                size_res => resultado(contador)
            );

            contador <= contador + 1;
            
        elsif contador = gen_n_caracteristicas  then
            o_ocupado <= '0';
            o_x_norm <= resultado;
        end if;
        
    end process calcula;

    inicializa : process (o_ocupado, i_init, i_reset) begin -- Controle dos estados da FSM
        if o_ocupado = '0' and i_init = '1' then
            iniciar <= '1';
            resetar <= '0';
        elsif o_ocupado = '1' and i_reset = '1' and i_init = '1' then
            iniciar <= '1';
            resetar <= '0';
        elsif o_ocupado = '1' and i_reset = '1' then
            resetar <= '1';
        else
            iniciar <= '0';
            resetar <= '0';
        end if;
    end process inicializa;
end redimensionamento;
