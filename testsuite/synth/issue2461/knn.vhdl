library ieee;
use ieee.fixed_pkg.all;

library work;
use work.pacote_aux.all;

entity knn is
    generic (
        gen_n_amostras        : in integer; -- quantidade de amostras de treino
        gen_n_caracteristicas : in integer; -- quantidade de características por amostra
        gen_n_classes         : in integer;
        gen_k                 : in integer -- valor k do algoritmo k-nn
    );
    port (
        i_clk       : in bit;
        i_init      : in bit;
        i_reset     : in bit;
        i_x_treino  : in mat_s_fixo(gen_n_amostras - 1 downto 0, gen_n_caracteristicas - 1 downto 0);
        i_y_treino  : vec_inteiro(gen_n_amostras - 1 downto 0);
        i_infere    : in vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        o_resultado : out integer;
        o_ocupado   : out bit
    );
end knn;

architecture insercao of knn is

    -- sinais do módulo de normalização
    signal norm_init      : bit := '0';
    signal norm_reset     : bit := '0';
    signal norm_entrada   : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal norm_resultado : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal norm_ocupado   : bit;
    signal norm_comecou   : bit                                            := '0';
    signal norm_terminou  : bit                                            := '0';
    signal finaliza_norm  : bit                                            := '0';
    signal menores        : vec_s_fixo(gen_n_caracteristicas - 1 downto 0) := menor_por_caracteristica;
    signal maiores        : vec_s_fixo(gen_n_caracteristicas - 1 downto 0) := maior_por_caracteristica;

    -- sinais do módulo de distancia
    signal dist_init      : bit := '0';
    signal dist_reset     : bit := '0';
    signal dist_treino    : mat_s_fixo(gen_n_amostras - 1 downto 0, gen_n_caracteristicas - 1 downto 0);
    signal dist_infere    : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal dist_resultado : vec_s_fixo(gen_n_amostras - 1 downto 0);
    signal dist_amostra   : integer;
    signal dist_ocupado   : bit;
    signal dist_comecou   : bit := '0';
    signal dist_terminou  : bit := '0';
    signal finaliza_dist  : bit := '0';

    --sinais do módulo de argmin
    signal argmin_init      : bit := '0';
    signal argmin_reset     : bit := '0';
    signal argmin_entrada   : vec_s_fixo(gen_n_amostras - 1 downto 0);
    signal argmin_resultado : vec_inteiro(gen_k - 1 downto 0);
    signal argmin_ocupado   : bit;
    signal argmin_comecou   : bit := '0';
    signal argmin_terminou  : bit := '0';
    signal finaliza_argmin  : bit := '0';

    -- sinais do módulo de moda
    signal moda_init      : bit := '0';
    signal moda_reset     : bit := '0';
    signal moda_entrada   : vec_inteiro(gen_k - 1 downto 0);
    signal moda_resultado : integer;
    signal moda_ocupado   : bit;
    signal moda_comecou   : bit     := '0';
    signal contador_moda  : integer := 0;
    signal moda_terminou  : bit     := '0';
    signal finaliza_moda  : bit     := '0';
    signal moda_iniciar   : bit     := '0';

    -- sinais do módulo de knn
    signal iniciar : bit := '0';
    signal treino  : mat_s_fixo(gen_n_amostras - 1 downto 0, gen_n_caracteristicas - 1 downto 0);
    signal infere  : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);

begin
    normaliza : entity work.norm
        generic map(
            gen_n_caracteristicas => n_caracteristicas
        )
        port map(
            i_clk     => i_clk,
            i_init    => norm_init,
            i_reset   => norm_reset,
            i_x       => norm_entrada,
            i_min_x   => menores,
            i_max_x   => maiores,
            o_x_norm  => norm_resultado,
            o_ocupado => norm_ocupado
        );

    distancias : entity work.distancias
        generic map(
            gen_n_amostras        => amostras_treino,
            gen_n_caracteristicas => n_caracteristicas
        )
        port map(
            i_clk       => i_clk,
            i_init      => dist_init,
            i_reset     => dist_reset,
            i_elementos => dist_treino,
            i_valor     => dist_infere,
            o_resultado => dist_resultado,
            o_amostra   => dist_amostra,
            o_ocupado   => dist_ocupado
        );

    argmin : entity work.argmin
        generic map(
            gen_n_elementos => amostras_treino,
            gen_k           => gen_k
        )
        port map(
            i_clk       => i_clk,
            i_init      => argmin_init,
            i_reset     => argmin_reset,
            o_ocupado   => argmin_ocupado,
            i_elementos => argmin_entrada,
            o_indices   => argmin_resultado
        );

    moda : entity work.moda
        generic map(
            gen_n_elementos => gen_k,
            gen_n_classes   => n_classes
        )
        port map(
            i_clk       => i_clk,
            i_init      => moda_init,
            i_reset     => moda_reset,
            o_ocupado   => moda_ocupado,
            i_vec       => moda_entrada,
            o_resultado => moda_resultado
        );

    termina_norm : process (norm_ocupado, i_clk) begin
        if falling_edge(norm_ocupado) then
            finaliza_norm <= '1';
        else
            finaliza_norm <= '0';
        end if;
    end process termina_norm;

    termina_dist : process (dist_ocupado, i_clk) begin
        if falling_edge(dist_ocupado) then
            finaliza_dist <= '1';
        else
            finaliza_dist <= '0';
        end if;
    end process termina_dist;

    termina_argmin : process (argmin_ocupado, i_clk) begin
        if falling_edge(argmin_ocupado) then
            finaliza_argmin <= '1';
        else
            finaliza_argmin <= '0';
        end if;
    end process termina_argmin;

    termina_moda : process (moda_ocupado, i_clk) begin
        if falling_edge(moda_ocupado) then
            finaliza_moda <= '1';
        else
            finaliza_moda <= '0';
        end if;
    end process termina_moda;

    entrada_moda : process (i_clk) begin
        if argmin_terminou = '1' and contador_moda < gen_k then
            moda_entrada(contador_moda) <= i_y_treino(argmin_resultado(contador_moda));
            contador_moda               <= contador_moda + 1;
        elsif argmin_terminou = '1' then
            moda_iniciar <= '1';
        else
            moda_iniciar <= '0';
            contador_moda <= 0;
        end if;
    end process entrada_moda;

    calcula : process (i_clk) begin
        if iniciar = '1' then
            o_ocupado    <= '1';
            norm_entrada <= infere;
        end if;

        if o_ocupado = '1' then

            -- Módulo de normalização
            if norm_terminou = '0' and norm_ocupado = '0' and norm_terminou = '0' and norm_comecou = '0' then
                norm_init    <= '1';
                norm_comecou <= '1';
            elsif norm_terminou = '0' and norm_ocupado = '1' then
                norm_init <= '0';
            end if;
            if finaliza_norm = '1' then
                norm_terminou <= '1';
                dist_infere   <= norm_resultado;
                dist_treino   <= treino;
            end if;

            -- Módulo de distancias
            if finaliza_norm = '1'and dist_comecou = '0' then
                dist_init    <= '1';
                dist_comecou <= '1';
            elsif dist_terminou = '0' and dist_ocupado = '1' then
                dist_init <= '0';
            end if;
            if finaliza_dist = '1' then
                dist_terminou  <= '1';
                argmin_entrada <= dist_resultado;
            end if;

            -- Módulo de argmin
            if finaliza_dist = '1' and argmin_comecou = '0' then
                argmin_init    <= '1';
                argmin_comecou <= '1';
            elsif argmin_terminou = '0' and argmin_ocupado = '1' then
                argmin_init <= '0';
            end if;
            if finaliza_argmin = '1' then
                argmin_terminou <= '1';
            end if;

            -- Módulo de moda
            if moda_iniciar = '1' and moda_comecou = '0' then
                moda_init    <= '1';
                moda_comecou <= '1';
            elsif moda_terminou = '0' and moda_ocupado = '1' then
                moda_init <= '0';
            end if;
            if finaliza_moda = '1' then
                moda_terminou <= '1';
            end if;

            -- Finaliza inferência
            if moda_terminou = '1' then
                o_ocupado <= '0';

                -- Reset das variáveis de estado
                norm_comecou <= '0';
                norm_terminou <= '0';

                dist_comecou    <= '0';
                dist_terminou   <= '0';

                argmin_comecou  <= '0';
                argmin_terminou <= '0';

                moda_comecou    <= '0';
                moda_terminou   <= '0';

                -- Ajusta resultado
                o_resultado <= moda_resultado;
            end if;
        end if;

    end process calcula;

    inicializa : process (o_ocupado, i_init, i_reset, iniciar) begin -- Controle dos estados da FSM
        checa_estado : if (o_ocupado = '0' and i_init = '1') or (o_ocupado = '1' and i_reset = '1' and i_init = '1') then
            treino  <= i_x_treino;
            infere  <= i_infere;
            iniciar <= '1';
        else
            iniciar <= '0';
        end if checa_estado;
    end process inicializa;
end insercao;
