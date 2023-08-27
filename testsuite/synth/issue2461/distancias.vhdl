library ieee;
use ieee.fixed_pkg.all;

library work;
use work.pacote_aux.all;

entity distancias is
    generic (
        gen_n_amostras        : in integer; -- quantidade de elementos na matriz de entradas
        gen_n_caracteristicas : in integer
    );
    port (
        i_clk       : in bit;
        i_init      : in bit;
        i_reset     : in bit;
        i_elementos : in mat_s_fixo(gen_n_amostras - 1 downto 0, gen_n_caracteristicas - 1 downto 0);
        i_valor     : in vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
        o_resultado : out vec_s_fixo(gen_n_amostras - 1 downto 0);
        o_amostra   : out integer;
        o_ocupado   : out bit
    );
end distancias;
architecture calcula of distancias is
    signal iniciar                 : bit := '0';
    signal finaliza                : bit := '0';
    signal elementos               : mat_s_fixo(gen_n_amostras - 1 downto 0, gen_n_caracteristicas - 1 downto 0);
    signal valor                   : vec_s_fixo(gen_n_caracteristicas - 1 downto 0);
    signal contador_elemento       : integer range 0 to gen_n_amostras        := 0;
    signal contador_caracteristica : integer range 0 to gen_n_caracteristicas := 0;
begin

    calcula_saida : process (i_clk) begin
        if iniciar = '1' then
            o_ocupado               <= '1';
            elementos               <= i_elementos;
            contador_elemento       <= 0;
            contador_caracteristica <= 0;

            for id_carac in gen_n_caracteristicas - 1 downto 0 loop
                valor(id_carac) <= i_valor(id_carac);
            end loop;

            for id_amostra in gen_n_amostras - 1 downto 0 loop
                o_resultado(id_amostra) <= to_sfixed(0, o_resultado(id_amostra));
            end loop;

        end if;

        if o_ocupado = '1' and finaliza = '0' then

            -- Atualiza distância do vetor atual
            if contador_caracteristica < gen_n_caracteristicas and contador_elemento < gen_n_amostras then
                o_resultado(contador_elemento) <= resize(o_resultado(contador_elemento) + (valor(contador_caracteristica) - elementos(contador_elemento, contador_caracteristica)) * (valor(contador_caracteristica) - elementos(contador_elemento, contador_caracteristica)), o_resultado(contador_elemento));
                contador_caracteristica <= contador_caracteristica + 1;

            -- Passa para o próximo vetor
            elsif contador_caracteristica = gen_n_caracteristicas then
                if o_resultado(contador_elemento) < 0 then
                    o_resultado(contador_elemento) <= resize(-o_resultado(contador_elemento), o_resultado(contador_elemento));
                end if;
                contador_caracteristica <= 0;
                contador_elemento <= contador_elemento + 1;
            end if;


            if contador_elemento = gen_n_amostras then
                o_resultado(gen_n_amostras - 1) <= resize(-o_resultado(gen_n_amostras - 1), o_resultado(gen_n_amostras - 1));
                finaliza <= '1';
            end if;

        elsif o_ocupado = '1' and finaliza = '1' then
            o_resultado(contador_elemento - 1) <= resize(-o_resultado(contador_elemento - 1), o_resultado(contador_elemento - 1));
            finaliza                           <= '0';
            o_ocupado                          <= '0';
        end if;

    end process calcula_saida;

    saida_amostra : process (contador_elemento, o_ocupado) begin
        if falling_edge(o_ocupado) then
            o_amostra <= gen_n_amostras;
        else
            o_amostra <= contador_elemento - 1;
        end if;
    end process saida_amostra;

    inicializa : process (o_ocupado, i_init, i_reset) begin -- Controle dos estados da FSM
        if o_ocupado = '0' and i_init = '1' then
            iniciar <= '1';
        elsif o_ocupado = '1' and i_reset = '1' and i_init = '1' then
            iniciar <= '1';
        else
            iniciar <= '0';
        end if;
    end process inicializa;
end architecture calcula;
