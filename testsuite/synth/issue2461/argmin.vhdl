library ieee;
use ieee.fixed_pkg.all;

library work;
use work.pacote_aux.all;

entity argmin is
    generic (
        gen_n_elementos : in integer; -- quantidade de elementos no vetor de entradas
        gen_k           : in integer  -- quantidade de índices do maior
    );
    port (
        i_clk       : in bit;
        i_init      : in bit;
        i_reset     : in bit;
        i_elementos : in vec_s_fixo;
        o_indices   : out vec_inteiro;
        o_ocupado   : out bit
    );
end argmin;

architecture insercao of argmin is
    signal iniciar         : bit := '0';
    signal finaliza        : bit := '0';
    signal deve_inserir    : bit := '0';
    signal inicia_insercao : bit := '0';
    signal inserindo       : bit := '0';
    signal elementos       : vec_s_fixo(gen_n_elementos - 1 downto 0);                       -- vetor de entradas
    signal indices         : vec_inteiro(gen_k downto 0) := (others => gen_n_elementos - 1); -- resultado
    signal c_elemento      : integer                     := 0;
    signal c_indices       : integer                     := 0;

    -- Sinais do insersor
    signal reset_insere     : bit := '0';
    signal insere_ocupado   : bit;
    signal resultado_insere : vec_inteiro(gen_k downto 0);
begin

    insersor : entity work.insere
        generic map(gen_n_elementos => gen_k + 1)
        port map(
            i_clk       => i_clk,
            i_init      => inicia_insercao,
            i_reset     => reset_insere,
            i_elementos => indices,
            i_valor     => c_elemento,
            i_indice    => c_indices,
            o_resultado => resultado_insere,
            o_ocupado   => insere_ocupado
        );

    busca_indice : process (i_clk) begin
        if iniciar = '1' then
            o_ocupado       <= '1';
            c_elemento      <= 0;
            deve_inserir    <= '0';
            inicia_insercao <= '0';
        end if;

        if o_ocupado = '1' and elementos(c_elemento) < elementos(indices(0)) then
            deve_inserir <= '1';
        elsif o_ocupado = '1' and c_elemento < gen_n_elementos - 1 then
            c_elemento <= c_elemento + 1;
        elsif c_elemento = gen_n_elementos - 1 then
            finaliza <= '1';
        end if;

        if o_ocupado = '1' and deve_inserir = '1' and insere_ocupado = '0' then
            if elementos(c_elemento) < elementos(indices(c_indices + 1)) and c_indices < gen_k - 1 then
                c_indices <= c_indices + 1;
            elsif inserindo = '1' then
                inserindo <= '0';
            else
                inicia_insercao <= '1';
            end if;
        end if;

        if o_ocupado = '1' and inicia_insercao = '1'then
            inicia_insercao <= '0';
            inserindo       <= '1';
        end if;

        if o_ocupado = '1' and insere_ocupado = '0' and inserindo = '1' then
            indices      <= resultado_insere;
            c_indices    <= 0;
            deve_inserir <= '0';
            if c_elemento < gen_n_elementos - 1 then
                c_elemento <= c_elemento + 1;
            elsif o_ocupado = '1' then
                finaliza <= '1';
            end if;
        end if;

        if finaliza = '1' then
            for i in gen_k - 1 downto 0 loop
                o_indices(i) <= indices(i);
            end loop;
            o_ocupado  <= '0';
            finaliza   <= '0';
            c_elemento <= 0;
        end if;

    end process busca_indice;

    inicializa : process (o_ocupado, i_init, i_reset, iniciar) begin -- Controle dos estados da FSM
        checa_estado : if o_ocupado = '0' and i_init = '1' then
            elementos <= i_elementos;
            iniciar   <= '1';
        elsif o_ocupado = '1' and i_reset = '1' and i_init = '1' then
            elementos <= i_elementos;
            iniciar   <= '1';
        else
            iniciar <= '0';
        end if checa_estado;
    end process inicializa;

end insercao;
