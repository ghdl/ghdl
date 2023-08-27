library work;
use work.pacote_aux.all;

entity insere is
    generic (
        gen_n_elementos : in integer -- quantidade de elementos no vetor de entradas
    );
    port (
        i_clk       : in bit;
        i_init      : in bit;
        i_reset     : in bit;
        i_elementos : in vec_inteiro(gen_n_elementos - 1 downto 0);
        i_valor     : in integer;
        i_indice    : in integer;
        o_resultado : out vec_inteiro(gen_n_elementos - 1 downto 0);
        o_ocupado   : out bit
    );
end insere;

architecture shift of insere is
    signal iniciar   : bit := '0';
    signal finaliza  : bit := '0';
    signal elementos : vec_inteiro(gen_n_elementos - 1 downto 0);
    signal valor     : integer;
    signal indice    : integer;

begin

    calcula_saida : process (i_clk) begin
        if iniciar = '1' then
            o_ocupado <= '1';
            elementos <= i_elementos;
            valor     <= i_valor;
            indice    <= i_indice;
        end if;

        if o_ocupado = '1' and finaliza = '0' then
            for indice_atual in 0 to indice - 1 loop
                elementos(indice_atual) <= elementos(indice_atual + 1);
            end loop;
            elementos(indice) <= valor;
            finaliza          <= '1';

        end if;

        if finaliza = '1' then
            finaliza    <= '0';
            o_resultado <= elementos;
            o_ocupado   <= '0';
        end if;
    end process calcula_saida;

    inicializa : process (o_ocupado, i_init, i_reset) begin -- Controle dos estados da FSM
        if o_ocupado = '0' and i_init = '1' then
            iniciar <= '1';
        elsif o_ocupado = '1' and i_reset = '1' and i_init = '1' then
            iniciar <= '1';
        else
            iniciar <= '0';
        end if;
    end process inicializa;

end shift;
