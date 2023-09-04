library ieee;
use ieee.fixed_pkg.all;

library work;
use work.pacote_aux.all;

entity top is
    port (
        i_clk       : in bit;
        i_init      : in bit;
        i_reset     : in bit;
        i_x_treino  : in mat_s_fixo(amostras_treino - 1 downto 0, n_caracteristicas - 1 downto 0);
        i_y_treino  : vec_inteiro(amostras_treino - 1 downto 0);
        i_infere    : in vec_s_fixo(n_caracteristicas - 1 downto 0);
        o_resultado : out integer;
        o_ocupado   : out bit
    );
end top;

architecture unica of top is
begin
    knn : entity work.knn
        generic map(
            gen_n_amostras        => amostras_treino,
            gen_n_caracteristicas => n_caracteristicas,
            gen_n_classes         => n_classes,
            gen_k                 => 3
        )
        port map(
            i_clk       => i_clk,       
            i_init      => i_init,      
            i_reset     => i_reset,     
            i_x_treino  => i_x_treino,  
            i_y_treino  => i_y_treino,  
            i_infere    => i_infere,    
            o_resultado => o_resultado, 
            o_ocupado   => o_ocupado
        );
end unica;
