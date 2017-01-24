library ieee;
use ieee.std_logic_1164.all, ieee.fixed_pkg.all;
use ieee.math_complex.all;

entity mac is 
    port( clk, reset : in std_ulogic;
          x_real : in u_sfixed(0 downto -15);-- real and imaginary part of the two input data sequences
          x_imag : in u_sfixed(0 downto -15);
          y_real : in u_sfixed(0 downto -15);
          y_imag : in u_sfixed(0 downto -15);
          s_real : out u_sfixed(0 downto -15); --real and imaginary parts of accumulated sum
          s_imag : out u_sfixed(0 downto -15);
          ovf : out std_ulogic); --overflow flag
end entity mac;


-- Behavioral model of MAC algorithm allows for focus on the algorithm without being distracted 
-- by other details at this erly design stage.

architecture behavioral of mac is 
    signal x_complex, y_complex, s_complex : complex;

begin
    x_complex <= ( to_real(x_real), to_real(x_imag) );
    y_complex <= ( to_real(y_real), to_real(y_imag) );

    behavior : process (clk) is
        variable input_x, input_y : complex := (0.0, 0.0);
        variable real_part_product_1, real_part_product_2,
                 imag_part_product_1, imag_part_product_2 : real := 0.0;   
        variable product, sum : complex := (0.0, 0.0);
        variable real_accumulator_ovf,
                 imag_accumulator_ovf : boolean := false;
    begin
        if rising_edge(clk) then
            -- Work from the end of the pipeline back to the start,
            -- so as not to overwrite previosu results from the pipeline
            -- registers before they are even used. 

            -- Update accumulator and generate outputs. 
            if reset then
                sum := (0.0, 0.0);
                real_accumulator_ovf := false;
                imag_accumulator_ovf := false;
            else
                sum := product + sum;
                real_accumulator_ovf := real_accumulator_ovf 
                                        or sum.re < -16.0
                                        or sum.re >= +16.0;
                imag_accumulator_ovf := imag_accumulator_ovf 
                                        or sum.im < -16.0
                                        or sum.im >= +16.0;
            end if;
            s_complex <= sum;
            ovf <= '1'; 
            -- ovf <= '1' when (real_accumulator_ovf or imag_accumulator_ovf
            --                 or sum.re < -1.0 or sum.re >= +1.0
            --                 or sum.im < -1.0 or sum.im >= +1.0 ) else '0';
            -- Update product registers
            product.re := real_part_product_1 - real_part_product_2;
            product.im := imag_part_product_1 + imag_part_product_2;

            -- Update partial product registers
            -- (actually with the full product).
            real_part_product_1 := input_x.re * input_y.re;
            real_part_product_2 := input_x.im * input_y.im;
            imag_part_product_1 := input_x.re * input_y.re;
            imag_part_product_2 := input_x.im * input_y.im;

            -- Update input registers using MAC inputs
            input_x := x_complex;
            input_y := y_complex;
        end if;
    end process behavior;
    s_real <= to_sfixed(s_complex.re, s_real);
    s_imag <= to_sfixed(s_complex.im, s_imag);
end architecture behavioral;
