library ieee;
use ieee.numeric_bit.unsigned;

package isa is

    type alu_op is (alu_abus,alu_add,alu_and,alu_neg);

    type cond_op is (cond_no_jump,cond_neg,cond_zero,cond_none);

    type shifter_op is (shifter_noop,shifter_left,shifter_right);

    type bus_operand is ('0','1',neg_one,pc,r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,ac);

    constant data_word_size : natural := 16;

    subtype data_word is unsigned(data_word_size - 1 downto 0);

end package;


library ieee;
use ieee.numeric_bit.all;

entity reg is
    port(input : in bit_vector;
         clk : in bit;
         rst : in bit := '1';
         output : out bit_vector);
end entity;

architecture behaviour of reg is
    signal mem : bit_vector(input'RANGE);
begin

    changes : process (clk,rst) is
    begin
        if rst = '0' then
            mem <= (others => '0');
        elsif rising_edge(clk) then
            mem <= input;
        end if;
    end process;

    output <= mem;

end architecture;


library ieee;
use work.isa.all;

entity register_bank is
    port(s_result : in data_word;
         s_select, b_select, a_select : in bus_operand;
         clk, write_enable : in bit;
         rst : in bit := '1';
         a_out, b_out : out data_word);
end entity;

architecture behavioural of register_bank is

    type reg_clocks_t is array (r0 to ac) of bit;
    signal reg_clocks: reg_clocks_t;

    subtype bv is bit_vector(data_word'RANGE);

    type reg_outputs_t is array (r0 to ac) of bv;
    signal reg_outputs: reg_outputs_t;

begin

    registers : for i in r0 to ac generate
        reg : entity work.reg
                    port map(input => bv(s_result),
                             clk => reg_clocks(i),
                             rst => rst,
                             output => reg_outputs(i));
    end generate;

    read_process : process (b_select, a_select, clk, rst) is

        procedure assign(signal bus_out : out data_word; reg_select : bus_operand) is
        begin
            case reg_select is
                when '0' | pc => bus_out <= (others => '0');
                when '1' => bus_out <= (0 => '1',others => '0');
                when neg_one => bus_out <= (others => '1');
                when others =>
                    bus_out <= data_word(reg_outputs(reg_select));
            end case;
        end procedure;

    begin
        assign(a_out, a_select);
        assign(b_out, b_select);
    end process;

    write_process : process (s_select, clk, write_enable) is
        variable enable : bit;
    begin
        for i in r0 to ac loop
            if i = s_select then
                enable := '1';
            else
                enable := '0';
            end if;
            reg_clocks(i) <= clk and write_enable and enable;
        end loop;
    end process;

end architecture;
