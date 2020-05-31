library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity ent is
	port (
		insn_i     : in std_ulogic_vector(31 downto 0);
		ispr1_o    : out std_ulogic_vector(5 downto 0);
		ispr2_o    : out std_ulogic_vector(5 downto 0)
	);
end entity ent;

architecture behaviour of ent is
    -- SPR numbers
    subtype spr_num_t is integer range 0 to 1023;

    function decode_spr_num(insn: std_ulogic_vector(31 downto 0)) return spr_num_t;

    constant SPR_XER    : spr_num_t := 1;
    constant SPR_LR     : spr_num_t := 8;
    constant SPR_CTR    : spr_num_t := 9;

    -- Extended GPR indice (can hold an SPR)
    subtype gspr_index_t is std_ulogic_vector(5 downto 0);

    function decode_spr_num(insn: std_ulogic_vector(31 downto 0)) return spr_num_t is
    begin
	return to_integer(unsigned(insn(15 downto 11) & insn(20 downto 16)));
    end;
    function fast_spr_num(spr: spr_num_t) return gspr_index_t is
       variable n : integer range 0 to 31;
    begin
       case spr is
       when SPR_LR =>
           n := 0;
       when SPR_CTR =>
           n:= 1;
       when SPR_XER =>
           n := 12;
       when others =>
           n := 0;
           return "000000";
       end case;
       return "1" & std_ulogic_vector(to_unsigned(n, 5));
    end;

begin
	decode1_1: process(all)
	begin
		ispr1_o <= fast_spr_num(decode_spr_num(insn_i));
		ispr2_o <= fast_spr_num(SPR_XER);
	end process;
end architecture behaviour;
