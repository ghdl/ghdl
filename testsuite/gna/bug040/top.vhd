library ieee;
use ieee.std_logic_1164.all;

entity top is
	port (
		clock : in  std_logic;
		reset : in  std_logic;
		start : in  std_logic;
		stdout_rdy : out std_logic;
		stdout_ack : in  std_logic;
		stdin_ack : in  std_logic;
		stdout_data : out std_logic_vector(7 downto 0);
		stdin_data : in  std_logic_vector(7 downto 0);
		stdin_rdy : out std_logic
	);
end top;

architecture augh of top is

	-- Declaration of components

	component cmp_869 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(7 downto 0);
			in0 : in  std_logic_vector(7 downto 0)
		);
	end component;

	component cmp_978 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_979 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_847 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_855 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_852 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_213 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_216 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_214 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_846 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_848 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_849 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component p_jinfo_comps_info_id is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_comps_info_h_samp_factor is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_comps_info_quant_tbl_no is
		port (
			wa0_data : in  std_logic_vector(1 downto 0);
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic_vector(1 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_comps_info_dc_tbl_no is
		port (
			wa0_data : in  std_logic;
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic;
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_quant_tbl_quantval is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(7 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(7 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_xhuff_tbl_bits is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_xhuff_tbl_huffval is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(9 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(9 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_xhuff_tbl_bits is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_xhuff_tbl_huffval is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(9 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(9 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_dhuff_tbl_ml is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic;
			clk : in  std_logic;
			ra0_addr : in  std_logic;
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_dhuff_tbl_maxcode is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_dhuff_tbl_mincode is
		port (
			wa0_data : in  std_logic_vector(8 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(8 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_dc_dhuff_tbl_valptr is
		port (
			wa0_data : in  std_logic_vector(8 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(8 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_dhuff_tbl_ml is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic;
			clk : in  std_logic;
			ra0_addr : in  std_logic;
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_dhuff_tbl_maxcode is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_dhuff_tbl_mincode is
		port (
			wa0_data : in  std_logic_vector(8 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(8 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component p_jinfo_ac_dhuff_tbl_valptr is
		port (
			wa0_data : in  std_logic_vector(8 downto 0);
			wa0_addr : in  std_logic_vector(6 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(6 downto 0);
			ra0_data : out std_logic_vector(8 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component outdata_comp_vpos is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component outdata_comp_hpos is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(1 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(1 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component outdata_comp_buf is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(14 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(14 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component izigzag_index is
		port (
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(5 downto 0);
			ra0_data : out std_logic_vector(5 downto 0)
		);
	end component;

	component jpegfilebuf is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(12 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(12 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component huffbuff is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(7 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(7 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component idctbuff is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(8 downto 0);
			clk : in  std_logic;
			ra2_data : out std_logic_vector(31 downto 0);
			ra2_addr : in  std_logic_vector(8 downto 0);
			ra1_data : out std_logic_vector(31 downto 0);
			ra1_addr : in  std_logic_vector(8 downto 0);
			ra0_addr : in  std_logic_vector(8 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component quantbuff is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(5 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(5 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component extend_mask is
		port (
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(4 downto 0);
			ra0_data : out std_logic_vector(20 downto 0)
		);
	end component;

	component bit_set_mask is
		port (
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(4 downto 0);
			ra0_data : out std_logic_vector(31 downto 0)
		);
	end component;

	component lmask is
		port (
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(4 downto 0);
			ra0_data : out std_logic_vector(31 downto 0)
		);
	end component;

	component huff_make_dhuff_tb_ac_huffsize is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(8 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(8 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component huff_make_dhuff_tb_ac_huffcode is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(8 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(8 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component huff_make_dhuff_tb_dc_huffsize is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(8 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(8 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component huff_make_dhuff_tb_dc_huffcode is
		port (
			wa0_data : in  std_logic_vector(31 downto 0);
			wa0_addr : in  std_logic_vector(8 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(8 downto 0);
			ra0_data : out std_logic_vector(31 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component rgb_buf is
		port (
			wa0_data : in  std_logic_vector(7 downto 0);
			wa0_addr : in  std_logic_vector(9 downto 0);
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(9 downto 0);
			ra0_data : out std_logic_vector(7 downto 0);
			wa0_en : in  std_logic
		);
	end component;

	component zigzag_index is
		port (
			clk : in  std_logic;
			ra0_addr : in  std_logic_vector(5 downto 0);
			ra0_data : out std_logic_vector(5 downto 0)
		);
	end component;

	component shr_212 is
		port (
			output : out std_logic_vector(31 downto 0);
			input : in  std_logic_vector(31 downto 0);
			shift : in  std_logic_vector(5 downto 0);
			padding : in  std_logic
		);
	end component;

	component mul_209 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_210 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component shl_211 is
		port (
			output : out std_logic_vector(31 downto 0);
			input : in  std_logic_vector(31 downto 0);
			shift : in  std_logic_vector(5 downto 0);
			padding : in  std_logic
		);
	end component;

	component sub_206 is
		port (
			gt : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component sub_207 is
		port (
			ge : out std_logic;
			le : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component sub_208 is
		port (
			ge : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component sub_205 is
		port (
			gt : out std_logic;
			ge : out std_logic;
			lt : out std_logic;
			le : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component add_202 is
		port (
			output : out std_logic_vector(31 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component add_203 is
		port (
			output : out std_logic_vector(38 downto 0);
			in_b : in  std_logic_vector(38 downto 0);
			in_a : in  std_logic_vector(38 downto 0)
		);
	end component;

	component add_204 is
		port (
			output : out std_logic_vector(24 downto 0);
			in_b : in  std_logic_vector(24 downto 0);
			in_a : in  std_logic_vector(24 downto 0)
		);
	end component;

	component add_201 is
		port (
			output : out std_logic_vector(38 downto 0);
			in_b : in  std_logic_vector(38 downto 0);
			in_a : in  std_logic_vector(38 downto 0)
		);
	end component;

	component add_200 is
		port (
			output : out std_logic_vector(38 downto 0);
			in_b : in  std_logic_vector(38 downto 0);
			in_a : in  std_logic_vector(38 downto 0)
		);
	end component;

	component cmp_775 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_779 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_780 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_787 is
		port (
			eq : out std_logic;
			in1 : in  std_logic;
			in0 : in  std_logic
		);
	end component;

	component cmp_788 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(2 downto 0);
			in0 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_790 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(3 downto 0);
			in0 : in  std_logic_vector(3 downto 0)
		);
	end component;

	component cmp_792 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_793 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_794 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_791 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_804 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_800 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_799 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_865 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(2 downto 0);
			in0 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_882 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_885 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_887 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component mul_215 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_850 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_851 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_861 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_871 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_873 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(7 downto 0);
			in0 : in  std_logic_vector(7 downto 0)
		);
	end component;

	component cmp_879 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_880 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_217 is
		port (
			ge : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component cmp_863 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(2 downto 0);
			in0 : in  std_logic_vector(2 downto 0)
		);
	end component;

	component cmp_868 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(23 downto 0);
			in0 : in  std_logic_vector(23 downto 0)
		);
	end component;

	component cmp_877 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_878 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_218 is
		port (
			le : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component sub_220 is
		port (
			gt : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component sub_221 is
		port (
			gt : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component mul_222 is
		port (
			output : out std_logic_vector(40 downto 0);
			in_b : in  std_logic_vector(31 downto 0);
			in_a : in  std_logic_vector(31 downto 0)
		);
	end component;

	component sub_219 is
		port (
			le : out std_logic;
			output : out std_logic_vector(40 downto 0);
			sign : in  std_logic;
			in_b : in  std_logic_vector(40 downto 0);
			in_a : in  std_logic_vector(40 downto 0)
		);
	end component;

	component cmp_962 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_975 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component fsm_224 is
		port (
			clock : in  std_logic;
			reset : in  std_logic;
			out40 : out std_logic;
			in2 : in  std_logic;
			in11 : in  std_logic;
			out146 : out std_logic;
			out148 : out std_logic;
			out150 : out std_logic;
			out152 : out std_logic;
			in12 : in  std_logic;
			out153 : out std_logic;
			out154 : out std_logic;
			in13 : in  std_logic;
			out156 : out std_logic;
			out157 : out std_logic;
			out160 : out std_logic;
			out162 : out std_logic;
			out165 : out std_logic;
			out170 : out std_logic;
			out171 : out std_logic;
			out173 : out std_logic;
			out175 : out std_logic;
			out177 : out std_logic;
			out180 : out std_logic;
			out184 : out std_logic;
			in14 : in  std_logic;
			out186 : out std_logic;
			out189 : out std_logic;
			out191 : out std_logic;
			out192 : out std_logic;
			out193 : out std_logic;
			out197 : out std_logic;
			out199 : out std_logic;
			out201 : out std_logic;
			out202 : out std_logic;
			out205 : out std_logic;
			out207 : out std_logic;
			out208 : out std_logic;
			out209 : out std_logic;
			out210 : out std_logic;
			out212 : out std_logic;
			out213 : out std_logic;
			in15 : in  std_logic;
			out221 : out std_logic;
			out222 : out std_logic;
			out224 : out std_logic;
			out225 : out std_logic;
			out228 : out std_logic;
			out229 : out std_logic;
			out230 : out std_logic;
			out231 : out std_logic;
			out99 : out std_logic;
			in6 : in  std_logic;
			out92 : out std_logic;
			out232 : out std_logic;
			in16 : in  std_logic;
			out234 : out std_logic;
			out236 : out std_logic;
			out239 : out std_logic;
			out240 : out std_logic;
			out241 : out std_logic;
			out245 : out std_logic;
			out246 : out std_logic;
			out247 : out std_logic;
			out251 : out std_logic;
			out252 : out std_logic;
			out253 : out std_logic;
			out255 : out std_logic;
			out256 : out std_logic;
			out258 : out std_logic;
			out259 : out std_logic;
			in17 : in  std_logic;
			out263 : out std_logic;
			out264 : out std_logic;
			out266 : out std_logic;
			in18 : in  std_logic;
			out267 : out std_logic;
			out268 : out std_logic;
			out270 : out std_logic;
			out273 : out std_logic;
			out275 : out std_logic;
			out276 : out std_logic;
			in19 : in  std_logic;
			out279 : out std_logic;
			in20 : in  std_logic;
			out281 : out std_logic;
			out282 : out std_logic;
			in21 : in  std_logic;
			out283 : out std_logic;
			out286 : out std_logic;
			out289 : out std_logic;
			out296 : out std_logic;
			out297 : out std_logic;
			out299 : out std_logic;
			out300 : out std_logic;
			out304 : out std_logic;
			out305 : out std_logic;
			in22 : in  std_logic;
			out306 : out std_logic;
			out310 : out std_logic;
			out311 : out std_logic;
			out313 : out std_logic;
			out314 : out std_logic;
			in23 : in  std_logic;
			out316 : out std_logic;
			out317 : out std_logic;
			out320 : out std_logic;
			out322 : out std_logic;
			out324 : out std_logic;
			out325 : out std_logic;
			out326 : out std_logic;
			out328 : out std_logic;
			out332 : out std_logic;
			out333 : out std_logic;
			out334 : out std_logic;
			out335 : out std_logic;
			out338 : out std_logic;
			out339 : out std_logic;
			out341 : out std_logic;
			out342 : out std_logic;
			out344 : out std_logic;
			out93 : out std_logic;
			out98 : out std_logic;
			out85 : out std_logic;
			out87 : out std_logic;
			out88 : out std_logic;
			out80 : out std_logic;
			out82 : out std_logic;
			out83 : out std_logic;
			out84 : out std_logic;
			in5 : in  std_logic;
			out77 : out std_logic;
			out78 : out std_logic;
			out71 : out std_logic;
			out72 : out std_logic;
			in4 : in  std_logic;
			out65 : out std_logic;
			out67 : out std_logic;
			out60 : out std_logic;
			out64 : out std_logic;
			in3 : in  std_logic;
			out59 : out std_logic;
			out53 : out std_logic;
			out55 : out std_logic;
			out49 : out std_logic;
			out44 : out std_logic;
			out104 : out std_logic;
			out107 : out std_logic;
			out111 : out std_logic;
			out112 : out std_logic;
			out114 : out std_logic;
			in7 : in  std_logic;
			out117 : out std_logic;
			out119 : out std_logic;
			out122 : out std_logic;
			in8 : in  std_logic;
			out128 : out std_logic;
			in9 : in  std_logic;
			out129 : out std_logic;
			out130 : out std_logic;
			out133 : out std_logic;
			out134 : out std_logic;
			out136 : out std_logic;
			out137 : out std_logic;
			in10 : in  std_logic;
			out139 : out std_logic;
			out143 : out std_logic;
			out144 : out std_logic;
			out32 : out std_logic;
			out35 : out std_logic;
			out27 : out std_logic;
			out25 : out std_logic;
			out26 : out std_logic;
			in1 : in  std_logic;
			out15 : out std_logic;
			out16 : out std_logic;
			out11 : out std_logic;
			out13 : out std_logic;
			out14 : out std_logic;
			out7 : out std_logic;
			out1 : out std_logic;
			out2 : out std_logic;
			out3 : out std_logic;
			out4 : out std_logic;
			in0 : in  std_logic;
			in24 : in  std_logic;
			out346 : out std_logic;
			out347 : out std_logic;
			out348 : out std_logic;
			out349 : out std_logic;
			in25 : in  std_logic;
			out350 : out std_logic;
			out351 : out std_logic;
			out355 : out std_logic;
			out356 : out std_logic;
			out357 : out std_logic;
			out358 : out std_logic;
			out360 : out std_logic;
			out362 : out std_logic;
			out363 : out std_logic;
			out364 : out std_logic;
			out365 : out std_logic;
			out366 : out std_logic;
			out370 : out std_logic;
			out371 : out std_logic;
			out372 : out std_logic;
			out373 : out std_logic;
			out375 : out std_logic;
			in26 : in  std_logic;
			out376 : out std_logic;
			out378 : out std_logic;
			out379 : out std_logic;
			out381 : out std_logic;
			out382 : out std_logic;
			in27 : in  std_logic;
			out384 : out std_logic;
			in28 : in  std_logic;
			out391 : out std_logic;
			out395 : out std_logic;
			out396 : out std_logic;
			out401 : out std_logic;
			out402 : out std_logic;
			out403 : out std_logic;
			out404 : out std_logic;
			out405 : out std_logic;
			out407 : out std_logic;
			out408 : out std_logic;
			out409 : out std_logic;
			out410 : out std_logic;
			in29 : in  std_logic;
			out412 : out std_logic;
			out414 : out std_logic;
			out415 : out std_logic;
			out417 : out std_logic;
			out418 : out std_logic;
			out419 : out std_logic;
			out420 : out std_logic;
			out422 : out std_logic;
			out424 : out std_logic;
			out425 : out std_logic;
			out426 : out std_logic;
			in30 : in  std_logic;
			out428 : out std_logic;
			out429 : out std_logic;
			out432 : out std_logic;
			out433 : out std_logic;
			out434 : out std_logic;
			out437 : out std_logic;
			out440 : out std_logic;
			out441 : out std_logic;
			in31 : in  std_logic;
			out443 : out std_logic;
			in32 : in  std_logic;
			out445 : out std_logic;
			out447 : out std_logic;
			out448 : out std_logic;
			out450 : out std_logic;
			in33 : in  std_logic;
			out453 : out std_logic;
			out455 : out std_logic;
			out458 : out std_logic;
			in34 : in  std_logic;
			out462 : out std_logic;
			out464 : out std_logic;
			out467 : out std_logic;
			out468 : out std_logic;
			out472 : out std_logic;
			in35 : in  std_logic;
			out478 : out std_logic;
			out479 : out std_logic;
			out480 : out std_logic;
			out487 : out std_logic;
			out488 : out std_logic;
			in36 : in  std_logic;
			out491 : out std_logic;
			out496 : out std_logic;
			out497 : out std_logic;
			out498 : out std_logic;
			out500 : out std_logic;
			out504 : out std_logic;
			out505 : out std_logic;
			in37 : in  std_logic;
			out506 : out std_logic;
			out508 : out std_logic;
			in38 : in  std_logic;
			out510 : out std_logic;
			out513 : out std_logic;
			out514 : out std_logic;
			out515 : out std_logic;
			out517 : out std_logic;
			out519 : out std_logic;
			in39 : in  std_logic;
			out523 : out std_logic;
			out526 : out std_logic;
			out527 : out std_logic;
			out528 : out std_logic;
			out530 : out std_logic;
			out531 : out std_logic;
			out533 : out std_logic;
			out534 : out std_logic;
			out537 : out std_logic;
			out538 : out std_logic;
			out549 : out std_logic;
			out558 : out std_logic;
			out559 : out std_logic;
			out561 : out std_logic;
			in40 : in  std_logic;
			out566 : out std_logic;
			out567 : out std_logic;
			out568 : out std_logic;
			out569 : out std_logic;
			out570 : out std_logic;
			out572 : out std_logic;
			out574 : out std_logic;
			out575 : out std_logic;
			out577 : out std_logic;
			in41 : in  std_logic;
			out578 : out std_logic;
			out581 : out std_logic;
			out589 : out std_logic;
			out590 : out std_logic;
			out595 : out std_logic;
			out597 : out std_logic;
			out599 : out std_logic;
			out601 : out std_logic;
			out602 : out std_logic;
			out607 : out std_logic;
			out610 : out std_logic;
			out612 : out std_logic;
			in42 : in  std_logic;
			out614 : out std_logic;
			out621 : out std_logic;
			out628 : out std_logic;
			out635 : out std_logic;
			out636 : out std_logic;
			out638 : out std_logic;
			out640 : out std_logic;
			out643 : out std_logic;
			out646 : out std_logic;
			out649 : out std_logic;
			out651 : out std_logic;
			out656 : out std_logic;
			in43 : in  std_logic;
			out658 : out std_logic;
			out659 : out std_logic;
			out661 : out std_logic;
			out663 : out std_logic;
			out664 : out std_logic;
			in44 : in  std_logic;
			out667 : out std_logic;
			out668 : out std_logic;
			out670 : out std_logic;
			out672 : out std_logic;
			out674 : out std_logic;
			in45 : in  std_logic;
			out679 : out std_logic;
			out681 : out std_logic;
			out683 : out std_logic;
			out686 : out std_logic;
			out688 : out std_logic;
			out690 : out std_logic;
			out692 : out std_logic;
			out694 : out std_logic;
			out696 : out std_logic;
			out697 : out std_logic;
			out698 : out std_logic;
			out699 : out std_logic;
			out700 : out std_logic;
			out703 : out std_logic;
			out704 : out std_logic;
			out706 : out std_logic;
			out708 : out std_logic;
			out710 : out std_logic;
			out712 : out std_logic;
			out715 : out std_logic;
			out718 : out std_logic;
			in46 : in  std_logic;
			out722 : out std_logic;
			out724 : out std_logic;
			out726 : out std_logic;
			out728 : out std_logic;
			out731 : out std_logic;
			out733 : out std_logic;
			out734 : out std_logic;
			out737 : out std_logic;
			out739 : out std_logic;
			out740 : out std_logic;
			out743 : out std_logic;
			out745 : out std_logic;
			out746 : out std_logic;
			in47 : in  std_logic;
			out749 : out std_logic;
			out753 : out std_logic;
			out755 : out std_logic;
			out759 : out std_logic;
			in48 : in  std_logic;
			out762 : out std_logic;
			out764 : out std_logic;
			out765 : out std_logic;
			out767 : out std_logic;
			out768 : out std_logic;
			in49 : in  std_logic;
			out772 : out std_logic;
			in50 : in  std_logic;
			out775 : out std_logic;
			out776 : out std_logic;
			out778 : out std_logic;
			out783 : out std_logic;
			out784 : out std_logic;
			out787 : out std_logic;
			out791 : out std_logic;
			in51 : in  std_logic;
			out794 : out std_logic;
			out795 : out std_logic;
			in52 : in  std_logic;
			out799 : out std_logic;
			out802 : out std_logic;
			out806 : out std_logic;
			out809 : out std_logic;
			out812 : out std_logic;
			out815 : out std_logic;
			out826 : out std_logic;
			out828 : out std_logic;
			in53 : in  std_logic;
			in54 : in  std_logic;
			out843 : out std_logic;
			out848 : out std_logic;
			out852 : out std_logic;
			in55 : in  std_logic;
			out855 : out std_logic;
			out858 : out std_logic;
			in56 : in  std_logic;
			out860 : out std_logic;
			out861 : out std_logic;
			out863 : out std_logic;
			out866 : out std_logic;
			out872 : out std_logic;
			in57 : in  std_logic;
			out874 : out std_logic;
			out876 : out std_logic;
			out879 : out std_logic;
			out882 : out std_logic;
			out886 : out std_logic;
			out887 : out std_logic;
			in58 : in  std_logic;
			out888 : out std_logic;
			out892 : out std_logic;
			out894 : out std_logic;
			out895 : out std_logic;
			out896 : out std_logic;
			out901 : out std_logic;
			out902 : out std_logic;
			out903 : out std_logic;
			out905 : out std_logic;
			out907 : out std_logic;
			out918 : out std_logic;
			out920 : out std_logic;
			out921 : out std_logic;
			out923 : out std_logic;
			out925 : out std_logic;
			out928 : out std_logic;
			out929 : out std_logic;
			out931 : out std_logic;
			out933 : out std_logic;
			out936 : out std_logic;
			out937 : out std_logic;
			out938 : out std_logic;
			out939 : out std_logic;
			out942 : out std_logic;
			out943 : out std_logic;
			out944 : out std_logic;
			out947 : out std_logic;
			out948 : out std_logic;
			out949 : out std_logic;
			out951 : out std_logic;
			in59 : in  std_logic;
			out952 : out std_logic;
			out953 : out std_logic;
			out955 : out std_logic;
			out956 : out std_logic;
			out957 : out std_logic;
			out958 : out std_logic;
			in60 : in  std_logic;
			in61 : in  std_logic;
			out962 : out std_logic;
			out963 : out std_logic;
			out972 : out std_logic;
			out973 : out std_logic;
			out974 : out std_logic;
			in62 : in  std_logic;
			out978 : out std_logic;
			out979 : out std_logic;
			out981 : out std_logic;
			out982 : out std_logic;
			out985 : out std_logic;
			out986 : out std_logic;
			out989 : out std_logic;
			in63 : in  std_logic;
			in64 : in  std_logic;
			in65 : in  std_logic;
			in66 : in  std_logic;
			in67 : in  std_logic;
			in68 : in  std_logic;
			in69 : in  std_logic;
			in70 : in  std_logic;
			in71 : in  std_logic;
			in72 : in  std_logic;
			in73 : in  std_logic;
			in74 : in  std_logic;
			in75 : in  std_logic;
			in76 : in  std_logic;
			in77 : in  std_logic;
			in78 : in  std_logic;
			out990 : out std_logic;
			out991 : out std_logic;
			out993 : out std_logic;
			out994 : out std_logic;
			out996 : out std_logic;
			out997 : out std_logic;
			out998 : out std_logic;
			out999 : out std_logic;
			out1000 : out std_logic;
			out1002 : out std_logic;
			out1003 : out std_logic;
			out1005 : out std_logic;
			out1006 : out std_logic;
			out1007 : out std_logic;
			out1009 : out std_logic;
			out1011 : out std_logic;
			out1012 : out std_logic;
			out1013 : out std_logic;
			out1014 : out std_logic;
			out1015 : out std_logic;
			out1016 : out std_logic;
			out1018 : out std_logic;
			out1019 : out std_logic;
			out1021 : out std_logic;
			out1022 : out std_logic;
			out1024 : out std_logic;
			out1026 : out std_logic;
			out1027 : out std_logic;
			out1029 : out std_logic;
			out1030 : out std_logic;
			out1032 : out std_logic;
			out1033 : out std_logic;
			out1035 : out std_logic;
			out1036 : out std_logic;
			out1037 : out std_logic;
			out1057 : out std_logic;
			out1068 : out std_logic;
			out1069 : out std_logic;
			out1070 : out std_logic;
			out1072 : out std_logic;
			out1073 : out std_logic;
			out1075 : out std_logic;
			out1078 : out std_logic;
			out1080 : out std_logic;
			out1082 : out std_logic;
			out1083 : out std_logic;
			out1084 : out std_logic;
			out1085 : out std_logic;
			out1088 : out std_logic;
			out1089 : out std_logic;
			out1091 : out std_logic;
			out1092 : out std_logic;
			out1094 : out std_logic;
			out1096 : out std_logic;
			out1098 : out std_logic;
			out1101 : out std_logic;
			out1104 : out std_logic;
			out1107 : out std_logic;
			out1109 : out std_logic;
			out1111 : out std_logic;
			out1114 : out std_logic;
			out1119 : out std_logic;
			out1121 : out std_logic;
			out1125 : out std_logic;
			out1126 : out std_logic;
			out1128 : out std_logic;
			out1131 : out std_logic;
			out1134 : out std_logic;
			out1137 : out std_logic;
			out1139 : out std_logic;
			out1141 : out std_logic;
			out1145 : out std_logic;
			out1146 : out std_logic;
			out1147 : out std_logic;
			out1150 : out std_logic;
			out1151 : out std_logic;
			out1152 : out std_logic;
			out1155 : out std_logic;
			out1158 : out std_logic;
			out1160 : out std_logic;
			out1164 : out std_logic;
			out1166 : out std_logic;
			out1169 : out std_logic;
			out1171 : out std_logic;
			out1174 : out std_logic;
			out1175 : out std_logic;
			out1176 : out std_logic;
			out1180 : out std_logic;
			out1181 : out std_logic;
			out1182 : out std_logic;
			out1185 : out std_logic;
			out1186 : out std_logic;
			out1187 : out std_logic;
			out1190 : out std_logic;
			out1213 : out std_logic;
			out1215 : out std_logic;
			out1217 : out std_logic;
			out1220 : out std_logic;
			out1221 : out std_logic;
			out1223 : out std_logic;
			out1228 : out std_logic;
			out1229 : out std_logic;
			out1231 : out std_logic;
			out1235 : out std_logic;
			out1236 : out std_logic;
			out1240 : out std_logic;
			out1243 : out std_logic;
			out1250 : out std_logic;
			out1252 : out std_logic;
			out1253 : out std_logic;
			out1258 : out std_logic;
			out1262 : out std_logic;
			out1266 : out std_logic;
			out1269 : out std_logic;
			out1275 : out std_logic;
			out1278 : out std_logic;
			out1279 : out std_logic;
			out1284 : out std_logic;
			out1286 : out std_logic;
			out1287 : out std_logic;
			out1289 : out std_logic;
			out1290 : out std_logic;
			out1292 : out std_logic;
			out1293 : out std_logic;
			out1295 : out std_logic;
			out1298 : out std_logic;
			out1301 : out std_logic;
			out1302 : out std_logic;
			out1303 : out std_logic;
			out1308 : out std_logic;
			out1309 : out std_logic;
			out1311 : out std_logic;
			out1318 : out std_logic;
			out1319 : out std_logic;
			out1320 : out std_logic;
			out1323 : out std_logic;
			out1324 : out std_logic;
			out1326 : out std_logic;
			out1327 : out std_logic;
			out1329 : out std_logic;
			out1337 : out std_logic;
			out1339 : out std_logic;
			out1340 : out std_logic;
			out1341 : out std_logic;
			out1344 : out std_logic;
			out1346 : out std_logic;
			out1349 : out std_logic;
			out1353 : out std_logic;
			out1356 : out std_logic;
			out1362 : out std_logic;
			out1363 : out std_logic;
			out1364 : out std_logic;
			out1365 : out std_logic;
			out1366 : out std_logic;
			out1368 : out std_logic;
			out1370 : out std_logic;
			out1375 : out std_logic;
			out1378 : out std_logic;
			out1381 : out std_logic;
			out1383 : out std_logic;
			out1387 : out std_logic
		);
	end component;

	component muxb_784 is
		port (
			in_sel : in  std_logic;
			out_data : out std_logic_vector(31 downto 0);
			in_data0 : in  std_logic_vector(31 downto 0);
			in_data1 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_964 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_972 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_973 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_974 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_985 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_971 is
		port (
			ne : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	component cmp_977 is
		port (
			eq : out std_logic;
			in1 : in  std_logic_vector(31 downto 0);
			in0 : in  std_logic_vector(31 downto 0)
		);
	end component;

	-- Declaration of signals

	signal sig_clock : std_logic;
	signal sig_reset : std_logic;
	signal augh_test_159 : std_logic;
	signal augh_test_6 : std_logic;
	signal augh_test_9 : std_logic;
	signal augh_test_10 : std_logic;
	signal augh_test_26 : std_logic;
	signal augh_test_49 : std_logic;
	signal augh_test_52 : std_logic;
	signal augh_test_53 : std_logic;
	signal augh_test_62 : std_logic;
	signal augh_test_67 : std_logic;
	signal augh_test_72 : std_logic;
	signal augh_test_77 : std_logic;
	signal augh_test_83 : std_logic;
	signal augh_test_89 : std_logic;
	signal augh_test_90 : std_logic;
	signal augh_test_94 : std_logic;
	signal augh_test_99 : std_logic;
	signal augh_test_100 : std_logic;
	signal augh_test_101 : std_logic;
	signal augh_test_102 : std_logic;
	signal augh_test_103 : std_logic;
	signal augh_test_104 : std_logic;
	signal augh_test_105 : std_logic;
	signal augh_test_106 : std_logic;
	signal augh_test_107 : std_logic;
	signal augh_test_108 : std_logic;
	signal augh_test_109 : std_logic;
	signal augh_test_111 : std_logic;
	signal augh_test_113 : std_logic;
	signal augh_test_114 : std_logic;
	signal augh_test_115 : std_logic;
	signal augh_test_118 : std_logic;
	signal augh_test_119 : std_logic;
	signal augh_test_120 : std_logic;
	signal augh_test_122 : std_logic;
	signal augh_test_123 : std_logic;
	signal augh_test_124 : std_logic;
	signal augh_test_125 : std_logic;
	signal augh_test_126 : std_logic;
	signal augh_test_127 : std_logic;
	signal augh_test_128 : std_logic;
	signal augh_test_130 : std_logic;
	signal augh_test_131 : std_logic;
	signal augh_test_132 : std_logic;
	signal augh_test_133 : std_logic;
	signal augh_test_134 : std_logic;
	signal augh_test_136 : std_logic;
	signal augh_test_138 : std_logic;
	signal augh_test_142 : std_logic;
	signal augh_test_144 : std_logic;
	signal augh_test_148 : std_logic;
	signal augh_test_150 : std_logic;
	signal augh_test_151 : std_logic;
	signal augh_test_152 : std_logic;
	signal augh_test_154 : std_logic;
	signal augh_test_155 : std_logic;
	signal augh_test_157 : std_logic;
	signal augh_test_158 : std_logic;
	signal augh_test_165 : std_logic;
	signal augh_test_166 : std_logic;
	signal augh_test_167 : std_logic;
	signal augh_test_168 : std_logic;
	signal sig_start : std_logic;
	signal augh_test_171 : std_logic;
	signal augh_test_178 : std_logic;
	signal augh_test_179 : std_logic;
	signal augh_test_180 : std_logic;
	signal augh_test_182 : std_logic;
	signal augh_test_183 : std_logic;
	signal augh_test_184 : std_logic;
	signal augh_test_186 : std_logic;
	signal augh_test_187 : std_logic;
	signal augh_test_188 : std_logic;
	signal augh_test_189 : std_logic;
	signal augh_test_194 : std_logic;
	signal augh_test_196 : std_logic;
	signal augh_test_197 : std_logic;
	signal sig_990 : std_logic;
	signal sig_991 : std_logic;
	signal sig_992 : std_logic_vector(31 downto 0);
	signal sig_993 : std_logic;
	signal sig_994 : std_logic;
	signal sig_995 : std_logic;
	signal sig_996 : std_logic;
	signal sig_997 : std_logic;
	signal sig_998 : std_logic;
	signal sig_999 : std_logic;
	signal sig_1000 : std_logic;
	signal sig_1001 : std_logic;
	signal sig_1002 : std_logic;
	signal sig_1003 : std_logic;
	signal sig_1004 : std_logic;
	signal sig_1005 : std_logic;
	signal sig_1006 : std_logic;
	signal sig_1007 : std_logic;
	signal sig_1008 : std_logic;
	signal sig_1009 : std_logic;
	signal sig_1010 : std_logic;
	signal sig_1011 : std_logic;
	signal sig_1012 : std_logic;
	signal sig_1013 : std_logic;
	signal sig_1014 : std_logic;
	signal sig_1015 : std_logic;
	signal sig_1016 : std_logic;
	signal sig_1017 : std_logic;
	signal sig_1018 : std_logic;
	signal sig_1019 : std_logic;
	signal sig_1020 : std_logic;
	signal sig_1021 : std_logic;
	signal sig_1022 : std_logic;
	signal sig_1023 : std_logic;
	signal sig_1024 : std_logic;
	signal sig_1025 : std_logic;
	signal sig_1026 : std_logic;
	signal sig_1027 : std_logic;
	signal sig_1028 : std_logic;
	signal sig_1029 : std_logic;
	signal sig_1030 : std_logic;
	signal sig_1031 : std_logic;
	signal sig_1032 : std_logic;
	signal sig_1033 : std_logic;
	signal sig_1034 : std_logic;
	signal sig_1035 : std_logic;
	signal sig_1036 : std_logic;
	signal sig_1037 : std_logic;
	signal sig_1038 : std_logic;
	signal sig_1039 : std_logic;
	signal sig_1040 : std_logic;
	signal sig_1041 : std_logic;
	signal sig_1042 : std_logic;
	signal sig_1043 : std_logic;
	signal sig_1044 : std_logic;
	signal sig_1045 : std_logic;
	signal sig_1046 : std_logic;
	signal sig_1047 : std_logic;
	signal sig_1048 : std_logic;
	signal sig_1049 : std_logic;
	signal sig_1050 : std_logic;
	signal sig_1051 : std_logic;
	signal sig_1052 : std_logic;
	signal sig_1053 : std_logic;
	signal sig_1054 : std_logic;
	signal sig_1055 : std_logic;
	signal sig_1056 : std_logic;
	signal sig_1057 : std_logic;
	signal sig_1058 : std_logic;
	signal sig_1059 : std_logic;
	signal sig_1060 : std_logic;
	signal sig_1061 : std_logic;
	signal sig_1062 : std_logic;
	signal sig_1063 : std_logic;
	signal sig_1064 : std_logic;
	signal sig_1065 : std_logic;
	signal sig_1066 : std_logic;
	signal sig_1067 : std_logic;
	signal sig_1068 : std_logic;
	signal sig_1069 : std_logic;
	signal sig_1070 : std_logic;
	signal sig_1071 : std_logic;
	signal sig_1072 : std_logic;
	signal sig_1073 : std_logic;
	signal sig_1074 : std_logic;
	signal sig_1075 : std_logic;
	signal sig_1076 : std_logic;
	signal sig_1077 : std_logic;
	signal sig_1078 : std_logic;
	signal sig_1079 : std_logic;
	signal sig_1080 : std_logic;
	signal sig_1081 : std_logic;
	signal sig_1082 : std_logic;
	signal sig_1083 : std_logic;
	signal sig_1084 : std_logic;
	signal sig_1085 : std_logic;
	signal sig_1086 : std_logic;
	signal sig_1087 : std_logic;
	signal sig_1088 : std_logic;
	signal sig_1089 : std_logic;
	signal sig_1090 : std_logic;
	signal sig_1091 : std_logic;
	signal sig_1092 : std_logic;
	signal sig_1093 : std_logic;
	signal sig_1094 : std_logic;
	signal sig_1095 : std_logic;
	signal sig_1096 : std_logic;
	signal sig_1097 : std_logic;
	signal sig_1098 : std_logic;
	signal sig_1099 : std_logic;
	signal sig_1100 : std_logic;
	signal sig_1101 : std_logic;
	signal sig_1102 : std_logic;
	signal sig_1103 : std_logic;
	signal sig_1104 : std_logic;
	signal sig_1105 : std_logic;
	signal sig_1106 : std_logic;
	signal sig_1107 : std_logic;
	signal sig_1108 : std_logic;
	signal sig_1109 : std_logic;
	signal sig_1110 : std_logic;
	signal sig_1111 : std_logic;
	signal sig_1112 : std_logic;
	signal sig_1113 : std_logic;
	signal sig_1114 : std_logic;
	signal sig_1115 : std_logic;
	signal sig_1116 : std_logic;
	signal sig_1117 : std_logic;
	signal sig_1118 : std_logic;
	signal sig_1119 : std_logic;
	signal sig_1120 : std_logic;
	signal sig_1121 : std_logic;
	signal sig_1122 : std_logic;
	signal sig_1123 : std_logic;
	signal sig_1124 : std_logic;
	signal sig_1125 : std_logic;
	signal sig_1126 : std_logic;
	signal sig_1127 : std_logic;
	signal sig_1128 : std_logic;
	signal sig_1129 : std_logic;
	signal sig_1130 : std_logic;
	signal sig_1131 : std_logic;
	signal sig_1132 : std_logic;
	signal sig_1133 : std_logic;
	signal sig_1134 : std_logic;
	signal sig_1135 : std_logic;
	signal sig_1136 : std_logic;
	signal sig_1137 : std_logic;
	signal sig_1138 : std_logic;
	signal sig_1139 : std_logic;
	signal sig_1140 : std_logic;
	signal sig_1141 : std_logic;
	signal sig_1142 : std_logic;
	signal sig_1143 : std_logic;
	signal sig_1144 : std_logic;
	signal sig_1145 : std_logic;
	signal sig_1146 : std_logic;
	signal sig_1147 : std_logic;
	signal sig_1148 : std_logic;
	signal sig_1149 : std_logic;
	signal sig_1150 : std_logic;
	signal sig_1151 : std_logic;
	signal sig_1152 : std_logic;
	signal sig_1153 : std_logic;
	signal sig_1154 : std_logic;
	signal sig_1155 : std_logic;
	signal sig_1156 : std_logic;
	signal sig_1157 : std_logic;
	signal sig_1158 : std_logic;
	signal sig_1159 : std_logic;
	signal sig_1160 : std_logic;
	signal sig_1161 : std_logic;
	signal sig_1162 : std_logic;
	signal sig_1163 : std_logic;
	signal sig_1164 : std_logic;
	signal sig_1165 : std_logic;
	signal sig_1166 : std_logic;
	signal sig_1167 : std_logic;
	signal sig_1168 : std_logic;
	signal sig_1169 : std_logic;
	signal sig_1170 : std_logic;
	signal sig_1171 : std_logic;
	signal sig_1172 : std_logic;
	signal sig_1173 : std_logic;
	signal sig_1174 : std_logic;
	signal sig_1175 : std_logic;
	signal sig_1176 : std_logic;
	signal sig_1177 : std_logic;
	signal sig_1178 : std_logic;
	signal sig_1179 : std_logic;
	signal sig_1180 : std_logic;
	signal sig_1181 : std_logic;
	signal sig_1182 : std_logic;
	signal sig_1183 : std_logic;
	signal sig_1184 : std_logic;
	signal sig_1185 : std_logic;
	signal sig_1186 : std_logic;
	signal sig_1187 : std_logic;
	signal sig_1188 : std_logic;
	signal sig_1189 : std_logic;
	signal sig_1190 : std_logic;
	signal sig_1191 : std_logic;
	signal sig_1192 : std_logic;
	signal sig_1193 : std_logic;
	signal sig_1194 : std_logic;
	signal sig_1195 : std_logic;
	signal sig_1196 : std_logic;
	signal sig_1197 : std_logic;
	signal sig_1198 : std_logic;
	signal sig_1199 : std_logic;
	signal sig_1200 : std_logic;
	signal sig_1201 : std_logic;
	signal sig_1202 : std_logic;
	signal sig_1203 : std_logic;
	signal sig_1204 : std_logic;
	signal sig_1205 : std_logic;
	signal sig_1206 : std_logic;
	signal sig_1207 : std_logic;
	signal sig_1208 : std_logic;
	signal sig_1209 : std_logic;
	signal sig_1210 : std_logic;
	signal sig_1211 : std_logic;
	signal sig_1212 : std_logic;
	signal sig_1213 : std_logic;
	signal sig_1214 : std_logic;
	signal sig_1215 : std_logic;
	signal sig_1216 : std_logic;
	signal sig_1217 : std_logic;
	signal sig_1218 : std_logic;
	signal sig_1219 : std_logic;
	signal sig_1220 : std_logic;
	signal sig_1221 : std_logic;
	signal sig_1222 : std_logic;
	signal sig_1223 : std_logic;
	signal sig_1224 : std_logic;
	signal sig_1225 : std_logic;
	signal sig_1226 : std_logic;
	signal sig_1227 : std_logic;
	signal sig_1228 : std_logic;
	signal sig_1229 : std_logic;
	signal sig_1230 : std_logic;
	signal sig_1231 : std_logic;
	signal sig_1232 : std_logic;
	signal sig_1233 : std_logic;
	signal sig_1234 : std_logic;
	signal sig_1235 : std_logic;
	signal sig_1236 : std_logic;
	signal sig_1237 : std_logic;
	signal sig_1238 : std_logic;
	signal sig_1239 : std_logic;
	signal sig_1240 : std_logic;
	signal sig_1241 : std_logic;
	signal sig_1242 : std_logic;
	signal sig_1243 : std_logic;
	signal sig_1244 : std_logic;
	signal sig_1245 : std_logic;
	signal sig_1246 : std_logic;
	signal sig_1247 : std_logic;
	signal sig_1248 : std_logic;
	signal sig_1249 : std_logic;
	signal sig_1250 : std_logic;
	signal sig_1251 : std_logic;
	signal sig_1252 : std_logic;
	signal sig_1253 : std_logic;
	signal sig_1254 : std_logic;
	signal sig_1255 : std_logic;
	signal sig_1256 : std_logic;
	signal sig_1257 : std_logic;
	signal sig_1258 : std_logic;
	signal sig_1259 : std_logic;
	signal sig_1260 : std_logic;
	signal sig_1261 : std_logic;
	signal sig_1262 : std_logic;
	signal sig_1263 : std_logic;
	signal sig_1264 : std_logic;
	signal sig_1265 : std_logic;
	signal sig_1266 : std_logic;
	signal sig_1267 : std_logic;
	signal sig_1268 : std_logic;
	signal sig_1269 : std_logic;
	signal sig_1270 : std_logic;
	signal sig_1271 : std_logic;
	signal sig_1272 : std_logic;
	signal sig_1273 : std_logic;
	signal sig_1274 : std_logic;
	signal sig_1275 : std_logic;
	signal sig_1276 : std_logic;
	signal sig_1277 : std_logic;
	signal sig_1278 : std_logic;
	signal sig_1279 : std_logic;
	signal sig_1280 : std_logic;
	signal sig_1281 : std_logic;
	signal sig_1282 : std_logic;
	signal sig_1283 : std_logic;
	signal sig_1284 : std_logic;
	signal sig_1285 : std_logic;
	signal sig_1286 : std_logic;
	signal sig_1287 : std_logic;
	signal sig_1288 : std_logic;
	signal sig_1289 : std_logic;
	signal sig_1290 : std_logic;
	signal sig_1291 : std_logic;
	signal sig_1292 : std_logic;
	signal sig_1293 : std_logic;
	signal sig_1294 : std_logic;
	signal sig_1295 : std_logic;
	signal sig_1296 : std_logic;
	signal sig_1297 : std_logic;
	signal sig_1298 : std_logic;
	signal sig_1299 : std_logic;
	signal sig_1300 : std_logic;
	signal sig_1301 : std_logic;
	signal sig_1302 : std_logic;
	signal sig_1303 : std_logic;
	signal sig_1304 : std_logic;
	signal sig_1305 : std_logic;
	signal sig_1306 : std_logic;
	signal sig_1307 : std_logic;
	signal sig_1308 : std_logic;
	signal sig_1309 : std_logic;
	signal sig_1310 : std_logic;
	signal sig_1311 : std_logic;
	signal sig_1312 : std_logic;
	signal sig_1313 : std_logic;
	signal sig_1314 : std_logic;
	signal sig_1315 : std_logic;
	signal sig_1316 : std_logic;
	signal sig_1317 : std_logic;
	signal sig_1318 : std_logic;
	signal sig_1319 : std_logic;
	signal sig_1320 : std_logic;
	signal sig_1321 : std_logic;
	signal sig_1322 : std_logic;
	signal sig_1323 : std_logic;
	signal sig_1324 : std_logic;
	signal sig_1325 : std_logic;
	signal sig_1326 : std_logic;
	signal sig_1327 : std_logic;
	signal sig_1328 : std_logic;
	signal sig_1329 : std_logic;
	signal sig_1330 : std_logic;
	signal sig_1331 : std_logic;
	signal sig_1332 : std_logic;
	signal sig_1333 : std_logic;
	signal sig_1334 : std_logic;
	signal sig_1335 : std_logic;
	signal sig_1336 : std_logic;
	signal sig_1337 : std_logic;
	signal sig_1338 : std_logic;
	signal sig_1339 : std_logic;
	signal sig_1340 : std_logic;
	signal sig_1341 : std_logic;
	signal sig_1342 : std_logic;
	signal sig_1343 : std_logic;
	signal sig_1344 : std_logic;
	signal sig_1345 : std_logic;
	signal sig_1346 : std_logic;
	signal sig_1347 : std_logic;
	signal sig_1348 : std_logic;
	signal sig_1349 : std_logic;
	signal sig_1350 : std_logic;
	signal sig_1351 : std_logic;
	signal sig_1352 : std_logic;
	signal sig_1353 : std_logic;
	signal sig_1354 : std_logic;
	signal sig_1355 : std_logic;
	signal sig_1356 : std_logic;
	signal sig_1357 : std_logic;
	signal sig_1358 : std_logic;
	signal sig_1359 : std_logic;
	signal sig_1360 : std_logic;
	signal sig_1361 : std_logic;
	signal sig_1362 : std_logic;
	signal sig_1363 : std_logic;
	signal sig_1364 : std_logic;
	signal sig_1365 : std_logic;
	signal sig_1366 : std_logic;
	signal sig_1367 : std_logic;
	signal sig_1368 : std_logic;
	signal sig_1369 : std_logic;
	signal sig_1370 : std_logic;
	signal sig_1371 : std_logic;
	signal sig_1372 : std_logic;
	signal sig_1373 : std_logic;
	signal sig_1374 : std_logic;
	signal sig_1375 : std_logic;
	signal sig_1376 : std_logic;
	signal sig_1377 : std_logic;
	signal sig_1378 : std_logic;
	signal sig_1379 : std_logic;
	signal sig_1380 : std_logic;
	signal sig_1381 : std_logic;
	signal sig_1382 : std_logic;
	signal sig_1383 : std_logic;
	signal sig_1384 : std_logic;
	signal sig_1385 : std_logic;
	signal sig_1386 : std_logic;
	signal sig_1387 : std_logic;
	signal sig_1388 : std_logic;
	signal sig_1389 : std_logic;
	signal sig_1390 : std_logic;
	signal sig_1391 : std_logic;
	signal sig_1392 : std_logic;
	signal sig_1393 : std_logic;
	signal sig_1394 : std_logic;
	signal sig_1395 : std_logic;
	signal sig_1396 : std_logic;
	signal sig_1397 : std_logic;
	signal sig_1398 : std_logic;
	signal sig_1399 : std_logic;
	signal sig_1400 : std_logic;
	signal sig_1401 : std_logic;
	signal sig_1402 : std_logic;
	signal sig_1403 : std_logic;
	signal sig_1404 : std_logic;
	signal sig_1405 : std_logic;
	signal sig_1406 : std_logic;
	signal sig_1407 : std_logic;
	signal sig_1408 : std_logic;
	signal sig_1409 : std_logic;
	signal sig_1410 : std_logic;
	signal sig_1411 : std_logic;
	signal sig_1412 : std_logic;
	signal sig_1413 : std_logic;
	signal sig_1414 : std_logic;
	signal sig_1415 : std_logic;
	signal sig_1416 : std_logic;
	signal sig_1417 : std_logic;
	signal sig_1418 : std_logic;
	signal sig_1419 : std_logic;
	signal sig_1420 : std_logic;
	signal sig_1421 : std_logic;
	signal sig_1422 : std_logic;
	signal sig_1423 : std_logic;
	signal sig_1424 : std_logic;
	signal sig_1425 : std_logic;
	signal sig_1426 : std_logic;
	signal sig_1427 : std_logic;
	signal sig_1428 : std_logic;
	signal sig_1429 : std_logic;
	signal sig_1430 : std_logic;
	signal sig_1431 : std_logic;
	signal sig_1432 : std_logic;
	signal sig_1433 : std_logic;
	signal sig_1434 : std_logic;
	signal sig_1435 : std_logic;
	signal sig_1436 : std_logic;
	signal sig_1437 : std_logic;
	signal sig_1438 : std_logic;
	signal sig_1439 : std_logic;
	signal sig_1440 : std_logic;
	signal sig_1441 : std_logic;
	signal sig_1442 : std_logic;
	signal sig_1443 : std_logic;
	signal sig_1444 : std_logic;
	signal sig_1445 : std_logic;
	signal sig_1446 : std_logic;
	signal sig_1447 : std_logic;
	signal sig_1448 : std_logic;
	signal sig_1449 : std_logic;
	signal sig_1450 : std_logic;
	signal sig_1451 : std_logic;
	signal sig_1452 : std_logic;
	signal sig_1453 : std_logic;
	signal sig_1454 : std_logic;
	signal sig_1455 : std_logic;
	signal sig_1456 : std_logic;
	signal sig_1457 : std_logic;
	signal sig_1458 : std_logic;
	signal sig_1459 : std_logic;
	signal sig_1460 : std_logic;
	signal sig_1461 : std_logic;
	signal sig_1462 : std_logic;
	signal sig_1463 : std_logic;
	signal sig_1464 : std_logic;
	signal sig_1465 : std_logic;
	signal sig_1466 : std_logic;
	signal sig_1467 : std_logic;
	signal sig_1468 : std_logic;
	signal sig_1469 : std_logic;
	signal sig_1470 : std_logic;
	signal sig_1471 : std_logic;
	signal sig_1472 : std_logic;
	signal sig_1473 : std_logic;
	signal sig_1474 : std_logic;
	signal sig_1475 : std_logic;
	signal sig_1476 : std_logic;
	signal sig_1477 : std_logic;
	signal sig_1478 : std_logic;
	signal sig_1479 : std_logic;
	signal sig_1480 : std_logic;
	signal sig_1481 : std_logic;
	signal sig_1482 : std_logic;
	signal sig_1483 : std_logic;
	signal sig_1484 : std_logic;
	signal sig_1485 : std_logic;
	signal sig_1486 : std_logic;
	signal sig_1487 : std_logic;
	signal sig_1488 : std_logic;
	signal sig_1489 : std_logic;
	signal sig_1490 : std_logic;
	signal sig_1491 : std_logic;
	signal sig_1492 : std_logic;
	signal sig_1493 : std_logic;
	signal sig_1494 : std_logic;
	signal sig_1495 : std_logic;
	signal sig_1496 : std_logic;
	signal sig_1497 : std_logic;
	signal sig_1498 : std_logic;
	signal sig_1499 : std_logic;
	signal sig_1500 : std_logic;
	signal sig_1501 : std_logic;
	signal sig_1502 : std_logic;
	signal sig_1503 : std_logic;
	signal sig_1504 : std_logic;
	signal sig_1505 : std_logic;
	signal sig_1506 : std_logic;
	signal sig_1507 : std_logic;
	signal sig_1508 : std_logic;
	signal sig_1509 : std_logic;
	signal sig_1510 : std_logic;
	signal sig_1511 : std_logic;
	signal sig_1512 : std_logic;
	signal sig_1513 : std_logic;
	signal sig_1514 : std_logic;
	signal sig_1515 : std_logic;
	signal sig_1516 : std_logic;
	signal sig_1517 : std_logic;
	signal sig_1518 : std_logic;
	signal sig_1519 : std_logic;
	signal sig_1520 : std_logic;
	signal sig_1521 : std_logic;
	signal sig_1522 : std_logic;
	signal sig_1523 : std_logic;
	signal sig_1524 : std_logic;
	signal sig_1525 : std_logic;
	signal sig_1526 : std_logic;
	signal sig_1527 : std_logic;
	signal sig_1528 : std_logic;
	signal sig_1529 : std_logic;
	signal sig_1530 : std_logic;
	signal sig_1531 : std_logic;
	signal sig_1532 : std_logic;
	signal sig_1533 : std_logic;
	signal sig_1534 : std_logic;
	signal sig_1535 : std_logic;
	signal sig_1536 : std_logic;
	signal sig_1537 : std_logic;
	signal sig_1538 : std_logic;
	signal sig_1539 : std_logic;
	signal sig_1540 : std_logic;
	signal sig_1541 : std_logic;
	signal sig_1542 : std_logic;
	signal sig_1543 : std_logic;
	signal sig_1544 : std_logic;
	signal sig_1545 : std_logic;
	signal sig_1546 : std_logic;
	signal sig_1547 : std_logic;
	signal sig_1548 : std_logic;
	signal sig_1549 : std_logic;
	signal sig_1550 : std_logic;
	signal sig_1551 : std_logic;
	signal sig_1552 : std_logic;
	signal sig_1553 : std_logic;
	signal sig_1554 : std_logic;
	signal sig_1555 : std_logic;
	signal sig_1556 : std_logic;
	signal sig_1557 : std_logic;
	signal sig_1558 : std_logic;
	signal sig_1559 : std_logic;
	signal sig_1560 : std_logic;
	signal sig_1561 : std_logic;
	signal sig_1562 : std_logic;
	signal sig_1563 : std_logic;
	signal sig_1564 : std_logic;
	signal sig_1565 : std_logic;
	signal sig_1566 : std_logic;
	signal sig_1567 : std_logic;
	signal sig_1568 : std_logic;
	signal sig_1569 : std_logic;
	signal sig_1570 : std_logic;
	signal sig_1571 : std_logic;
	signal sig_1572 : std_logic;
	signal sig_1573 : std_logic;
	signal sig_1574 : std_logic;
	signal sig_1575 : std_logic;
	signal sig_1576 : std_logic;
	signal sig_1577 : std_logic;
	signal sig_1578 : std_logic;
	signal sig_1579 : std_logic;
	signal sig_1580 : std_logic;
	signal sig_1581 : std_logic;
	signal sig_1582 : std_logic;
	signal sig_1583 : std_logic;
	signal sig_1584 : std_logic;
	signal sig_1585 : std_logic_vector(40 downto 0);
	signal sig_1586 : std_logic;
	signal sig_1587 : std_logic_vector(40 downto 0);
	signal sig_1588 : std_logic_vector(40 downto 0);
	signal sig_1589 : std_logic;
	signal sig_1590 : std_logic_vector(40 downto 0);
	signal sig_1591 : std_logic;
	signal sig_1592 : std_logic_vector(40 downto 0);
	signal sig_1593 : std_logic;
	signal sig_1594 : std_logic;
	signal sig_1595 : std_logic;
	signal sig_1596 : std_logic_vector(40 downto 0);
	signal sig_1597 : std_logic;
	signal sig_1598 : std_logic;
	signal sig_1599 : std_logic;
	signal sig_1600 : std_logic_vector(40 downto 0);
	signal sig_1601 : std_logic;
	signal sig_1602 : std_logic;
	signal sig_1603 : std_logic;
	signal sig_1604 : std_logic;
	signal sig_1605 : std_logic;
	signal sig_1606 : std_logic;
	signal sig_1607 : std_logic;
	signal sig_1608 : std_logic;
	signal sig_1609 : std_logic_vector(38 downto 0);
	signal sig_1610 : std_logic_vector(38 downto 0);
	signal sig_1611 : std_logic_vector(24 downto 0);
	signal sig_1612 : std_logic_vector(38 downto 0);
	signal sig_1613 : std_logic_vector(31 downto 0);
	signal sig_1614 : std_logic_vector(40 downto 0);
	signal sig_1615 : std_logic;
	signal sig_1616 : std_logic;
	signal sig_1617 : std_logic;
	signal sig_1618 : std_logic;
	signal sig_1619 : std_logic_vector(40 downto 0);
	signal sig_1620 : std_logic;
	signal sig_1621 : std_logic_vector(40 downto 0);
	signal sig_1622 : std_logic;
	signal sig_1623 : std_logic;
	signal sig_1624 : std_logic_vector(40 downto 0);
	signal sig_1625 : std_logic;
	signal sig_1626 : std_logic_vector(31 downto 0);
	signal sig_1627 : std_logic_vector(40 downto 0);
	signal sig_1628 : std_logic_vector(40 downto 0);
	signal sig_1629 : std_logic_vector(31 downto 0);
	signal sig_1630 : std_logic_vector(5 downto 0);
	signal sig_1631 : std_logic_vector(7 downto 0);
	signal sig_1632 : std_logic_vector(31 downto 0);
	signal sig_1633 : std_logic_vector(31 downto 0);
	signal sig_1634 : std_logic_vector(31 downto 0);
	signal sig_1635 : std_logic_vector(31 downto 0);
	signal sig_1636 : std_logic_vector(31 downto 0);
	signal sig_1637 : std_logic_vector(31 downto 0);
	signal sig_1638 : std_logic_vector(20 downto 0);
	signal sig_1639 : std_logic_vector(31 downto 0);
	signal sig_1640 : std_logic_vector(31 downto 0);
	signal sig_1641 : std_logic_vector(31 downto 0);
	signal sig_1642 : std_logic_vector(31 downto 0);
	signal sig_1643 : std_logic_vector(31 downto 0);
	signal sig_1644 : std_logic_vector(7 downto 0);
	signal sig_1645 : std_logic_vector(5 downto 0);
	signal sig_1646 : std_logic_vector(7 downto 0);
	signal sig_1647 : std_logic_vector(31 downto 0);
	signal sig_1648 : std_logic_vector(31 downto 0);
	signal sig_1649 : std_logic_vector(8 downto 0);
	signal sig_1650 : std_logic_vector(8 downto 0);
	signal sig_1651 : std_logic_vector(31 downto 0);
	signal sig_1652 : std_logic_vector(31 downto 0);
	signal sig_1653 : std_logic_vector(8 downto 0);
	signal sig_1654 : std_logic_vector(8 downto 0);
	signal sig_1655 : std_logic_vector(31 downto 0);
	signal sig_1656 : std_logic_vector(31 downto 0);
	signal sig_1657 : std_logic_vector(31 downto 0);
	signal sig_1658 : std_logic_vector(31 downto 0);
	signal sig_1659 : std_logic_vector(31 downto 0);
	signal sig_1660 : std_logic_vector(31 downto 0);
	signal sig_1661 : std_logic_vector(31 downto 0);
	signal sig_1662 : std_logic;
	signal sig_1663 : std_logic_vector(1 downto 0);
	signal sig_1664 : std_logic_vector(7 downto 0);
	signal sig_1665 : std_logic_vector(7 downto 0);
	signal sig_1666 : std_logic_vector(40 downto 0);
	signal sig_1667 : std_logic_vector(40 downto 0);
	signal sig_1668 : std_logic_vector(40 downto 0);
	signal sig_1669 : std_logic;
	signal sig_1670 : std_logic;
	signal sig_1671 : std_logic_vector(31 downto 0);
	signal sig_1672 : std_logic_vector(31 downto 0);
	signal sig_1673 : std_logic_vector(40 downto 0);
	signal sig_1674 : std_logic_vector(40 downto 0);
	signal sig_1675 : std_logic_vector(40 downto 0);
	signal sig_1676 : std_logic_vector(40 downto 0);
	signal sig_1677 : std_logic_vector(31 downto 0);
	signal sig_1678 : std_logic_vector(31 downto 0);
	signal sig_1679 : std_logic_vector(40 downto 0);
	signal sig_1680 : std_logic_vector(31 downto 0);
	signal sig_1681 : std_logic_vector(31 downto 0);
	signal sig_1682 : std_logic_vector(31 downto 0);
	signal sig_1683 : std_logic_vector(31 downto 0);
	signal sig_1684 : std_logic_vector(31 downto 0);
	signal sig_1685 : std_logic_vector(31 downto 0);
	signal sig_1686 : std_logic_vector(31 downto 0);
	signal sig_1687 : std_logic_vector(31 downto 0);
	signal sig_1688 : std_logic_vector(24 downto 0);
	signal sig_1689 : std_logic_vector(40 downto 0);
	signal sig_1690 : std_logic_vector(31 downto 0);
	signal sig_1691 : std_logic_vector(9 downto 0);
	signal sig_1692 : std_logic_vector(8 downto 0);
	signal sig_1693 : std_logic_vector(14 downto 0);
	signal sig_1694 : std_logic_vector(14 downto 0);
	signal sig_1695 : std_logic_vector(6 downto 0);
	signal sig_1696 : std_logic_vector(6 downto 0);
	signal sig_1697 : std_logic_vector(6 downto 0);
	signal sig_1698 : std_logic_vector(6 downto 0);
	signal sig_1699 : std_logic_vector(6 downto 0);
	signal sig_1700 : std_logic_vector(6 downto 0);
	signal sig_1701 : std_logic_vector(6 downto 0);
	signal sig_1702 : std_logic_vector(6 downto 0);
	signal sig_1703 : std_logic_vector(9 downto 0);
	signal sig_1704 : std_logic_vector(6 downto 0);
	signal sig_1705 : std_logic_vector(9 downto 0);
	signal sig_1706 : std_logic_vector(6 downto 0);
	signal sig_1707 : std_logic_vector(7 downto 0);
	signal sig_1708 : std_logic_vector(31 downto 0);
	signal sig_1709 : std_logic_vector(31 downto 0);
	signal sig_1710 : std_logic_vector(31 downto 0);
	signal sig_1711 : std_logic_vector(31 downto 0);
	signal sig_1712 : std_logic_vector(31 downto 0);
	signal sig_1713 : std_logic_vector(31 downto 0);
	signal sig_1714 : std_logic_vector(31 downto 0);
	signal sig_1715 : std_logic_vector(31 downto 0);
	signal sig_1716 : std_logic_vector(31 downto 0);

	-- Other inlined components

	signal mux_967 : std_logic_vector(31 downto 0);
	signal and_976 : std_logic;
	signal and_982 : std_logic_vector(31 downto 0);
	signal and_983 : std_logic_vector(27 downto 0);
	signal and_984 : std_logic_vector(31 downto 0);
	signal mux_689 : std_logic_vector(31 downto 0);
	signal mux_690 : std_logic_vector(6 downto 0);
	signal mux_691 : std_logic_vector(6 downto 0);
	signal and_853 : std_logic_vector(31 downto 0);
	signal izigzagmatrix_i : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_233 : std_logic_vector(31 downto 0);
	signal izigzagmatrix_out_idx : std_logic_vector(31 downto 0) := (others => '0');
	signal iquantize_qidx : std_logic_vector(1 downto 0) := (others => '0');
	signal write8_u8 : std_logic_vector(7 downto 0) := (others => '0');
	signal p_jinfo_image_height : std_logic_vector(15 downto 0) := (others => '0');
	signal p_jinfo_image_width : std_logic_vector(15 downto 0) := (others => '0');
	signal mux_671 : std_logic_vector(31 downto 0);
	signal p_jinfo_num_components : std_logic_vector(7 downto 0) := (others => '0');
	signal p_jinfo_smp_fact : std_logic_vector(1 downto 0) := (others => '0');
	signal mux_665 : std_logic_vector(1 downto 0);
	signal mux_663 : std_logic_vector(31 downto 0);
	signal mux_664 : std_logic_vector(1 downto 0);
	signal mux_659 : std_logic_vector(31 downto 0);
	signal mux_660 : std_logic_vector(1 downto 0);
	signal mux_661 : std_logic_vector(1 downto 0);
	signal mux_652 : std_logic_vector(12 downto 0);
	signal mux_648 : std_logic_vector(31 downto 0);
	signal mux_633 : std_logic_vector(31 downto 0);
	signal mux_622 : std_logic_vector(31 downto 0);
	signal mux_614 : std_logic_vector(31 downto 0);
	signal mux_616 : std_logic_vector(31 downto 0);
	signal p_jinfo_mcuwidth : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_602 : std_logic_vector(31 downto 0);
	signal p_jinfo_mcuheight : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_600 : std_logic_vector(31 downto 0);
	signal p_jinfo_nummcu : std_logic_vector(31 downto 0) := (others => '0');
	signal i_jinfo_jpeg_data : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_593 : std_logic_vector(31 downto 0);
	signal curhuffreadbuf_idx : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_587 : std_logic_vector(31 downto 0);
	signal outdata_image_width : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_585 : std_logic_vector(15 downto 0);
	signal outdata_image_height : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_580 : std_logic_vector(7 downto 0);
	signal mux_569 : std_logic_vector(7 downto 0);
	signal mux_567 : std_logic_vector(31 downto 0);
	signal mux_568 : std_logic_vector(7 downto 0);
	signal mux_563 : std_logic_vector(8 downto 0);
	signal mux_565 : std_logic_vector(8 downto 0);
	signal mux_561 : std_logic_vector(31 downto 0);
	signal mux_562 : std_logic_vector(8 downto 0);
	signal mux_557 : std_logic_vector(31 downto 0);
	signal mux_558 : std_logic_vector(5 downto 0);
	signal mux_559 : std_logic_vector(5 downto 0);
	signal mux_555 : std_logic_vector(31 downto 0);
	signal mux_551 : std_logic_vector(31 downto 0);
	signal mux_553 : std_logic_vector(31 downto 0);
	signal mux_549 : std_logic_vector(31 downto 0);
	signal mux_545 : std_logic_vector(31 downto 0);
	signal mux_547 : std_logic_vector(31 downto 0);
	signal mux_543 : std_logic_vector(31 downto 0);
	signal mux_731 : std_logic_vector(7 downto 0);
	signal mux_727 : std_logic_vector(6 downto 0);
	signal mux_723 : std_logic_vector(9 downto 0);
	signal mux_719 : std_logic_vector(6 downto 0);
	signal mux_539 : std_logic_vector(31 downto 0);
	signal mux_541 : std_logic_vector(31 downto 0);
	signal mux_537 : std_logic_vector(31 downto 0);
	signal mux_533 : std_logic_vector(31 downto 0);
	signal mux_535 : std_logic_vector(31 downto 0);
	signal mux_715 : std_logic_vector(9 downto 0);
	signal mux_711 : std_logic;
	signal mux_705 : std_logic_vector(31 downto 0);
	signal mux_706 : std_logic_vector(6 downto 0);
	signal mux_707 : std_logic_vector(6 downto 0);
	signal mux_531 : std_logic_vector(31 downto 0);
	signal mux_529 : std_logic_vector(31 downto 0);
	signal mux_695 : std_logic;
	signal mux_524 : std_logic_vector(4 downto 0);
	signal mux_521 : std_logic_vector(31 downto 0);
	signal readbuf_idx : std_logic_vector(31 downto 0) := (others => '0');
	signal read_byte : std_logic_vector(7 downto 0) := (others => '0');
	signal read_word : std_logic_vector(15 downto 0) := (others => '0');
	signal read_word_c : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_519 : std_logic_vector(31 downto 0);
	signal mux_517 : std_logic_vector(7 downto 0);
	signal next_marker : std_logic_vector(7 downto 0) := (others => '0');
	signal next_marker_c : std_logic_vector(7 downto 0) := (others => '0');
	signal get_sof_ci : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_507 : std_logic_vector(31 downto 0);
	signal mux_505 : std_logic_vector(31 downto 0);
	signal get_sof_i_comp_info_id : std_logic_vector(1 downto 0) := (others => '0');
	signal mux_501 : std_logic_vector(31 downto 0);
	signal get_sof_i_comp_info_h_samp_factor : std_logic_vector(1 downto 0) := (others => '0');
	signal get_sof_i_comp_info_quant_tbl_no : std_logic_vector(1 downto 0) := (others => '0');
	signal mux_492 : std_logic_vector(31 downto 0);
	signal mux_488 : std_logic_vector(31 downto 0);
	signal mux_490 : std_logic_vector(31 downto 0);
	signal get_sos_num_comp : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_486 : std_logic_vector(31 downto 0);
	signal get_sos_i : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_482 : std_logic_vector(31 downto 0);
	signal mux_484 : std_logic_vector(31 downto 0);
	signal get_sos_c : std_logic := '0';
	signal mux_480 : std_logic_vector(31 downto 0);
	signal get_sos_cc : std_logic_vector(7 downto 0) := (others => '0');
	signal mux_476 : std_logic_vector(31 downto 0);
	signal mux_478 : std_logic_vector(8 downto 0);
	signal get_sos_ci : std_logic_vector(31 downto 0) := (others => '0');
	signal get_sos_j : std_logic_vector(31 downto 0) := (others => '0');
	signal get_sos_i_comp_info_dc_tbl_no : std_logic_vector(1 downto 0) := (others => '0');
	signal get_dht_length : std_logic_vector(31 downto 0) := (others => '0');
	signal get_dht_index : std_logic := '0';
	signal mux_459 : std_logic_vector(31 downto 0);
	signal get_dht_i : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_455 : std_logic_vector(31 downto 0);
	signal mux_457 : std_logic_vector(31 downto 0);
	signal get_dht_count : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_453 : std_logic_vector(31 downto 0);
	signal mux_449 : std_logic_vector(31 downto 0);
	signal mux_451 : std_logic_vector(31 downto 0);
	signal get_dht_is_ac : std_logic := '0';
	signal get_dqt_length : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_447 : std_logic_vector(31 downto 0);
	signal get_dqt_prec : std_logic_vector(3 downto 0) := (others => '0');
	signal mux_443 : std_logic_vector(31 downto 0);
	signal mux_445 : std_logic_vector(8 downto 0);
	signal get_dqt_num : std_logic_vector(1 downto 0) := (others => '0');
	signal get_dqt_i : std_logic_vector(31 downto 0) := (others => '0');
	signal get_dqt_tmp : std_logic_vector(15 downto 0) := (others => '0');
	signal read_markers_unread_marker : std_logic_vector(7 downto 0) := (others => '0');
	signal read_markers_sow_soi : std_logic := '0';
	signal mux_430 : std_logic_vector(31 downto 0);
	signal mux_422 : std_logic_vector(31 downto 0);
	signal mux_424 : std_logic_vector(31 downto 0);
	signal chenidct_i : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_416 : std_logic_vector(31 downto 0);
	signal chenidct_aidx : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_a0 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_410 : std_logic_vector(31 downto 0);
	signal chenidct_a1 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_408 : std_logic_vector(31 downto 0);
	signal chenidct_a2 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_a3 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_b0 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_398 : std_logic_vector(31 downto 0);
	signal mux_400 : std_logic_vector(31 downto 0);
	signal chenidct_b1 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_b2 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_392 : std_logic_vector(31 downto 0);
	signal mux_394 : std_logic_vector(31 downto 0);
	signal chenidct_b3 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_c0 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_c1 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_c2 : std_logic_vector(31 downto 0) := (others => '0');
	signal chenidct_c3 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_378 : std_logic_vector(7 downto 0);
	signal mux_379 : std_logic_vector(9 downto 0);
	signal mux_375 : std_logic_vector(1 downto 0);
	signal mux_373 : std_logic_vector(1 downto 0);
	signal current_read_byte : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_365 : std_logic_vector(31 downto 0);
	signal mux_367 : std_logic_vector(31 downto 0);
	signal read_position : std_logic_vector(31 downto 0) := "11111111111111111111111111111111";
	signal pgetc : std_logic_vector(7 downto 0) := (others => '0');
	signal pgetc_temp : std_logic_vector(7 downto 0) := (others => '0');
	signal buf_getb : std_logic := '0';
	signal buf_getv : std_logic_vector(31 downto 0) := (others => '0');
	signal buf_getv_n : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_363 : std_logic_vector(31 downto 0);
	signal buf_getv_p : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_359 : std_logic_vector(31 downto 0);
	signal mux_361 : std_logic_vector(31 downto 0);
	signal buf_getv_rv : std_logic_vector(31 downto 0) := (others => '0');
	signal huff_make_dhuff_tb_ac : std_logic_vector(31 downto 0) := (others => '0');
	signal huff_make_dhuff_tb_ac_tbl_no : std_logic := '0';
	signal huff_make_dhuff_tb_ac_p_dhtbl_ml : std_logic_vector(31 downto 0) := (others => '0');
	signal huff_make_dhuff_tb_ac_i_c0 : std_logic_vector(31 downto 0) := (others => '0');
	signal huff_make_dhuff_tb_ac_j : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_347 : std_logic_vector(31 downto 0);
	signal huff_make_dhuff_tb_ac_p : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_345 : std_logic_vector(31 downto 0);
	signal huff_make_dhuff_tb_ac_code : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_341 : std_logic_vector(2 downto 0);
	signal mux_343 : std_logic_vector(1 downto 0);
	signal huff_make_dhuff_tb_ac_size : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_339 : std_logic_vector(2 downto 0);
	signal huff_make_dhuff_tb_ac_l : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_335 : std_logic_vector(31 downto 0);
	signal mux_337 : std_logic_vector(2 downto 0);
	signal mux_333 : std_logic_vector(31 downto 0);
	signal mux_331 : std_logic_vector(31 downto 0);
	signal huff_make_dhuff_tb_dc : std_logic_vector(31 downto 0) := (others => '0');
	signal huff_make_dhuff_tb_dc_tbl_no : std_logic := '0';
	signal huff_make_dhuff_tb_dc_p_dhtbl_ml : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_323 : std_logic_vector(5 downto 0);
	signal huff_make_dhuff_tb_dc_i_c0 : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_320 : std_logic_vector(31 downto 0);
	signal mux_322 : std_logic_vector(31 downto 0);
	signal huff_make_dhuff_tb_dc_j : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_317 : std_logic_vector(1 downto 0);
	signal huff_make_dhuff_tb_dc_p : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_314 : std_logic_vector(31 downto 0);
	signal mux_315 : std_logic_vector(31 downto 0);
	signal mux_316 : std_logic_vector(31 downto 0);
	signal huff_make_dhuff_tb_dc_code : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_313 : std_logic_vector(8 downto 0);
	signal huff_make_dhuff_tb_dc_size : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_308 : std_logic_vector(2 downto 0);
	signal huff_make_dhuff_tb_dc_l : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_306 : std_logic_vector(40 downto 0);
	signal mux_307 : std_logic_vector(40 downto 0);
	signal mux_302 : std_logic_vector(40 downto 0);
	signal mux_303 : std_logic_vector(40 downto 0);
	signal decodehuffman_ac : std_logic_vector(31 downto 0) := (others => '0');
	signal decodehuffman_ac_tbl_no : std_logic := '0';
	signal mux_294 : std_logic_vector(1 downto 0);
	signal decodehuffman_ac_dhuff_ml : std_logic_vector(5 downto 0) := (others => '0');
	signal mux_290 : std_logic_vector(40 downto 0);
	signal mux_291 : std_logic_vector(40 downto 0);
	signal mux_292 : std_logic_vector(31 downto 0);
	signal decodehuffman_ac_code : std_logic_vector(31 downto 0) := (others => '0');
	signal decodehuffman_ac_l : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_286 : std_logic_vector(31 downto 0);
	signal decodehuffman_ac_p : std_logic_vector(8 downto 0) := (others => '0');
	signal decodehuffman_dc : std_logic_vector(31 downto 0) := (others => '0');
	signal decodehuffman_dc_tbl_no : std_logic := '0';
	signal decodehuffman_dc_dhuff_ml : std_logic_vector(5 downto 0) := (others => '0');
	signal mux_275 : std_logic_vector(31 downto 0);
	signal decodehuffman_dc_code : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_272 : std_logic_vector(38 downto 0);
	signal mux_274 : std_logic_vector(31 downto 0);
	signal decodehuffman_dc_l : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_271 : std_logic_vector(38 downto 0);
	signal decodehuffman_dc_p : std_logic_vector(8 downto 0) := (others => '0');
	signal decodehuffmcu_bufdim1 : std_logic_vector(1 downto 0) := (others => '0');
	signal mux_266 : std_logic_vector(38 downto 0);
	signal mux_265 : std_logic_vector(38 downto 0);
	signal decodehuffmcu_s : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_260 : std_logic_vector(38 downto 0);
	signal mux_261 : std_logic_vector(38 downto 0);
	signal mux_262 : std_logic_vector(31 downto 0);
	signal decodehuffmcu_diff : std_logic_vector(31 downto 0) := (others => '0');
	signal mux_257 : std_logic_vector(31 downto 0);
	signal decodehuffmcu_tbl_no : std_logic := '0';
	signal decodehuffmcu_i : std_logic_vector(31 downto 0) := (others => '0');
	signal decodehuffmcu_k : std_logic_vector(31 downto 0) := (others => '0');
	signal decodehuffmcu_n : std_logic_vector(27 downto 0) := (others => '0');
	signal writeoneblock_outidx : std_logic_vector(1 downto 0) := (others => '0');
	signal writeoneblock_indim1 : std_logic_vector(1 downto 0) := (others => '0');
	signal writeoneblock_width : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_height : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_voffs : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_hoffs : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_i : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_e : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_inidx : std_logic_vector(31 downto 0) := (others => '0');
	signal writeoneblock_diff : std_logic_vector(12 downto 0) := (others => '0');
	signal writeblock_i : std_logic_vector(1 downto 0) := (others => '0');
	signal write4blocks_i : std_logic_vector(1 downto 0) := (others => '0');
	signal write4blocks_voffs : std_logic_vector(31 downto 0) := (others => '0');
	signal write4blocks_hoffs : std_logic_vector(31 downto 0) := (others => '0');
	signal yuvtorgb_p : std_logic_vector(1 downto 0) := (others => '0');
	signal yuvtorgb_yidx : std_logic_vector(2 downto 0) := (others => '0');
	signal yuvtorgb_uidx : std_logic_vector(2 downto 0) := (others => '0');
	signal yuvtorgb_vidx : std_logic_vector(2 downto 0) := (others => '0');
	signal yuvtorgb_r : std_logic_vector(31 downto 0) := (others => '0');
	signal yuvtorgb_g : std_logic_vector(31 downto 0) := (others => '0');
	signal yuvtorgb_b : std_logic_vector(31 downto 0) := (others => '0');
	signal yuvtorgb_y : std_logic_vector(23 downto 0) := (others => '0');
	signal yuvtorgb_u : std_logic_vector(30 downto 0) := (others => '0');
	signal yuvtorgb_v : std_logic_vector(31 downto 0) := (others => '0');
	signal yuvtorgb_i : std_logic_vector(31 downto 0) := (others => '0');
	signal decode_block_comp_no : std_logic_vector(1 downto 0) := (others => '0');
	signal decode_block_out_buf_idx : std_logic_vector(2 downto 0) := (others => '0');
	signal decode_block_in_buf_idx : std_logic_vector(1 downto 0) := (others => '0');
	signal decode_start_i : std_logic_vector(31 downto 0) := (others => '0');
	signal decode_start_currentmcu : std_logic_vector(31 downto 0) := (others => '0');
	signal nand_786 : std_logic;
	signal or_845 : std_logic_vector(31 downto 0);
	signal or_854 : std_logic_vector(31 downto 0);
	signal or_866 : std_logic_vector(31 downto 0);
	signal jpeg2bmp_main_i : std_logic_vector(31 downto 0) := (others => '0');
	signal jpeg2bmp_main_j : std_logic_vector(31 downto 0) := (others => '0');
	signal read8_ret0_195 : std_logic_vector(7 downto 0) := (others => '0');
	signal and_785 : std_logic;
	signal and_801 : std_logic_vector(31 downto 0);
	signal mux_761 : std_logic_vector(8 downto 0);
	signal mux_782 : std_logic_vector(31 downto 0);
	signal or_802 : std_logic_vector(23 downto 0);
	signal and_803 : std_logic_vector(31 downto 0);
	signal mux_822 : std_logic_vector(31 downto 0);
	signal mux_823 : std_logic_vector(31 downto 0);
	signal mux_776 : std_logic_vector(31 downto 0);
	signal mux_820 : std_logic_vector(31 downto 0);
	signal mux_824 : std_logic_vector(31 downto 0);
	signal mux_825 : std_logic_vector(31 downto 0);
	signal mux_760 : std_logic_vector(31 downto 0);
	signal and_789 : std_logic;
	signal mux_759 : std_logic_vector(5 downto 0);
	signal mux_768 : std_logic_vector(31 downto 0);
	signal mux_757 : std_logic_vector(7 downto 0);
	signal mux_773 : std_logic_vector(7 downto 0);
	signal mux_762 : std_logic_vector(31 downto 0);
	signal mux_766 : std_logic_vector(31 downto 0);
	signal mux_781 : std_logic_vector(31 downto 0);
	signal mux_797 : std_logic_vector(31 downto 0);
	signal mux_821 : std_logic_vector(31 downto 0);
	signal mux_826 : std_logic_vector(31 downto 0);
	signal mux_778 : std_logic_vector(31 downto 0);
	signal mux_827 : std_logic_vector(31 downto 0);
	signal mux_815 : std_logic_vector(31 downto 0);
	signal mux_798 : std_logic_vector(31 downto 0);
	signal mux_816 : std_logic_vector(31 downto 0);
	signal mux_817 : std_logic_vector(31 downto 0);
	signal mux_777 : std_logic_vector(31 downto 0);
	signal mux_819 : std_logic_vector(31 downto 0);
	signal mux_783 : std_logic_vector(31 downto 0);
	signal mux_795 : std_logic_vector(31 downto 0);
	signal mux_796 : std_logic_vector(31 downto 0);
	signal mux_805 : std_logic_vector(31 downto 0);
	signal mux_806 : std_logic_vector(31 downto 0);
	signal mux_807 : std_logic_vector(31 downto 0);
	signal mux_808 : std_logic_vector(31 downto 0);
	signal mux_809 : std_logic_vector(31 downto 0);
	signal mux_810 : std_logic_vector(31 downto 0);
	signal mux_811 : std_logic_vector(31 downto 0);
	signal mux_812 : std_logic_vector(31 downto 0);
	signal mux_813 : std_logic_vector(31 downto 0);
	signal mux_814 : std_logic_vector(31 downto 0);
	signal mux_818 : std_logic_vector(31 downto 0);
	signal mux_828 : std_logic_vector(31 downto 0);
	signal mux_829 : std_logic_vector(31 downto 0);
	signal mux_830 : std_logic_vector(31 downto 0);
	signal mux_831 : std_logic_vector(31 downto 0);
	signal mux_832 : std_logic_vector(31 downto 0);
	signal mux_836 : std_logic_vector(31 downto 0);
	signal mux_837 : std_logic_vector(31 downto 0);
	signal mux_839 : std_logic_vector(31 downto 0);
	signal mux_840 : std_logic_vector(31 downto 0);
	signal mux_841 : std_logic_vector(31 downto 0);
	signal mux_842 : std_logic_vector(31 downto 0);
	signal mux_843 : std_logic_vector(31 downto 0);
	signal mux_856 : std_logic_vector(31 downto 0);
	signal and_864 : std_logic;
	signal mux_870 : std_logic_vector(31 downto 0);
	signal mux_872 : std_logic_vector(1 downto 0);
	signal mux_875 : std_logic_vector(31 downto 0);
	signal mux_891 : std_logic_vector(31 downto 0);
	signal mux_892 : std_logic_vector(31 downto 0);
	signal mux_893 : std_logic_vector(31 downto 0);
	signal mux_894 : std_logic_vector(31 downto 0);
	signal mux_895 : std_logic_vector(31 downto 0);
	signal mux_896 : std_logic_vector(31 downto 0);
	signal mux_897 : std_logic_vector(31 downto 0);
	signal mux_898 : std_logic_vector(31 downto 0);
	signal mux_899 : std_logic_vector(31 downto 0);
	signal mux_900 : std_logic_vector(31 downto 0);
	signal mux_901 : std_logic_vector(31 downto 0);
	signal mux_902 : std_logic_vector(31 downto 0);
	signal mux_903 : std_logic_vector(31 downto 0);
	signal mux_904 : std_logic_vector(31 downto 0);
	signal mux_905 : std_logic_vector(31 downto 0);
	signal mux_906 : std_logic_vector(31 downto 0);
	signal mux_907 : std_logic_vector(31 downto 0);
	signal mux_908 : std_logic_vector(31 downto 0);
	signal mux_917 : std_logic_vector(31 downto 0);
	signal mux_918 : std_logic_vector(31 downto 0);
	signal mux_924 : std_logic_vector(31 downto 0);
	signal mux_925 : std_logic_vector(31 downto 0);
	signal mux_928 : std_logic_vector(31 downto 0);
	signal mux_929 : std_logic_vector(31 downto 0);
	signal mux_931 : std_logic_vector(31 downto 0);
	signal mux_932 : std_logic_vector(31 downto 0);
	signal mux_934 : std_logic_vector(31 downto 0);
	signal mux_935 : std_logic_vector(31 downto 0);
	signal mux_936 : std_logic_vector(31 downto 0);
	signal mux_937 : std_logic_vector(31 downto 0);
	signal mux_938 : std_logic_vector(31 downto 0);
	signal mux_939 : std_logic_vector(31 downto 0);
	signal mux_941 : std_logic_vector(31 downto 0);
	signal mux_944 : std_logic_vector(31 downto 0);
	signal mux_945 : std_logic_vector(31 downto 0);
	signal mux_946 : std_logic_vector(31 downto 0);
	signal mux_833 : std_logic_vector(31 downto 0);
	signal mux_834 : std_logic_vector(31 downto 0);
	signal mux_835 : std_logic_vector(31 downto 0);
	signal mux_838 : std_logic_vector(31 downto 0);
	signal mux_844 : std_logic_vector(31 downto 0);
	signal mux_857 : std_logic_vector(31 downto 0);
	signal mux_858 : std_logic_vector(31 downto 0);
	signal mux_859 : std_logic_vector(31 downto 0);
	signal mux_874 : std_logic_vector(31 downto 0);
	signal mux_888 : std_logic_vector(31 downto 0);
	signal mux_889 : std_logic_vector(31 downto 0);
	signal mux_913 : std_logic_vector(31 downto 0);
	signal mux_914 : std_logic_vector(31 downto 0);
	signal mux_915 : std_logic_vector(31 downto 0);
	signal mux_916 : std_logic_vector(31 downto 0);
	signal mux_933 : std_logic_vector(31 downto 0);
	signal mux_940 : std_logic_vector(31 downto 0);
	signal mux_942 : std_logic_vector(31 downto 0);
	signal and_867 : std_logic;
	signal mux_909 : std_logic_vector(31 downto 0);
	signal mux_910 : std_logic_vector(31 downto 0);
	signal mux_911 : std_logic_vector(31 downto 0);
	signal mux_920 : std_logic_vector(31 downto 0);
	signal mux_921 : std_logic_vector(31 downto 0);
	signal mux_926 : std_logic_vector(31 downto 0);
	signal mux_927 : std_logic_vector(31 downto 0);
	signal mux_943 : std_logic_vector(31 downto 0);
	signal mux_886 : std_logic;
	signal mux_922 : std_logic_vector(31 downto 0);
	signal mux_923 : std_logic_vector(31 downto 0);
	signal mux_930 : std_logic_vector(31 downto 0);
	signal mux_987 : std_logic_vector(31 downto 0);
	signal and_860 : std_logic_vector(31 downto 0);
	signal and_881 : std_logic_vector(31 downto 0);
	signal and_884 : std_logic_vector(31 downto 0);
	signal mux_890 : std_logic_vector(31 downto 0);
	signal mux_912 : std_logic_vector(31 downto 0);
	signal mux_919 : std_logic_vector(31 downto 0);
	signal mux_948 : std_logic_vector(31 downto 0);
	signal mux_949 : std_logic_vector(31 downto 0);
	signal mux_950 : std_logic_vector(31 downto 0);
	signal and_862 : std_logic;
	signal mux_953 : std_logic_vector(31 downto 0);
	signal mux_954 : std_logic_vector(31 downto 0);
	signal mux_955 : std_logic_vector(31 downto 0);
	signal mux_951 : std_logic_vector(31 downto 0);
	signal mux_952 : std_logic_vector(31 downto 0);
	signal mux_959 : std_logic_vector(31 downto 0);
	signal mux_960 : std_logic_vector(31 downto 0);
	signal mux_961 : std_logic_vector(31 downto 0);
	signal mux_965 : std_logic_vector(31 downto 0);
	signal mux_966 : std_logic_vector(31 downto 0);
	signal and_876 : std_logic_vector(7 downto 0);
	signal mux_956 : std_logic_vector(31 downto 0);
	signal mux_957 : std_logic_vector(31 downto 0);
	signal mux_947 : std_logic_vector(31 downto 0);
	signal mux_968 : std_logic_vector(31 downto 0);
	signal mux_969 : std_logic_vector(31 downto 0);
	signal mux_970 : std_logic_vector(31 downto 0);
	signal mux_980 : std_logic_vector(31 downto 0);
	signal mux_981 : std_logic_vector(31 downto 0);
	signal mux_958 : std_logic_vector(31 downto 0);
	signal and_963 : std_logic;
	signal mux_986 : std_logic_vector(31 downto 0);
	signal mux_988 : std_logic_vector(31 downto 0);
	signal mux_989 : std_logic_vector(31 downto 0);

	-- This utility function is used for inlining MUX behaviour

	-- Little utility function to ease concatenation of an std_logic
	-- and explicitely return an std_logic_vector
	function repeat(N: natural; B: std_logic) return std_logic_vector is
		variable result: std_logic_vector(N-1 downto 0);
	begin
		result := (others => B);
		return result;
	end;

begin

	-- Instantiation of components

	cmp_869_i : cmp_869 port map (
		eq => sig_1670,
		in1 => sig_1665,
		in0 => get_sos_cc
	);

	cmp_978_i : cmp_978 port map (
		ne => augh_test_132,
		in1 => sig_1633,
		in0 => huff_make_dhuff_tb_dc_size
	);

	cmp_979_i : cmp_979 port map (
		ne => augh_test_124,
		in1 => sig_1635,
		in0 => huff_make_dhuff_tb_ac_size
	);

	cmp_847_i : cmp_847 port map (
		eq => augh_test_100,
		in1 => sig_1716,
		in0 => "00000000000000000000000011000000"
	);

	cmp_855_i : cmp_855 port map (
		ne => sig_1669,
		in1 => sig_1715,
		in0 => "00000000000000000000000000000000"
	);

	cmp_852_i : cmp_852 port map (
		eq => augh_test_94,
		in1 => sig_1714,
		in0 => "00000000000000000000000000000000"
	);

	mul_213_i : mul_213 port map (
		output => sig_1668,
		in_b => "00000000000000000000000000110001",
		in_a => chenidct_b3
	);

	mul_216_i : mul_216 port map (
		output => sig_1667,
		in_b => sig_1713,
		in_a => mux_762
	);

	mul_214_i : mul_214 port map (
		output => sig_1666,
		in_b => sig_1712,
		in_a => mux_760
	);

	cmp_846_i : cmp_846 port map (
		eq => augh_test_99,
		in1 => sig_1711,
		in0 => "00000000000000000000000011011000"
	);

	cmp_848_i : cmp_848 port map (
		eq => augh_test_101,
		in1 => sig_1710,
		in0 => "00000000000000000000000011011010"
	);

	cmp_849_i : cmp_849 port map (
		eq => augh_test_102,
		in1 => sig_1709,
		in0 => "00000000000000000000000011000100"
	);

	p_jinfo_comps_info_id_i : p_jinfo_comps_info_id port map (
		wa0_data => read_byte,
		wa0_addr => get_sof_i_comp_info_id,
		clk => sig_clock,
		ra0_addr => get_sos_ci(1 downto 0),
		ra0_data => sig_1665,
		wa0_en => sig_1213
	);

	p_jinfo_comps_info_h_samp_factor_i : p_jinfo_comps_info_h_samp_factor port map (
		wa0_data => and_876,
		wa0_addr => get_sof_i_comp_info_h_samp_factor,
		clk => sig_clock,
		ra0_addr => "00",
		ra0_data => sig_1664,
		wa0_en => sig_1214
	);

	p_jinfo_comps_info_quant_tbl_no_i : p_jinfo_comps_info_quant_tbl_no port map (
		wa0_data => read_byte(1 downto 0),
		wa0_addr => get_sof_i_comp_info_quant_tbl_no,
		clk => sig_clock,
		ra0_addr => decode_block_comp_no,
		ra0_data => sig_1663,
		wa0_en => sig_1212
	);

	p_jinfo_comps_info_dc_tbl_no_i : p_jinfo_comps_info_dc_tbl_no port map (
		wa0_data => get_sos_c,
		wa0_addr => get_sos_i_comp_info_dc_tbl_no,
		clk => sig_clock,
		ra0_addr => decode_block_comp_no,
		ra0_data => sig_1662,
		wa0_en => sig_1252
	);

	p_jinfo_quant_tbl_quantval_i : p_jinfo_quant_tbl_quantval port map (
		wa0_data => sig_1708,
		wa0_addr => sig_1707,
		clk => sig_clock,
		ra0_addr => mux_731,
		ra0_data => sig_1661,
		wa0_en => sig_1334
	);

	p_jinfo_dc_xhuff_tbl_bits_i : p_jinfo_dc_xhuff_tbl_bits port map (
		wa0_data => mux_782,
		wa0_addr => sig_1706,
		clk => sig_clock,
		ra0_addr => mux_727,
		ra0_data => sig_1660,
		wa0_en => sig_1457
	);

	p_jinfo_dc_xhuff_tbl_huffval_i : p_jinfo_dc_xhuff_tbl_huffval port map (
		wa0_data => mux_778,
		wa0_addr => sig_1705,
		clk => sig_clock,
		ra0_addr => mux_723,
		ra0_data => sig_1659,
		wa0_en => sig_1540
	);

	p_jinfo_ac_xhuff_tbl_bits_i : p_jinfo_ac_xhuff_tbl_bits port map (
		wa0_data => mux_783,
		wa0_addr => sig_1704,
		clk => sig_clock,
		ra0_addr => mux_719,
		ra0_data => sig_1658,
		wa0_en => sig_1457
	);

	p_jinfo_ac_xhuff_tbl_huffval_i : p_jinfo_ac_xhuff_tbl_huffval port map (
		wa0_data => mux_781,
		wa0_addr => sig_1703,
		clk => sig_clock,
		ra0_addr => mux_715,
		ra0_data => sig_1657,
		wa0_en => sig_1540
	);

	p_jinfo_dc_dhuff_tbl_ml_i : p_jinfo_dc_dhuff_tbl_ml port map (
		wa0_data => huff_make_dhuff_tb_dc,
		wa0_addr => sig_1188,
		clk => sig_clock,
		ra0_addr => mux_711,
		ra0_data => sig_1656,
		wa0_en => sig_1190
	);

	p_jinfo_dc_dhuff_tbl_maxcode_i : p_jinfo_dc_dhuff_tbl_maxcode port map (
		wa0_data => mux_705,
		wa0_addr => mux_706,
		clk => sig_clock,
		ra0_addr => mux_707,
		ra0_data => sig_1655,
		wa0_en => sig_1560
	);

	p_jinfo_dc_dhuff_tbl_mincode_i : p_jinfo_dc_dhuff_tbl_mincode port map (
		wa0_data => sig_1632(8 downto 0),
		wa0_addr => sig_1702,
		clk => sig_clock,
		ra0_addr => sig_1701,
		ra0_data => sig_1654,
		wa0_en => sig_1039
	);

	p_jinfo_dc_dhuff_tbl_valptr_i : p_jinfo_dc_dhuff_tbl_valptr port map (
		wa0_data => huff_make_dhuff_tb_dc_p(8 downto 0),
		wa0_addr => sig_1700,
		clk => sig_clock,
		ra0_addr => sig_1699,
		ra0_data => sig_1653,
		wa0_en => sig_1039
	);

	p_jinfo_ac_dhuff_tbl_ml_i : p_jinfo_ac_dhuff_tbl_ml port map (
		wa0_data => huff_make_dhuff_tb_ac,
		wa0_addr => sig_1183,
		clk => sig_clock,
		ra0_addr => mux_695,
		ra0_data => sig_1652,
		wa0_en => sig_1185
	);

	p_jinfo_ac_dhuff_tbl_maxcode_i : p_jinfo_ac_dhuff_tbl_maxcode port map (
		wa0_data => mux_689,
		wa0_addr => mux_690,
		clk => sig_clock,
		ra0_addr => mux_691,
		ra0_data => sig_1651,
		wa0_en => sig_1522
	);

	p_jinfo_ac_dhuff_tbl_mincode_i : p_jinfo_ac_dhuff_tbl_mincode port map (
		wa0_data => sig_1634(8 downto 0),
		wa0_addr => sig_1698,
		clk => sig_clock,
		ra0_addr => sig_1697,
		ra0_data => sig_1650,
		wa0_en => sig_1549
	);

	p_jinfo_ac_dhuff_tbl_valptr_i : p_jinfo_ac_dhuff_tbl_valptr port map (
		wa0_data => huff_make_dhuff_tb_ac_p(8 downto 0),
		wa0_addr => sig_1696,
		clk => sig_clock,
		ra0_addr => sig_1695,
		ra0_data => sig_1649,
		wa0_en => sig_1549
	);

	outdata_comp_vpos_i : outdata_comp_vpos port map (
		wa0_data => mux_663,
		wa0_addr => mux_664,
		clk => sig_clock,
		ra0_addr => mux_665,
		ra0_data => sig_1648,
		wa0_en => sig_1295
	);

	outdata_comp_hpos_i : outdata_comp_hpos port map (
		wa0_data => mux_659,
		wa0_addr => mux_660,
		clk => sig_clock,
		ra0_addr => mux_661,
		ra0_data => sig_1647,
		wa0_en => sig_1295
	);

	outdata_comp_buf_i : outdata_comp_buf port map (
		wa0_data => sig_1631,
		wa0_addr => sig_1694,
		clk => sig_clock,
		ra0_addr => sig_1693,
		ra0_data => sig_1646,
		wa0_en => sig_1013
	);

	izigzag_index_i : izigzag_index port map (
		clk => sig_clock,
		ra0_addr => get_dqt_i(5 downto 0),
		ra0_data => sig_1645
	);

	jpegfilebuf_i : jpegfilebuf port map (
		wa0_data => read8_ret0_195,
		wa0_addr => jpeg2bmp_main_i(12 downto 0),
		clk => sig_clock,
		ra0_addr => mux_652,
		ra0_data => sig_1644,
		wa0_en => sig_1041
	);

	huffbuff_i : huffbuff port map (
		wa0_data => mux_567,
		wa0_addr => mux_568,
		clk => sig_clock,
		ra0_addr => mux_569,
		ra0_data => sig_1643,
		wa0_en => sig_1428
	);

	idctbuff_i : idctbuff port map (
		wa0_data => mux_561,
		wa0_addr => mux_562,
		clk => sig_clock,
		ra2_data => sig_1642,
		ra2_addr => mux_563,
		ra1_data => sig_1641,
		ra1_addr => sig_1692,
		ra0_addr => mux_565,
		ra0_data => sig_1640,
		wa0_en => sig_1474
	);

	quantbuff_i : quantbuff port map (
		wa0_data => mux_557,
		wa0_addr => mux_558,
		clk => sig_clock,
		ra0_addr => mux_559,
		ra0_data => sig_1639,
		wa0_en => sig_1431
	);

	extend_mask_i : extend_mask port map (
		clk => sig_clock,
		ra0_addr => decodehuffmcu_s(4 downto 0),
		ra0_data => sig_1638
	);

	bit_set_mask_i : bit_set_mask port map (
		clk => sig_clock,
		ra0_addr => mux_524,
		ra0_data => sig_1637
	);

	lmask_i : lmask port map (
		clk => sig_clock,
		ra0_addr => buf_getv_n(4 downto 0),
		ra0_data => sig_1636
	);

	huff_make_dhuff_tb_ac_huffsize_i : huff_make_dhuff_tb_ac_huffsize port map (
		wa0_data => mux_476,
		wa0_addr => huff_make_dhuff_tb_ac_p(8 downto 0),
		clk => sig_clock,
		ra0_addr => mux_478,
		ra0_data => sig_1635,
		wa0_en => sig_1501
	);

	huff_make_dhuff_tb_ac_huffcode_i : huff_make_dhuff_tb_ac_huffcode port map (
		wa0_data => huff_make_dhuff_tb_ac_code,
		wa0_addr => huff_make_dhuff_tb_ac_p(8 downto 0),
		clk => sig_clock,
		ra0_addr => huff_make_dhuff_tb_ac_p(8 downto 0),
		ra0_data => sig_1634,
		wa0_en => sig_1024
	);

	huff_make_dhuff_tb_dc_huffsize_i : huff_make_dhuff_tb_dc_huffsize port map (
		wa0_data => mux_443,
		wa0_addr => huff_make_dhuff_tb_dc_p(8 downto 0),
		clk => sig_clock,
		ra0_addr => mux_445,
		ra0_data => sig_1633,
		wa0_en => sig_1530
	);

	huff_make_dhuff_tb_dc_huffcode_i : huff_make_dhuff_tb_dc_huffcode port map (
		wa0_data => huff_make_dhuff_tb_dc_code,
		wa0_addr => huff_make_dhuff_tb_dc_p(8 downto 0),
		clk => sig_clock,
		ra0_addr => huff_make_dhuff_tb_dc_p(8 downto 0),
		ra0_data => sig_1632,
		wa0_en => sig_1036
	);

	rgb_buf_i : rgb_buf port map (
		wa0_data => mux_378,
		wa0_addr => mux_379,
		clk => sig_clock,
		ra0_addr => sig_1691,
		ra0_data => sig_1631,
		wa0_en => sig_1236
	);

	zigzag_index_i : zigzag_index port map (
		clk => sig_clock,
		ra0_addr => izigzagmatrix_i(5 downto 0),
		ra0_data => sig_1630
	);

	shr_212_i : shr_212 port map (
		output => sig_1629,
		input => mux_322,
		shift => mux_323,
		padding => '0'
	);

	mul_209_i : mul_209 port map (
		output => sig_1628,
		in_b => mux_315,
		in_a => mux_316
	);

	mul_210_i : mul_210 port map (
		output => sig_1627,
		in_b => sig_1690,
		in_a => mux_314
	);

	shl_211_i : shl_211 port map (
		output => sig_1626,
		input => current_read_byte,
		shift => buf_getv_p(5 downto 0),
		padding => '0'
	);

	sub_206_i : sub_206 port map (
		gt => sig_1625,
		output => sig_1624,
		sign => '1',
		in_b => mux_306,
		in_a => mux_307
	);

	sub_207_i : sub_207 port map (
		ge => sig_1623,
		le => sig_1622,
		output => sig_1621,
		sign => '1',
		in_b => mux_302,
		in_a => mux_303
	);

	sub_208_i : sub_208 port map (
		ge => sig_1620,
		output => sig_1619,
		sign => '1',
		in_b => "00000000000000000000000000000000000000000",
		in_a => sig_1689
	);

	sub_205_i : sub_205 port map (
		gt => sig_1618,
		ge => sig_1617,
		lt => sig_1616,
		le => sig_1615,
		output => sig_1614,
		sign => '1',
		in_b => mux_290,
		in_a => mux_291
	);

	add_202_i : add_202 port map (
		output => sig_1613,
		in_b => mux_274,
		in_a => mux_275
	);

	add_203_i : add_203 port map (
		output => sig_1612,
		in_b => mux_271,
		in_a => mux_272
	);

	add_204_i : add_204 port map (
		output => sig_1611,
		in_b => "0000000000000000000000001",
		in_a => sig_1688
	);

	add_201_i : add_201 port map (
		output => sig_1610,
		in_b => mux_265,
		in_a => mux_266
	);

	add_200_i : add_200 port map (
		output => sig_1609,
		in_b => mux_260,
		in_a => mux_261
	);

	cmp_775_i : cmp_775 port map (
		eq => augh_test_158,
		in1 => sig_1687,
		in0 => "00000000000000000000000000001111"
	);

	cmp_779_i : cmp_779 port map (
		eq => sig_1608,
		in1 => sig_1686,
		in0 => "00000000000000000000000000000000"
	);

	cmp_780_i : cmp_780 port map (
		ne => sig_1607,
		in1 => sig_1685,
		in0 => "00000000000000000000000000000000"
	);

	cmp_787_i : cmp_787 port map (
		eq => sig_1606,
		in1 => '0',
		in0 => sig_1610(0)
	);

	cmp_788_i : cmp_788 port map (
		eq => sig_1605,
		in1 => "000",
		in0 => sig_1642(2 downto 0)
	);

	cmp_790_i : cmp_790 port map (
		ne => sig_1604,
		in1 => sig_1624(3 downto 0),
		in0 => "0000"
	);

	cmp_792_i : cmp_792 port map (
		eq => augh_test_134,
		in1 => sig_1660,
		in0 => "00000000000000000000000000000000"
	);

	cmp_793_i : cmp_793 port map (
		eq => augh_test_131,
		in1 => sig_1633,
		in0 => "00000000000000000000000000000000"
	);

	cmp_794_i : cmp_794 port map (
		eq => augh_test_126,
		in1 => sig_1658,
		in0 => "00000000000000000000000000000000"
	);

	cmp_791_i : cmp_791 port map (
		ne => augh_test_148,
		in1 => decodehuffman_dc,
		in0 => "00000000000000000000000000000000"
	);

	cmp_804_i : cmp_804 port map (
		ne => augh_test_113,
		in1 => and_803,
		in0 => "00000000000000000000000000000000"
	);

	cmp_800_i : cmp_800 port map (
		eq => augh_test_118,
		in1 => buf_getv_p,
		in0 => "00000000000000000000000000000000"
	);

	cmp_799_i : cmp_799 port map (
		eq => augh_test_123,
		in1 => sig_1635,
		in0 => "00000000000000000000000000000000"
	);

	cmp_865_i : cmp_865 port map (
		ne => sig_1603,
		in1 => sig_1624(2 downto 0),
		in0 => "000"
	);

	cmp_882_i : cmp_882 port map (
		eq => augh_test_157,
		in1 => and_881,
		in0 => "00000000000000000000000000000000"
	);

	cmp_885_i : cmp_885 port map (
		ne => sig_1602,
		in1 => and_884,
		in0 => "00000000000000000000000000000000"
	);

	cmp_887_i : cmp_887 port map (
		eq => sig_1601,
		in1 => and_884,
		in0 => "00000000000000000000000000000000"
	);

	mul_215_i : mul_215 port map (
		output => sig_1600,
		in_b => "00000000000000000000000111011001",
		in_a => chenidct_b2
	);

	cmp_850_i : cmp_850 port map (
		eq => augh_test_103,
		in1 => sig_1684,
		in0 => "00000000000000000000000011011011"
	);

	cmp_851_i : cmp_851 port map (
		eq => augh_test_104,
		in1 => sig_1683,
		in0 => "00000000000000000000000011011001"
	);

	cmp_861_i : cmp_861 port map (
		eq => augh_test_150,
		in1 => and_860,
		in0 => "00000000000000000000000000000000"
	);

	cmp_871_i : cmp_871 port map (
		eq => sig_1599,
		in1 => sig_1682,
		in0 => "00000000000000000000000000000000"
	);

	cmp_873_i : cmp_873 port map (
		eq => sig_1598,
		in1 => sig_1664,
		in0 => "00000010"
	);

	cmp_879_i : cmp_879 port map (
		ne => augh_test_6,
		in1 => sig_1681,
		in0 => "00000000000000000000000011111111"
	);

	cmp_880_i : cmp_880 port map (
		eq => augh_test_9,
		in1 => sig_1680,
		in0 => "00000000000000000000000011111111"
	);

	sub_217_i : sub_217 port map (
		ge => sig_1597,
		output => sig_1596,
		sign => '1',
		in_b => "00000000000000000000000000000000000000000",
		in_a => sig_1679
	);

	cmp_863_i : cmp_863 port map (
		ne => sig_1595,
		in1 => sig_1614(2 downto 0),
		in0 => "000"
	);

	cmp_868_i : cmp_868 port map (
		eq => sig_1594,
		in1 => "000000000000000000000000",
		in0 => "000000000000000000000000"
	);

	cmp_877_i : cmp_877 port map (
		ne => augh_test_109,
		in1 => sig_1678,
		in0 => "00000000000000000000000000000000"
	);

	cmp_878_i : cmp_878 port map (
		ne => augh_test_10,
		in1 => sig_1677,
		in0 => "00000000000000000000000000000000"
	);

	sub_218_i : sub_218 port map (
		le => sig_1593,
		output => sig_1592,
		sign => '1',
		in_b => "00000000000000000000000000000000011111111",
		in_a => sig_1676
	);

	sub_220_i : sub_220 port map (
		gt => sig_1591,
		output => sig_1590,
		sign => '1',
		in_b => "00000000000000000000000000000000011111111",
		in_a => sig_1675
	);

	sub_221_i : sub_221 port map (
		gt => sig_1589,
		output => sig_1588,
		sign => '1',
		in_b => "00000000000000000000000000000000011111111",
		in_a => sig_1674
	);

	mul_222_i : mul_222 port map (
		output => sig_1587,
		in_b => "00000000000000000000000010110101",
		in_a => mux_233
	);

	sub_219_i : sub_219 port map (
		le => sig_1586,
		output => sig_1585,
		sign => '1',
		in_b => "00000000000000000000000000000000011111111",
		in_a => sig_1673
	);

	cmp_962_i : cmp_962 port map (
		ne => augh_test_62,
		in1 => get_sos_j,
		in0 => "11111111111111111111111111111111"
	);

	cmp_975_i : cmp_975 port map (
		ne => augh_test_154,
		in1 => decodehuffmcu_s,
		in0 => "00000000000000000000000000000000"
	);

	fsm_224_i : fsm_224 port map (
		clock => sig_clock,
		reset => sig_reset,
		out40 => sig_1584,
		in2 => augh_test_152,
		in11 => augh_test_131,
		out146 => sig_1583,
		out148 => sig_1582,
		out150 => sig_1581,
		out152 => sig_1580,
		in12 => augh_test_128,
		out153 => sig_1579,
		out154 => sig_1578,
		in13 => augh_test_127,
		out156 => sig_1577,
		out157 => sig_1576,
		out160 => sig_1575,
		out162 => sig_1574,
		out165 => sig_1573,
		out170 => sig_1572,
		out171 => sig_1571,
		out173 => sig_1570,
		out175 => sig_1569,
		out177 => sig_1568,
		out180 => sig_1567,
		out184 => sig_1566,
		in14 => augh_test_126,
		out186 => sig_1565,
		out189 => sig_1564,
		out191 => sig_1563,
		out192 => sig_1562,
		out193 => sig_1561,
		out197 => sig_1560,
		out199 => sig_1559,
		out201 => sig_1558,
		out202 => sig_1557,
		out205 => sig_1556,
		out207 => sig_1555,
		out208 => sig_1554,
		out209 => sig_1553,
		out210 => sig_1552,
		out212 => sig_1551,
		out213 => sig_1550,
		in15 => augh_test_125,
		out221 => sig_1549,
		out222 => sig_1548,
		out224 => sig_1547,
		out225 => sig_1546,
		out228 => sig_1545,
		out229 => sig_1544,
		out230 => sig_1543,
		out231 => sig_1542,
		out99 => sig_1541,
		in6 => augh_test_142,
		out92 => sig_1540,
		out232 => sig_1539,
		in16 => augh_test_123,
		out234 => sig_1538,
		out236 => sig_1537,
		out239 => sig_1536,
		out240 => sig_1535,
		out241 => sig_1534,
		out245 => sig_1533,
		out246 => sig_1532,
		out247 => sig_1531,
		out251 => sig_1530,
		out252 => sig_1529,
		out253 => sig_1528,
		out255 => sig_1527,
		out256 => sig_1526,
		out258 => sig_1525,
		out259 => sig_1524,
		in17 => augh_test_120,
		out263 => sig_1523,
		out264 => sig_1522,
		out266 => sig_1521,
		in18 => augh_test_119,
		out267 => sig_1520,
		out268 => sig_1519,
		out270 => sig_1518,
		out273 => sig_1517,
		out275 => sig_1516,
		out276 => sig_1515,
		in19 => augh_test_118,
		out279 => sig_1514,
		in20 => augh_test_115,
		out281 => sig_1513,
		out282 => sig_1512,
		in21 => augh_test_114,
		out283 => sig_1511,
		out286 => sig_1510,
		out289 => sig_1509,
		out296 => sig_1508,
		out297 => sig_1507,
		out299 => sig_1506,
		out300 => sig_1505,
		out304 => sig_1504,
		out305 => sig_1503,
		in22 => augh_test_113,
		out306 => sig_1502,
		out310 => sig_1501,
		out311 => sig_1500,
		out313 => sig_1499,
		out314 => sig_1498,
		in23 => augh_test_111,
		out316 => sig_1497,
		out317 => sig_1496,
		out320 => sig_1495,
		out322 => sig_1494,
		out324 => sig_1493,
		out325 => sig_1492,
		out326 => sig_1491,
		out328 => sig_1490,
		out332 => sig_1489,
		out333 => sig_1488,
		out334 => sig_1487,
		out335 => sig_1486,
		out338 => sig_1485,
		out339 => sig_1484,
		out341 => sig_1483,
		out342 => sig_1482,
		out344 => sig_1481,
		out93 => sig_1480,
		out98 => sig_1479,
		out85 => sig_1478,
		out87 => sig_1477,
		out88 => sig_1476,
		out80 => sig_1475,
		out82 => sig_1474,
		out83 => sig_1473,
		out84 => sig_1472,
		in5 => augh_test_144,
		out77 => sig_1471,
		out78 => sig_1470,
		out71 => sig_1469,
		out72 => sig_1468,
		in4 => augh_test_148,
		out65 => sig_1467,
		out67 => sig_1466,
		out60 => sig_1465,
		out64 => sig_1464,
		in3 => augh_test_151,
		out59 => sig_1463,
		out53 => sig_1462,
		out55 => sig_1461,
		out49 => sig_1460,
		out44 => sig_1459,
		out104 => sig_1458,
		out107 => sig_1457,
		out111 => sig_1456,
		out112 => sig_1455,
		out114 => sig_1454,
		in7 => augh_test_138,
		out117 => sig_1453,
		out119 => sig_1452,
		out122 => sig_1451,
		in8 => augh_test_136,
		out128 => sig_1450,
		in9 => augh_test_134,
		out129 => sig_1449,
		out130 => sig_1448,
		out133 => sig_1447,
		out134 => sig_1446,
		out136 => sig_1445,
		out137 => sig_1444,
		in10 => augh_test_133,
		out139 => sig_1443,
		out143 => sig_1442,
		out144 => sig_1441,
		out32 => sig_1440,
		out35 => sig_1439,
		out27 => sig_1438,
		out25 => sig_1437,
		out26 => sig_1436,
		in1 => augh_test_158,
		out15 => sig_1435,
		out16 => sig_1434,
		out11 => sig_1433,
		out13 => sig_1432,
		out14 => sig_1431,
		out7 => sig_1430,
		out1 => sig_1429,
		out2 => sig_1428,
		out3 => sig_1427,
		out4 => sig_1426,
		in0 => augh_test_159,
		in24 => augh_test_107,
		out346 => sig_1425,
		out347 => sig_1424,
		out348 => sig_1423,
		out349 => sig_1422,
		in25 => augh_test_106,
		out350 => sig_1421,
		out351 => sig_1420,
		out355 => sig_1419,
		out356 => sig_1418,
		out357 => sig_1417,
		out358 => sig_1416,
		out360 => sig_1415,
		out362 => sig_1414,
		out363 => sig_1413,
		out364 => sig_1412,
		out365 => sig_1411,
		out366 => sig_1410,
		out370 => sig_1409,
		out371 => sig_1408,
		out372 => sig_1407,
		out373 => sig_1406,
		out375 => sig_1405,
		in26 => augh_test_105,
		out376 => sig_1404,
		out378 => sig_1403,
		out379 => sig_1402,
		out381 => sig_1401,
		out382 => sig_1400,
		in27 => augh_test_99,
		out384 => sig_1399,
		in28 => augh_test_100,
		out391 => sig_1398,
		out395 => sig_1397,
		out396 => sig_1396,
		out401 => sig_1395,
		out402 => sig_1394,
		out403 => sig_1393,
		out404 => sig_1392,
		out405 => sig_1391,
		out407 => sig_1390,
		out408 => sig_1389,
		out409 => sig_1388,
		out410 => sig_1387,
		in29 => augh_test_101,
		out412 => sig_1386,
		out414 => sig_1385,
		out415 => sig_1384,
		out417 => sig_1383,
		out418 => sig_1382,
		out419 => sig_1381,
		out420 => sig_1380,
		out422 => sig_1379,
		out424 => sig_1378,
		out425 => sig_1377,
		out426 => sig_1376,
		in30 => augh_test_102,
		out428 => sig_1375,
		out429 => sig_1374,
		out432 => sig_1373,
		out433 => sig_1372,
		out434 => sig_1371,
		out437 => sig_1370,
		out440 => sig_1369,
		out441 => sig_1368,
		in31 => augh_test_103,
		out443 => sig_1367,
		in32 => augh_test_104,
		out445 => sig_1366,
		out447 => sig_1365,
		out448 => sig_1364,
		out450 => sig_1363,
		in33 => augh_test_94,
		out453 => sig_1362,
		out455 => sig_1361,
		out458 => sig_1360,
		in34 => augh_test_90,
		out462 => sig_1359,
		out464 => sig_1358,
		out467 => sig_1357,
		out468 => sig_1356,
		out472 => sig_1355,
		in35 => augh_test_89,
		out478 => sig_1354,
		out479 => sig_1353,
		out480 => sig_1352,
		out487 => sig_1351,
		out488 => sig_1350,
		in36 => augh_test_83,
		out491 => sig_1349,
		out496 => sig_1348,
		out497 => sig_1347,
		out498 => sig_1346,
		out500 => sig_1345,
		out504 => sig_1344,
		out505 => sig_1343,
		in37 => augh_test_150,
		out506 => sig_1342,
		out508 => sig_1341,
		in38 => augh_test_77,
		out510 => sig_1340,
		out513 => sig_1339,
		out514 => sig_1338,
		out515 => sig_1337,
		out517 => sig_1336,
		out519 => sig_1335,
		in39 => augh_test_72,
		out523 => sig_1334,
		out526 => sig_1333,
		out527 => sig_1332,
		out528 => sig_1331,
		out530 => sig_1330,
		out531 => sig_1329,
		out533 => sig_1328,
		out534 => sig_1327,
		out537 => sig_1326,
		out538 => sig_1325,
		out549 => sig_1324,
		out558 => sig_1323,
		out559 => sig_1322,
		out561 => sig_1321,
		in40 => augh_test_67,
		out566 => sig_1320,
		out567 => sig_1319,
		out568 => sig_1318,
		out569 => sig_1317,
		out570 => sig_1316,
		out572 => sig_1315,
		out574 => sig_1314,
		out575 => sig_1313,
		out577 => sig_1312,
		in41 => augh_test_52,
		out578 => sig_1311,
		out581 => sig_1310,
		out589 => sig_1309,
		out590 => sig_1308,
		out595 => sig_1307,
		out597 => sig_1306,
		out599 => sig_1305,
		out601 => sig_1304,
		out602 => sig_1303,
		out607 => sig_1302,
		out610 => sig_1301,
		out612 => sig_1300,
		in42 => augh_test_53,
		out614 => sig_1299,
		out621 => sig_1298,
		out628 => sig_1297,
		out635 => sig_1296,
		out636 => sig_1295,
		out638 => sig_1294,
		out640 => sig_1293,
		out643 => sig_1292,
		out646 => sig_1291,
		out649 => sig_1290,
		out651 => sig_1289,
		out656 => sig_1288,
		in43 => augh_test_49,
		out658 => sig_1287,
		out659 => sig_1286,
		out661 => sig_1285,
		out663 => sig_1284,
		out664 => sig_1283,
		in44 => augh_test_109,
		out667 => sig_1282,
		out668 => sig_1281,
		out670 => sig_1280,
		out672 => sig_1279,
		out674 => sig_1278,
		in45 => augh_test_26,
		out679 => sig_1277,
		out681 => sig_1276,
		out683 => sig_1275,
		out686 => sig_1274,
		out688 => sig_1273,
		out690 => sig_1272,
		out692 => sig_1271,
		out694 => sig_1270,
		out696 => sig_1269,
		out697 => sig_1268,
		out698 => sig_1267,
		out699 => sig_1266,
		out700 => sig_1265,
		out703 => sig_1264,
		out704 => sig_1263,
		out706 => sig_1262,
		out708 => sig_1261,
		out710 => sig_1260,
		out712 => sig_1259,
		out715 => sig_1258,
		out718 => sig_1257,
		in46 => augh_test_10,
		out722 => sig_1256,
		out724 => sig_1255,
		out726 => sig_1254,
		out728 => sig_1253,
		out731 => sig_1252,
		out733 => sig_1251,
		out734 => sig_1250,
		out737 => sig_1249,
		out739 => sig_1248,
		out740 => sig_1247,
		out743 => sig_1246,
		out745 => sig_1245,
		out746 => sig_1244,
		in47 => augh_test_6,
		out749 => sig_1243,
		out753 => sig_1242,
		out755 => sig_1241,
		out759 => sig_1240,
		in48 => augh_test_9,
		out762 => sig_1239,
		out764 => sig_1238,
		out765 => sig_1237,
		out767 => sig_1236,
		out768 => sig_1235,
		in49 => augh_test_157,
		out772 => sig_1234,
		in50 => stdout_ack,
		out775 => sig_1233,
		out776 => sig_1232,
		out778 => sig_1231,
		out783 => sig_1230,
		out784 => sig_1229,
		out787 => sig_1228,
		out791 => sig_1227,
		in51 => stdin_ack,
		out794 => sig_1226,
		out795 => sig_1225,
		in52 => augh_test_62,
		out799 => sig_1224,
		out802 => sig_1223,
		out806 => sig_1222,
		out809 => sig_1221,
		out812 => sig_1220,
		out815 => sig_1219,
		out826 => sig_1218,
		out828 => sig_1217,
		in53 => augh_test_122,
		in54 => augh_test_197,
		out843 => sig_1216,
		out848 => sig_1215,
		out852 => sig_1214,
		in55 => augh_test_196,
		out855 => sig_1213,
		out858 => sig_1212,
		in56 => augh_test_189,
		out860 => sig_1211,
		out861 => sig_1210,
		out863 => sig_1209,
		out866 => sig_1208,
		out872 => sig_1207,
		in57 => augh_test_188,
		out874 => sig_1206,
		out876 => sig_1205,
		out879 => sig_1204,
		out882 => sig_1203,
		out886 => sig_1202,
		out887 => sig_1201,
		in58 => augh_test_187,
		out888 => sig_1200,
		out892 => sig_1199,
		out894 => sig_1198,
		out895 => sig_1197,
		out896 => sig_1196,
		out901 => sig_1195,
		out902 => sig_1194,
		out903 => sig_1193,
		out905 => sig_1192,
		out907 => sig_1191,
		out918 => sig_1190,
		out920 => sig_1189,
		out921 => sig_1188,
		out923 => sig_1187,
		out925 => sig_1186,
		out928 => sig_1185,
		out929 => sig_1184,
		out931 => sig_1183,
		out933 => sig_1182,
		out936 => stdout_rdy,
		out937 => sig_1181,
		out938 => sig_1180,
		out939 => sig_1179,
		out942 => sig_1178,
		out943 => sig_1177,
		out944 => sig_1176,
		out947 => sig_1175,
		out948 => sig_1174,
		out949 => sig_1173,
		out951 => sig_1172,
		in59 => augh_test_186,
		out952 => sig_1171,
		out953 => sig_1170,
		out955 => sig_1169,
		out956 => sig_1168,
		out957 => sig_1167,
		out958 => sig_1166,
		in60 => augh_test_184,
		in61 => augh_test_183,
		out962 => sig_1165,
		out963 => sig_1164,
		out972 => sig_1163,
		out973 => sig_1162,
		out974 => sig_1161,
		in62 => augh_test_182,
		out978 => sig_1160,
		out979 => sig_1159,
		out981 => sig_1158,
		out982 => sig_1157,
		out985 => sig_1156,
		out986 => sig_1155,
		out989 => sig_1154,
		in63 => augh_test_180,
		in64 => augh_test_179,
		in65 => augh_test_178,
		in66 => augh_test_194,
		in67 => augh_test_154,
		in68 => augh_test_130,
		in69 => augh_test_132,
		in70 => augh_test_124,
		in71 => augh_test_171,
		in72 => augh_test_168,
		in73 => augh_test_167,
		in74 => augh_test_166,
		in75 => augh_test_165,
		in76 => augh_test_108,
		in77 => sig_start,
		in78 => augh_test_155,
		out990 => sig_1153,
		out991 => sig_1152,
		out993 => sig_1151,
		out994 => sig_1150,
		out996 => sig_1149,
		out997 => sig_1148,
		out998 => sig_1147,
		out999 => sig_1146,
		out1000 => sig_1145,
		out1002 => sig_1144,
		out1003 => sig_1143,
		out1005 => sig_1142,
		out1006 => sig_1141,
		out1007 => sig_1140,
		out1009 => sig_1139,
		out1011 => sig_1138,
		out1012 => sig_1137,
		out1013 => sig_1136,
		out1014 => sig_1135,
		out1015 => sig_1134,
		out1016 => sig_1133,
		out1018 => sig_1132,
		out1019 => sig_1131,
		out1021 => sig_1130,
		out1022 => sig_1129,
		out1024 => sig_1128,
		out1026 => sig_1127,
		out1027 => sig_1126,
		out1029 => sig_1125,
		out1030 => sig_1124,
		out1032 => sig_1123,
		out1033 => sig_1122,
		out1035 => sig_1121,
		out1036 => sig_1120,
		out1037 => sig_1119,
		out1057 => sig_1118,
		out1068 => sig_1117,
		out1069 => sig_1116,
		out1070 => sig_1115,
		out1072 => sig_1114,
		out1073 => sig_1113,
		out1075 => sig_1112,
		out1078 => sig_1111,
		out1080 => sig_1110,
		out1082 => sig_1109,
		out1083 => sig_1108,
		out1084 => sig_1107,
		out1085 => sig_1106,
		out1088 => sig_1105,
		out1089 => sig_1104,
		out1091 => sig_1103,
		out1092 => sig_1102,
		out1094 => sig_1101,
		out1096 => sig_1100,
		out1098 => sig_1099,
		out1101 => sig_1098,
		out1104 => sig_1097,
		out1107 => sig_1096,
		out1109 => sig_1095,
		out1111 => sig_1094,
		out1114 => sig_1093,
		out1119 => sig_1092,
		out1121 => sig_1091,
		out1125 => sig_1090,
		out1126 => sig_1089,
		out1128 => sig_1088,
		out1131 => sig_1087,
		out1134 => sig_1086,
		out1137 => sig_1085,
		out1139 => sig_1084,
		out1141 => sig_1083,
		out1145 => sig_1082,
		out1146 => sig_1081,
		out1147 => sig_1080,
		out1150 => sig_1079,
		out1151 => sig_1078,
		out1152 => sig_1077,
		out1155 => sig_1076,
		out1158 => sig_1075,
		out1160 => sig_1074,
		out1164 => sig_1073,
		out1166 => sig_1072,
		out1169 => sig_1071,
		out1171 => sig_1070,
		out1174 => sig_1069,
		out1175 => sig_1068,
		out1176 => sig_1067,
		out1180 => sig_1066,
		out1181 => sig_1065,
		out1182 => sig_1064,
		out1185 => sig_1063,
		out1186 => sig_1062,
		out1187 => sig_1061,
		out1190 => sig_1060,
		out1213 => sig_1059,
		out1215 => sig_1058,
		out1217 => sig_1057,
		out1220 => sig_1056,
		out1221 => sig_1055,
		out1223 => sig_1054,
		out1228 => sig_1053,
		out1229 => sig_1052,
		out1231 => sig_1051,
		out1235 => sig_1050,
		out1236 => sig_1049,
		out1240 => sig_1048,
		out1243 => sig_1047,
		out1250 => sig_1046,
		out1252 => sig_1045,
		out1253 => sig_1044,
		out1258 => sig_1043,
		out1262 => sig_1042,
		out1266 => sig_1041,
		out1269 => sig_1040,
		out1275 => sig_1039,
		out1278 => sig_1038,
		out1279 => sig_1037,
		out1284 => sig_1036,
		out1286 => sig_1035,
		out1287 => sig_1034,
		out1289 => sig_1033,
		out1290 => sig_1032,
		out1292 => sig_1031,
		out1293 => sig_1030,
		out1295 => sig_1029,
		out1298 => sig_1028,
		out1301 => sig_1027,
		out1302 => sig_1026,
		out1303 => sig_1025,
		out1308 => sig_1024,
		out1309 => sig_1023,
		out1311 => sig_1022,
		out1318 => sig_1021,
		out1319 => sig_1020,
		out1320 => sig_1019,
		out1323 => sig_1018,
		out1324 => sig_1017,
		out1326 => sig_1016,
		out1327 => sig_1015,
		out1329 => sig_1014,
		out1337 => sig_1013,
		out1339 => sig_1012,
		out1340 => sig_1011,
		out1341 => sig_1010,
		out1344 => sig_1009,
		out1346 => sig_1008,
		out1349 => sig_1007,
		out1353 => sig_1006,
		out1356 => sig_1005,
		out1362 => sig_1004,
		out1363 => sig_1003,
		out1364 => sig_1002,
		out1365 => sig_1001,
		out1366 => sig_1000,
		out1368 => sig_999,
		out1370 => sig_998,
		out1375 => sig_997,
		out1378 => sig_996,
		out1381 => sig_995,
		out1383 => sig_994,
		out1387 => sig_993
	);

	muxb_784_i : muxb_784 port map (
		in_sel => sig_1616,
		out_data => sig_992,
		in_data0 => sig_1609(31 downto 0),
		in_data1 => sig_1613
	);

	cmp_964_i : cmp_964 port map (
		eq => sig_991,
		in1 => sig_1635,
		in0 => huff_make_dhuff_tb_ac_size
	);

	cmp_972_i : cmp_972 port map (
		ne => augh_test_196,
		in1 => jpeg2bmp_main_i,
		in0 => "00000000000000000000000000000010"
	);

	cmp_973_i : cmp_973 port map (
		eq => augh_test_180,
		in1 => sig_1672,
		in0 => "00000000000000000000000000000000"
	);

	cmp_974_i : cmp_974 port map (
		ne => augh_test_194,
		in1 => jpeg2bmp_main_i,
		in0 => "00000000000000000001010001010110"
	);

	cmp_985_i : cmp_985 port map (
		eq => augh_test_108,
		in1 => sig_1671,
		in0 => "00000000000000000000000011111111"
	);

	cmp_971_i : cmp_971 port map (
		ne => augh_test_197,
		in1 => jpeg2bmp_main_j,
		in0 => "00000000000000000001010010111101"
	);

	cmp_977_i : cmp_977 port map (
		eq => sig_990,
		in1 => sig_1633,
		in0 => huff_make_dhuff_tb_dc_size
	);

	-- Behaviour of component 'mux_967' model 'mux'
	mux_967 <=
		(repeat(32, sig_1620) and mux_968);

	-- Behaviour of component 'and_976' model 'and'
	and_976 <=
		sig_1615 and
		sig_990;

	-- Behaviour of component 'and_982' model 'and'
	and_982 <=
		"00000000000000000000000000001111" and
		decodehuffman_ac;

	-- Behaviour of component 'and_983' model 'and'
	and_983 <=
		"0000000000000000000000001111" and
		decodehuffman_ac(31 downto 4);

	-- Behaviour of component 'and_984' model 'and'
	and_984 <=
		sig_1636 and
		buf_getv_rv;

	-- Behaviour of component 'mux_689' model 'mux'
	mux_689 <=
		(repeat(32, sig_1034) and sig_1634) or
		(repeat(32, sig_1520) and "11111111111111111111111111111111") or
		(repeat(32, sig_1523) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_690' model 'mux'
	mux_690 <=
		(repeat(7, sig_1519) and huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_l(5 downto 0)) or
		(repeat(7, sig_1523) and huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_p_dhtbl_ml(5 downto 0));

	-- Behaviour of component 'mux_691' model 'mux'
	mux_691 <=
		(repeat(7, sig_1523) and huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_p_dhtbl_ml(5 downto 0)) or
		(repeat(7, sig_1568) and decodehuffman_ac_tbl_no & decodehuffman_ac_l(5 downto 0)) or
		(repeat(7, sig_1570) and decodehuffman_ac_tbl_no & decodehuffman_ac_dhuff_ml);

	-- Behaviour of component 'and_853' model 'and'
	and_853 <=
		sig_1636 and
		sig_1629;

	-- Behaviour of component 'mux_233' model 'mux'
	mux_233 <=
		(repeat(32, sig_1118) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1324) and sig_1624(31 downto 0));

	-- Behaviour of component 'mux_671' model 'mux'
	mux_671 <=
		(repeat(32, sig_1183) and i_jinfo_jpeg_data) or
		(repeat(32, sig_1441) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_665' model 'mux'
	mux_665 <=
		(repeat(2, sig_1162) and write4blocks_i) or
		(repeat(2, sig_1196) and decode_start_i(1 downto 0)) or
		(repeat(2, sig_1296) and writeblock_i);

	-- Behaviour of component 'mux_663' model 'mux'
	mux_663 <=
		(repeat(32, sig_1163) and sig_1609(30 downto 0) & sig_1648(0)) or
		(repeat(32, sig_1161) and mux_896) or
		(repeat(32, sig_1215) and mux_874) or
		(repeat(32, sig_1297) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_664' model 'mux'
	mux_664 <=
		(repeat(2, sig_1043) and decode_start_i(1 downto 0)) or
		(repeat(2, sig_1162) and write4blocks_i) or
		(repeat(2, sig_1296) and writeblock_i);

	-- Behaviour of component 'mux_659' model 'mux'
	mux_659 <=
		(repeat(32, sig_1163) and sig_1610(30 downto 0) & sig_1647(0)) or
		(repeat(32, sig_1161) and mux_897) or
		(repeat(32, sig_1215) and mux_875) or
		(repeat(32, sig_1297) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_660' model 'mux'
	mux_660 <=
		(repeat(2, sig_1043) and decode_start_i(1 downto 0)) or
		(repeat(2, sig_1162) and write4blocks_i) or
		(repeat(2, sig_1296) and writeblock_i);

	-- Behaviour of component 'mux_661' model 'mux'
	mux_661 <=
		(repeat(2, sig_1162) and write4blocks_i) or
		(repeat(2, sig_1196) and decode_start_i(1 downto 0)) or
		(repeat(2, sig_1296) and writeblock_i);

	-- Behaviour of component 'mux_652' model 'mux'
	mux_652 <=
		(repeat(13, sig_1247) and readbuf_idx(12 downto 0)) or
		(repeat(13, sig_1441) and curhuffreadbuf_idx(12 downto 0));

	-- Behaviour of component 'mux_648' model 'mux'
	mux_648 <=
		(repeat(32, sig_1247) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_633' model 'mux'
	mux_633 <=
		(repeat(32, sig_1211) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_622' model 'mux'
	mux_622 <=
		(repeat(32, sig_1251) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_614' model 'mux'
	mux_614 <=
		(repeat(32, sig_1269) and "00000000000000000000000000000011") or
		(repeat(32, sig_1287) and sig_1614(31 downto 0));

	-- Behaviour of component 'mux_616' model 'mux'
	mux_616 <=
		(repeat(32, sig_1254) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_602' model 'mux'
	mux_602 <=
		(repeat(32, sig_1198) and "00000000000000000000000000000001") or
		(repeat(32, sig_1479) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_600' model 'mux'
	mux_600 <=
		(repeat(32, sig_1458) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_593' model 'mux'
	mux_593 <=
		(repeat(32, sig_1240) and mux_870) or
		(repeat(32, sig_1317) and sig_1614(31 downto 0));

	-- Behaviour of component 'mux_587' model 'mux'
	mux_587 <=
		(repeat(32, sig_1335) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_585' model 'mux'
	mux_585 <=
		(repeat(16, sig_1337) and read_word) or
		(repeat(16, sig_1339) and "00000000" & read_byte);

	-- Behaviour of component 'mux_580' model 'mux'
	mux_580 <=
		(repeat(8, sig_1346) and read_byte) or
		(repeat(8, sig_1348) and next_marker);

	-- Behaviour of component 'mux_569' model 'mux'
	mux_569 <=
		(repeat(8, sig_1027) and decodehuffmcu_bufdim1 & "000000") or
		(repeat(8, sig_1268) and decodehuffmcu_bufdim1 & decodehuffmcu_k(5 downto 0)) or
		(repeat(8, sig_1436) and decode_block_in_buf_idx & sig_1630);

	-- Behaviour of component 'mux_567' model 'mux'
	mux_567 <=
		(repeat(32, sig_1257) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1000) and decodehuffmcu_diff) or
		(repeat(32, sig_1202) and buf_getv) or
		(repeat(32, sig_1267) and or_866);

	-- Behaviour of component 'mux_568' model 'mux'
	mux_568 <=
		(repeat(8, sig_1266) and decodehuffmcu_bufdim1 & decodehuffmcu_k(5 downto 0)) or
		(repeat(8, sig_1000) and decodehuffmcu_bufdim1 & "000000") or
		(repeat(8, sig_1443) and decodehuffmcu_bufdim1 & decodehuffmcu_i(5 downto 0)) or
		(repeat(8, sig_1429) and decode_start_i(1 downto 0) & "000000");

	-- Behaviour of component 'mux_563' model 'mux'
	mux_563 <=
		(repeat(9, sig_1555) and decode_block_out_buf_idx & "011000") or
		(repeat(9, sig_1408) and decode_block_out_buf_idx & "001010") or
		(repeat(9, sig_1407) and decode_block_out_buf_idx & "101010") or
		(repeat(9, sig_1405) and decode_block_out_buf_idx & "010100") or
		(repeat(9, sig_1403) and decode_block_out_buf_idx & "110101") or
		(repeat(9, sig_1401) and decode_block_out_buf_idx & "111000") or
		(repeat(9, sig_1510) and decode_block_out_buf_idx & "101000") or
		(repeat(9, sig_1389) and decode_block_out_buf_idx & "011001") or
		(repeat(9, sig_1388) and decode_block_out_buf_idx & "100110") or
		(repeat(9, sig_1384) and decode_block_out_buf_idx & "111010") or
		(repeat(9, sig_1382) and decode_block_out_buf_idx & "111011") or
		(repeat(9, sig_1381) and decode_block_out_buf_idx & "111100") or
		(repeat(9, sig_1377) and decode_block_out_buf_idx & "000100") or
		(repeat(9, sig_1375) and decode_block_out_buf_idx & "100100") or
		(repeat(9, sig_1372) and decode_block_out_buf_idx & "010010") or
		(repeat(9, sig_1512) and decode_block_out_buf_idx & "000001") or
		(repeat(9, sig_1515) and decode_block_out_buf_idx & "011110") or
		(repeat(9, sig_1517) and decode_block_out_buf_idx & "011100") or
		(repeat(9, sig_1418) and decode_block_out_buf_idx & "111101") or
		(repeat(9, sig_1417) and decode_block_out_buf_idx & "100010") or
		(repeat(9, sig_1415) and decode_block_out_buf_idx & "010111") or
		(repeat(9, sig_1414) and decode_block_out_buf_idx & chenidct_aidx(5 downto 0)) or
		(repeat(9, sig_1451) and decode_block_out_buf_idx & chenidct_i(5 downto 0)) or
		(repeat(9, sig_1469) and decode_block_out_buf_idx & "010000") or
		(repeat(9, sig_1370) and decode_block_out_buf_idx & "000111") or
		(repeat(9, sig_1368) and decode_block_out_buf_idx & "001100") or
		(repeat(9, sig_1366) and decode_block_out_buf_idx & "111111") or
		(repeat(9, sig_1365) and decode_block_out_buf_idx & "101100") or
		(repeat(9, sig_1362) and decode_block_out_buf_idx & "110010") or
		(repeat(9, sig_1331) and decode_block_out_buf_idx & "000101") or
		(repeat(9, sig_1330) and decode_block_out_buf_idx & "010001") or
		(repeat(9, sig_1328) and decode_block_out_buf_idx & "001111") or
		(repeat(9, sig_1326) and decode_block_out_buf_idx & "100111") or
		(repeat(9, sig_1299) and yuvtorgb_yidx & yuvtorgb_i(5 downto 0)) or
		(repeat(9, sig_1281) and decode_block_out_buf_idx & "011101") or
		(repeat(9, sig_1279) and decode_block_out_buf_idx & "101110") or
		(repeat(9, sig_1278) and decode_block_out_buf_idx & "110110") or
		(repeat(9, sig_1265) and decode_block_out_buf_idx & "001110") or
		(repeat(9, sig_1261) and decode_block_out_buf_idx & "001001") or
		(repeat(9, sig_1238) and decode_block_out_buf_idx & "010110") or
		(repeat(9, sig_1232) and decode_block_out_buf_idx & "001011") or
		(repeat(9, sig_1177) and decode_block_out_buf_idx & "111110") or
		(repeat(9, sig_1174) and decode_block_out_buf_idx & "100001") or
		(repeat(9, sig_1171) and decode_block_out_buf_idx & "011111") or
		(repeat(9, sig_1159) and decode_block_out_buf_idx & "000000") or
		(repeat(9, sig_1157) and decode_block_out_buf_idx & "100000") or
		(repeat(9, sig_1153) and decode_block_out_buf_idx & "000010") or
		(repeat(9, sig_1151) and decode_block_out_buf_idx & "010101") or
		(repeat(9, sig_1146) and decode_block_out_buf_idx & "101001") or
		(repeat(9, sig_1144) and decode_block_out_buf_idx & "110111") or
		(repeat(9, sig_1141) and decode_block_out_buf_idx & "001000") or
		(repeat(9, sig_1137) and decode_block_out_buf_idx & "101011") or
		(repeat(9, sig_1134) and decode_block_out_buf_idx & "111001") or
		(repeat(9, sig_1132) and decode_block_out_buf_idx & "000110") or
		(repeat(9, sig_1130) and decode_block_out_buf_idx & "011010") or
		(repeat(9, sig_1126) and decode_block_out_buf_idx & "100101") or
		(repeat(9, sig_1124) and decode_block_out_buf_idx & "011011") or
		(repeat(9, sig_1122) and decode_block_out_buf_idx & "000011") or
		(repeat(9, sig_1120) and decode_block_out_buf_idx & "100011") or
		(repeat(9, sig_1116) and decode_block_out_buf_idx & "001101") or
		(repeat(9, sig_1114) and decode_block_out_buf_idx & "101101") or
		(repeat(9, sig_1108) and decode_block_out_buf_idx & "110011") or
		(repeat(9, sig_1107) and decode_block_out_buf_idx & "010011") or
		(repeat(9, sig_1104) and decode_block_out_buf_idx & "110100") or
		(repeat(9, sig_1102) and decode_block_out_buf_idx & "110000") or
		(repeat(9, sig_1096) and decode_block_out_buf_idx & "101111") or
		(repeat(9, sig_1095) and decode_block_out_buf_idx & "110001") or
		(repeat(9, sig_1088) and decode_block_out_buf_idx & chenidct_i(2 downto 0) & "000");

	-- Behaviour of component 'mux_565' model 'mux'
	mux_565 <=
		(repeat(9, sig_1088) and decode_block_out_buf_idx & chenidct_i(2 downto 0) & "001") or
		(repeat(9, sig_1299) and yuvtorgb_vidx & yuvtorgb_i(5 downto 0));

	-- Behaviour of component 'mux_561' model 'mux'
	mux_561 <=
		(repeat(32, sig_1556) and sig_1610(24 downto 0) & sig_1642(6 downto 0)) or
		(repeat(32, sig_1400) and mux_817) or
		(repeat(32, sig_1399) and mux_819) or
		(repeat(32, sig_1395) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1392) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1390) and mux_821) or
		(repeat(32, sig_1416) and mux_807) or
		(repeat(32, sig_1387) and mux_823) or
		(repeat(32, sig_1386) and mux_825) or
		(repeat(32, sig_1385) and mux_827) or
		(repeat(32, sig_1383) and mux_829) or
		(repeat(32, sig_1380) and mux_831) or
		(repeat(32, sig_1379) and mux_833) or
		(repeat(32, sig_1378) and mux_835) or
		(repeat(32, sig_1374) and mux_837) or
		(repeat(32, sig_1419) and mux_805) or
		(repeat(32, sig_1513) and mux_797) or
		(repeat(32, sig_1516) and mux_795) or
		(repeat(32, sig_1409) and mux_809) or
		(repeat(32, sig_1406) and mux_811) or
		(repeat(32, sig_1404) and mux_813) or
		(repeat(32, sig_1402) and mux_815) or
		(repeat(32, sig_1451) and sig_992) or
		(repeat(32, sig_1475) and mux_776) or
		(repeat(32, sig_1373) and mux_839) or
		(repeat(32, sig_1369) and mux_841) or
		(repeat(32, sig_1364) and mux_843) or
		(repeat(32, sig_1329) and mux_856) or
		(repeat(32, sig_1327) and mux_858) or
		(repeat(32, sig_1263) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1176) and mux_888) or
		(repeat(32, sig_1175) and mux_890) or
		(repeat(32, sig_1173) and mux_892) or
		(repeat(32, sig_1172) and mux_894) or
		(repeat(32, sig_1160) and mux_898) or
		(repeat(32, sig_1158) and mux_900) or
		(repeat(32, sig_1154) and mux_902) or
		(repeat(32, sig_1152) and mux_904) or
		(repeat(32, sig_1150) and mux_906) or
		(repeat(32, sig_1149) and mux_908) or
		(repeat(32, sig_1148) and mux_910) or
		(repeat(32, sig_1147) and mux_912) or
		(repeat(32, sig_1145) and mux_914) or
		(repeat(32, sig_1143) and mux_916) or
		(repeat(32, sig_1142) and mux_918) or
		(repeat(32, sig_1140) and mux_920) or
		(repeat(32, sig_1139) and mux_922) or
		(repeat(32, sig_1138) and mux_924) or
		(repeat(32, sig_1136) and mux_926) or
		(repeat(32, sig_1135) and mux_928) or
		(repeat(32, sig_1133) and mux_930) or
		(repeat(32, sig_1131) and mux_932) or
		(repeat(32, sig_1129) and mux_934) or
		(repeat(32, sig_1128) and mux_936) or
		(repeat(32, sig_1127) and mux_938) or
		(repeat(32, sig_1125) and mux_940) or
		(repeat(32, sig_1123) and mux_942) or
		(repeat(32, sig_1121) and mux_944) or
		(repeat(32, sig_1119) and mux_946) or
		(repeat(32, sig_1117) and mux_948) or
		(repeat(32, sig_1115) and mux_950) or
		(repeat(32, sig_1113) and mux_952) or
		(repeat(32, sig_1109) and mux_954) or
		(repeat(32, sig_1106) and mux_956) or
		(repeat(32, sig_1105) and mux_958) or
		(repeat(32, sig_1103) and mux_960) or
		(repeat(32, sig_1031) and mux_980) or
		(repeat(32, sig_1003) and mux_986) or
		(repeat(32, sig_1002) and mux_988);

	-- Behaviour of component 'mux_562' model 'mux'
	mux_562 <=
		(repeat(9, sig_1555) and decode_block_out_buf_idx & "011000") or
		(repeat(9, sig_1407) and decode_block_out_buf_idx & "101010") or
		(repeat(9, sig_1405) and decode_block_out_buf_idx & "010100") or
		(repeat(9, sig_1403) and decode_block_out_buf_idx & "110101") or
		(repeat(9, sig_1401) and decode_block_out_buf_idx & "111000") or
		(repeat(9, sig_1391) and decode_block_out_buf_idx & chenidct_aidx(5 downto 0)) or
		(repeat(9, sig_1510) and decode_block_out_buf_idx & "101000") or
		(repeat(9, sig_1389) and decode_block_out_buf_idx & "011001") or
		(repeat(9, sig_1388) and decode_block_out_buf_idx & "100110") or
		(repeat(9, sig_1384) and decode_block_out_buf_idx & "111010") or
		(repeat(9, sig_1382) and decode_block_out_buf_idx & "111011") or
		(repeat(9, sig_1381) and decode_block_out_buf_idx & "111100") or
		(repeat(9, sig_1377) and decode_block_out_buf_idx & "000100") or
		(repeat(9, sig_1375) and decode_block_out_buf_idx & "100100") or
		(repeat(9, sig_1372) and decode_block_out_buf_idx & "010010") or
		(repeat(9, sig_1512) and decode_block_out_buf_idx & "000001") or
		(repeat(9, sig_1515) and decode_block_out_buf_idx & "011110") or
		(repeat(9, sig_1517) and decode_block_out_buf_idx & "011100") or
		(repeat(9, sig_1418) and decode_block_out_buf_idx & "111101") or
		(repeat(9, sig_1417) and decode_block_out_buf_idx & "100010") or
		(repeat(9, sig_1415) and decode_block_out_buf_idx & "010111") or
		(repeat(9, sig_1408) and decode_block_out_buf_idx & "001010") or
		(repeat(9, sig_1450) and decode_block_out_buf_idx & chenidct_i(5 downto 0)) or
		(repeat(9, sig_1469) and decode_block_out_buf_idx & "010000") or
		(repeat(9, sig_1370) and decode_block_out_buf_idx & "000111") or
		(repeat(9, sig_1368) and decode_block_out_buf_idx & "001100") or
		(repeat(9, sig_1366) and decode_block_out_buf_idx & "111111") or
		(repeat(9, sig_1365) and decode_block_out_buf_idx & "101100") or
		(repeat(9, sig_1362) and decode_block_out_buf_idx & "110010") or
		(repeat(9, sig_1331) and decode_block_out_buf_idx & "000101") or
		(repeat(9, sig_1330) and decode_block_out_buf_idx & "010001") or
		(repeat(9, sig_1328) and decode_block_out_buf_idx & "001111") or
		(repeat(9, sig_1326) and decode_block_out_buf_idx & "100111") or
		(repeat(9, sig_1281) and decode_block_out_buf_idx & "011101") or
		(repeat(9, sig_1279) and decode_block_out_buf_idx & "101110") or
		(repeat(9, sig_1278) and decode_block_out_buf_idx & "110110") or
		(repeat(9, sig_1265) and decode_block_out_buf_idx & "001110") or
		(repeat(9, sig_1261) and decode_block_out_buf_idx & "001001") or
		(repeat(9, sig_1238) and decode_block_out_buf_idx & "010110") or
		(repeat(9, sig_1232) and decode_block_out_buf_idx & "001011") or
		(repeat(9, sig_1177) and decode_block_out_buf_idx & "111110") or
		(repeat(9, sig_1174) and decode_block_out_buf_idx & "100001") or
		(repeat(9, sig_1171) and decode_block_out_buf_idx & "011111") or
		(repeat(9, sig_1159) and decode_block_out_buf_idx & "000000") or
		(repeat(9, sig_1157) and decode_block_out_buf_idx & "100000") or
		(repeat(9, sig_1153) and decode_block_out_buf_idx & "000010") or
		(repeat(9, sig_1151) and decode_block_out_buf_idx & "010101") or
		(repeat(9, sig_1146) and decode_block_out_buf_idx & "101001") or
		(repeat(9, sig_1144) and decode_block_out_buf_idx & "110111") or
		(repeat(9, sig_1141) and decode_block_out_buf_idx & "001000") or
		(repeat(9, sig_1137) and decode_block_out_buf_idx & "101011") or
		(repeat(9, sig_1134) and decode_block_out_buf_idx & "111001") or
		(repeat(9, sig_1132) and decode_block_out_buf_idx & "000110") or
		(repeat(9, sig_1130) and decode_block_out_buf_idx & "011010") or
		(repeat(9, sig_1126) and decode_block_out_buf_idx & "100101") or
		(repeat(9, sig_1124) and decode_block_out_buf_idx & "011011") or
		(repeat(9, sig_1122) and decode_block_out_buf_idx & "000011") or
		(repeat(9, sig_1120) and decode_block_out_buf_idx & "100011") or
		(repeat(9, sig_1116) and decode_block_out_buf_idx & "001101") or
		(repeat(9, sig_1114) and decode_block_out_buf_idx & "101101") or
		(repeat(9, sig_1108) and decode_block_out_buf_idx & "110011") or
		(repeat(9, sig_1107) and decode_block_out_buf_idx & "010011") or
		(repeat(9, sig_1104) and decode_block_out_buf_idx & "110100") or
		(repeat(9, sig_1102) and decode_block_out_buf_idx & "110000") or
		(repeat(9, sig_1096) and decode_block_out_buf_idx & "101111") or
		(repeat(9, sig_1095) and decode_block_out_buf_idx & "110001") or
		(repeat(9, sig_1087) and decode_block_out_buf_idx & chenidct_i(2 downto 0) & "000") or
		(repeat(9, sig_1083) and decode_block_out_buf_idx & chenidct_i(2 downto 0) & "001");

	-- Behaviour of component 'mux_557' model 'mux'
	mux_557 <=
		(repeat(32, sig_1436) and sig_1643) or
		(repeat(32, sig_1433) and sig_1628(31 downto 0));

	-- Behaviour of component 'mux_558' model 'mux'
	mux_558 <=
		(repeat(6, sig_1564) and "000101") or
		(repeat(6, sig_1321) and "110001") or
		(repeat(6, sig_1320) and "000110") or
		(repeat(6, sig_1315) and "010101") or
		(repeat(6, sig_1311) and "011111") or
		(repeat(6, sig_1301) and "100101") or
		(repeat(6, sig_1367) and "111010") or
		(repeat(6, sig_1293) and "100111") or
		(repeat(6, sig_1277) and "000010") or
		(repeat(6, sig_1276) and "111001") or
		(repeat(6, sig_1275) and "010001") or
		(repeat(6, sig_1270) and "110000") or
		(repeat(6, sig_1260) and "101001") or
		(repeat(6, sig_1259) and "111100") or
		(repeat(6, sig_1258) and "011000") or
		(repeat(6, sig_1371) and "110111") or
		(repeat(6, sig_1410) and "011001") or
		(repeat(6, sig_1508) and "001101") or
		(repeat(6, sig_1361) and "101100") or
		(repeat(6, sig_1359) and "001000") or
		(repeat(6, sig_1358) and "101011") or
		(repeat(6, sig_1436) and izigzagmatrix_out_idx(5 downto 0)) or
		(repeat(6, sig_1432) and "010010") or
		(repeat(6, sig_1256) and "010110") or
		(repeat(6, sig_1255) and "000011") or
		(repeat(6, sig_1246) and "100011") or
		(repeat(6, sig_1239) and "100001") or
		(repeat(6, sig_1235) and "100100") or
		(repeat(6, sig_1231) and "100110") or
		(repeat(6, sig_1230) and "100000") or
		(repeat(6, sig_1228) and "110101") or
		(repeat(6, sig_1227) and "101101") or
		(repeat(6, sig_1226) and "011110") or
		(repeat(6, sig_1225) and "000100") or
		(repeat(6, sig_1223) and "000111") or
		(repeat(6, sig_1222) and "110110") or
		(repeat(6, sig_1221) and "011101") or
		(repeat(6, sig_1220) and "101110") or
		(repeat(6, sig_1166) and "001110") or
		(repeat(6, sig_1164) and "110100") or
		(repeat(6, sig_1156) and "010100") or
		(repeat(6, sig_1155) and "101010") or
		(repeat(6, sig_1099) and "011010") or
		(repeat(6, sig_1098) and "101111") or
		(repeat(6, sig_1097) and "010011") or
		(repeat(6, sig_1094) and "010111") or
		(repeat(6, sig_1093) and "111000") or
		(repeat(6, sig_1092) and "011100") or
		(repeat(6, sig_1091) and "000001") or
		(repeat(6, sig_1090) and "001001") or
		(repeat(6, sig_1086) and "001011") or
		(repeat(6, sig_1085) and "110010") or
		(repeat(6, sig_1084) and "010000") or
		(repeat(6, sig_1079) and "001111") or
		(repeat(6, sig_1076) and "001010") or
		(repeat(6, sig_1075) and "110011") or
		(repeat(6, sig_1074) and "111111") or
		(repeat(6, sig_1071) and "011011") or
		(repeat(6, sig_1063) and "001100") or
		(repeat(6, sig_1054) and "101000") or
		(repeat(6, sig_1050) and "100010") or
		(repeat(6, sig_1028) and "111101") or
		(repeat(6, sig_1022) and "111110") or
		(repeat(6, sig_1007) and "111011");

	-- Behaviour of component 'mux_559' model 'mux'
	mux_559 <=
		(repeat(6, sig_1581) and chenidct_i(5 downto 0)) or
		(repeat(6, sig_1358) and "101011") or
		(repeat(6, sig_1321) and "110001") or
		(repeat(6, sig_1320) and "000110") or
		(repeat(6, sig_1315) and "010101") or
		(repeat(6, sig_1311) and "011111") or
		(repeat(6, sig_1371) and "110111") or
		(repeat(6, sig_1301) and "100101") or
		(repeat(6, sig_1293) and "100111") or
		(repeat(6, sig_1277) and "000010") or
		(repeat(6, sig_1276) and "111001") or
		(repeat(6, sig_1275) and "010001") or
		(repeat(6, sig_1270) and "110000") or
		(repeat(6, sig_1260) and "101001") or
		(repeat(6, sig_1259) and "111100") or
		(repeat(6, sig_1410) and "011001") or
		(repeat(6, sig_1508) and "001101") or
		(repeat(6, sig_1564) and "000101") or
		(repeat(6, sig_1367) and "111010") or
		(repeat(6, sig_1361) and "101100") or
		(repeat(6, sig_1359) and "001000") or
		(repeat(6, sig_1473) and chenidct_aidx(5 downto 0)) or
		(repeat(6, sig_1432) and "010010") or
		(repeat(6, sig_1258) and "011000") or
		(repeat(6, sig_1256) and "010110") or
		(repeat(6, sig_1255) and "000011") or
		(repeat(6, sig_1246) and "100011") or
		(repeat(6, sig_1239) and "100001") or
		(repeat(6, sig_1235) and "100100") or
		(repeat(6, sig_1231) and "100110") or
		(repeat(6, sig_1230) and "100000") or
		(repeat(6, sig_1228) and "110101") or
		(repeat(6, sig_1227) and "101101") or
		(repeat(6, sig_1226) and "011110") or
		(repeat(6, sig_1225) and "000100") or
		(repeat(6, sig_1223) and "000111") or
		(repeat(6, sig_1222) and "110110") or
		(repeat(6, sig_1221) and "011101") or
		(repeat(6, sig_1220) and "101110") or
		(repeat(6, sig_1166) and "001110") or
		(repeat(6, sig_1164) and "110100") or
		(repeat(6, sig_1156) and "010100") or
		(repeat(6, sig_1155) and "101010") or
		(repeat(6, sig_1099) and "011010") or
		(repeat(6, sig_1098) and "101111") or
		(repeat(6, sig_1097) and "010011") or
		(repeat(6, sig_1094) and "010111") or
		(repeat(6, sig_1093) and "111000") or
		(repeat(6, sig_1092) and "011100") or
		(repeat(6, sig_1091) and "000001") or
		(repeat(6, sig_1090) and "001001") or
		(repeat(6, sig_1086) and "001011") or
		(repeat(6, sig_1085) and "110010") or
		(repeat(6, sig_1084) and "010000") or
		(repeat(6, sig_1079) and "001111") or
		(repeat(6, sig_1076) and "001010") or
		(repeat(6, sig_1075) and "110011") or
		(repeat(6, sig_1074) and "111111") or
		(repeat(6, sig_1071) and "011011") or
		(repeat(6, sig_1063) and "001100") or
		(repeat(6, sig_1054) and "101000") or
		(repeat(6, sig_1050) and "100010") or
		(repeat(6, sig_1028) and "111101") or
		(repeat(6, sig_1022) and "111110") or
		(repeat(6, sig_1007) and "111011");

	-- Behaviour of component 'mux_555' model 'mux'
	mux_555 <=
		(repeat(32, sig_1396) and sig_1613) or
		(repeat(32, sig_1449) and sig_1612(31 downto 0));

	-- Behaviour of component 'mux_551' model 'mux'
	mux_551 <=
		(repeat(32, sig_1118) and sig_1587(39 downto 8)) or
		(repeat(32, sig_1088) and sig_1640) or
		(repeat(32, sig_1332) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1463) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_553' model 'mux'
	mux_553 <=
		(repeat(32, sig_1411) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1111) and sig_1609(28 downto 0) & chenidct_aidx(2 downto 0)) or
		(repeat(32, sig_1262) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1582) and sig_1610(28 downto 0) & chenidct_i(2 downto 0)) or
		(repeat(32, sig_1477) and sig_1610(28 downto 0) & chenidct_aidx(2 downto 0));

	-- Behaviour of component 'mux_549' model 'mux'
	mux_549 <=
		(repeat(32, sig_1323) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1274) and sig_1642) or
		(repeat(32, sig_1324) and sig_1587(39 downto 8)) or
		(repeat(32, sig_1463) and sig_1614(31 downto 0));

	-- Behaviour of component 'mux_545' model 'mux'
	mux_545 <=
		(repeat(32, sig_1118) and sig_1612(38 downto 7)) or
		(repeat(32, sig_1040) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1351) and sig_1642) or
		(repeat(32, sig_1463) and sig_1613);

	-- Behaviour of component 'mux_547' model 'mux'
	mux_547 <=
		(repeat(32, sig_1349) and sig_1614(40 downto 9)) or
		(repeat(32, sig_1001) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1413) and sig_1642) or
		(repeat(32, sig_1463) and sig_1624(31 downto 0));

	-- Behaviour of component 'mux_543' model 'mux'
	mux_543 <=
		(repeat(32, sig_1088) and sig_1642) or
		(repeat(32, sig_1581) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1463) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_731' model 'mux'
	mux_731 <=
		(repeat(8, sig_1508) and iquantize_qidx & "001101") or
		(repeat(8, sig_1320) and iquantize_qidx & "000110") or
		(repeat(8, sig_1315) and iquantize_qidx & "010101") or
		(repeat(8, sig_1311) and iquantize_qidx & "011111") or
		(repeat(8, sig_1301) and iquantize_qidx & "100101") or
		(repeat(8, sig_1293) and iquantize_qidx & "100111") or
		(repeat(8, sig_1361) and iquantize_qidx & "101100") or
		(repeat(8, sig_1277) and iquantize_qidx & "000010") or
		(repeat(8, sig_1276) and iquantize_qidx & "111001") or
		(repeat(8, sig_1275) and iquantize_qidx & "010001") or
		(repeat(8, sig_1270) and iquantize_qidx & "110000") or
		(repeat(8, sig_1260) and iquantize_qidx & "101001") or
		(repeat(8, sig_1259) and iquantize_qidx & "111100") or
		(repeat(8, sig_1258) and iquantize_qidx & "011000") or
		(repeat(8, sig_1256) and iquantize_qidx & "010110") or
		(repeat(8, sig_1367) and iquantize_qidx & "111010") or
		(repeat(8, sig_1371) and iquantize_qidx & "110111") or
		(repeat(8, sig_1410) and iquantize_qidx & "011001") or
		(repeat(8, sig_1360) and iquantize_qidx & "000000") or
		(repeat(8, sig_1359) and iquantize_qidx & "001000") or
		(repeat(8, sig_1358) and iquantize_qidx & "101011") or
		(repeat(8, sig_1321) and iquantize_qidx & "110001") or
		(repeat(8, sig_1564) and iquantize_qidx & "000101") or
		(repeat(8, sig_1432) and iquantize_qidx & "010010") or
		(repeat(8, sig_1255) and iquantize_qidx & "000011") or
		(repeat(8, sig_1246) and iquantize_qidx & "100011") or
		(repeat(8, sig_1239) and iquantize_qidx & "100001") or
		(repeat(8, sig_1235) and iquantize_qidx & "100100") or
		(repeat(8, sig_1231) and iquantize_qidx & "100110") or
		(repeat(8, sig_1230) and iquantize_qidx & "100000") or
		(repeat(8, sig_1228) and iquantize_qidx & "110101") or
		(repeat(8, sig_1227) and iquantize_qidx & "101101") or
		(repeat(8, sig_1226) and iquantize_qidx & "011110") or
		(repeat(8, sig_1225) and iquantize_qidx & "000100") or
		(repeat(8, sig_1223) and iquantize_qidx & "000111") or
		(repeat(8, sig_1222) and iquantize_qidx & "110110") or
		(repeat(8, sig_1221) and iquantize_qidx & "011101") or
		(repeat(8, sig_1220) and iquantize_qidx & "101110") or
		(repeat(8, sig_1166) and iquantize_qidx & "001110") or
		(repeat(8, sig_1164) and iquantize_qidx & "110100") or
		(repeat(8, sig_1156) and iquantize_qidx & "010100") or
		(repeat(8, sig_1155) and iquantize_qidx & "101010") or
		(repeat(8, sig_1099) and iquantize_qidx & "011010") or
		(repeat(8, sig_1098) and iquantize_qidx & "101111") or
		(repeat(8, sig_1097) and iquantize_qidx & "010011") or
		(repeat(8, sig_1094) and iquantize_qidx & "010111") or
		(repeat(8, sig_1093) and iquantize_qidx & "111000") or
		(repeat(8, sig_1092) and iquantize_qidx & "011100") or
		(repeat(8, sig_1091) and iquantize_qidx & "000001") or
		(repeat(8, sig_1090) and iquantize_qidx & "001001") or
		(repeat(8, sig_1086) and iquantize_qidx & "001011") or
		(repeat(8, sig_1085) and iquantize_qidx & "110010") or
		(repeat(8, sig_1084) and iquantize_qidx & "010000") or
		(repeat(8, sig_1079) and iquantize_qidx & "001111") or
		(repeat(8, sig_1076) and iquantize_qidx & "001010") or
		(repeat(8, sig_1075) and iquantize_qidx & "110011") or
		(repeat(8, sig_1074) and iquantize_qidx & "111111") or
		(repeat(8, sig_1071) and iquantize_qidx & "011011") or
		(repeat(8, sig_1063) and iquantize_qidx & "001100") or
		(repeat(8, sig_1054) and iquantize_qidx & "101000") or
		(repeat(8, sig_1050) and iquantize_qidx & "100010") or
		(repeat(8, sig_1028) and iquantize_qidx & "111101") or
		(repeat(8, sig_1022) and iquantize_qidx & "111110") or
		(repeat(8, sig_1007) and iquantize_qidx & "111011");

	-- Behaviour of component 'mux_727' model 'mux'
	mux_727 <=
		(repeat(7, sig_1534) and huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_i_c0(5 downto 0)) or
		(repeat(7, sig_1552) and huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_l(5 downto 0)) or
		(repeat(7, sig_1458) and get_dht_index & get_dht_i(5 downto 0));

	-- Behaviour of component 'mux_723' model 'mux'
	mux_723 <=
		(repeat(10, sig_1304) and decodehuffman_dc_tbl_no & decodehuffman_dc_p) or
		(repeat(10, sig_1480) and get_dht_index & get_dht_i(8 downto 0));

	-- Behaviour of component 'mux_719' model 'mux'
	mux_719 <=
		(repeat(7, sig_1505) and huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_i_c0(5 downto 0)) or
		(repeat(7, sig_1547) and huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_l(5 downto 0)) or
		(repeat(7, sig_1458) and get_dht_index & get_dht_i(5 downto 0));

	-- Behaviour of component 'mux_539' model 'mux'
	mux_539 <=
		(repeat(32, sig_1118) and sig_1624(31 downto 0)) or
		(repeat(32, sig_1354) and sig_1642) or
		(repeat(32, sig_1472) and sig_1639(29 downto 0) & "00");

	-- Behaviour of component 'mux_541' model 'mux'
	mux_541 <=
		(repeat(32, sig_999) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1118) and sig_1613) or
		(repeat(32, sig_1357) and sig_1642);

	-- Behaviour of component 'mux_537' model 'mux'
	mux_537 <=
		(repeat(32, sig_1285) and sig_1642) or
		(repeat(32, sig_1325) and sig_1639(29 downto 0) & "00") or
		(repeat(32, sig_1463) and sig_1621(31 downto 0));

	-- Behaviour of component 'mux_533' model 'mux'
	mux_533 <=
		(repeat(32, sig_1324) and sig_1614(40 downto 9)) or
		(repeat(32, sig_1395) and sig_1627(39 downto 8));

	-- Behaviour of component 'mux_535' model 'mux'
	mux_535 <=
		(repeat(32, sig_1118) and sig_1614(40 downto 9)) or
		(repeat(32, sig_1463) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_715' model 'mux'
	mux_715 <=
		(repeat(10, sig_1284) and decodehuffman_ac_tbl_no & decodehuffman_ac_p) or
		(repeat(10, sig_1480) and get_dht_index & get_dht_i(8 downto 0));

	-- Behaviour of component 'mux_711' model 'mux'
	mux_711 <=
		(sig_1170 and decodehuffmcu_tbl_no) or
		(sig_1189 and '1');

	-- Behaviour of component 'mux_705' model 'mux'
	mux_705 <=
		(repeat(32, sig_1271) and sig_1632) or
		(repeat(32, sig_1554) and "11111111111111111111111111111111") or
		(repeat(32, sig_1561) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_706' model 'mux'
	mux_706 <=
		(repeat(7, sig_1553) and huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_l(5 downto 0)) or
		(repeat(7, sig_1561) and huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_p_dhtbl_ml(5 downto 0));

	-- Behaviour of component 'mux_707' model 'mux'
	mux_707 <=
		(repeat(7, sig_1561) and huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_p_dhtbl_ml(5 downto 0)) or
		(repeat(7, sig_1575) and decodehuffman_dc_tbl_no & decodehuffman_dc_l(5 downto 0)) or
		(repeat(7, sig_1577) and decodehuffman_dc_tbl_no & decodehuffman_dc_dhuff_ml);

	-- Behaviour of component 'mux_531' model 'mux'
	mux_531 <=
		(repeat(32, sig_1324) and sig_1609(38 downto 7)) or
		(repeat(32, sig_1395) and sig_1628(39 downto 8));

	-- Behaviour of component 'mux_529' model 'mux'
	mux_529 <=
		(repeat(32, sig_1118) and sig_1610(38 downto 7)) or
		(repeat(32, sig_1463) and sig_1613);

	-- Behaviour of component 'mux_695' model 'mux'
	mux_695 <=
		(sig_1184 and '1') or
		(sig_1453 and decodehuffmcu_tbl_no);

	-- Behaviour of component 'mux_524' model 'mux'
	mux_524 <=
		(repeat(5, sig_1310) and decodehuffmcu_s(4 downto 0)) or
		(repeat(5, sig_1482) and read_position(4 downto 0));

	-- Behaviour of component 'mux_521' model 'mux'
	mux_521 <=
		(repeat(32, sig_1422) and "000000000000000000000000" & pgetc) or
		(repeat(32, sig_1493) and or_802 & pgetc);

	-- Behaviour of component 'mux_519' model 'mux'
	mux_519 <=
		(repeat(32, sig_1484) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1355) and sig_1624(31 downto 0)) or
		(repeat(32, sig_1421) and "00000000000000000000000000000111") or
		(repeat(32, sig_1493) and sig_1610(28 downto 0) & read_position(2 downto 0)) or
		(repeat(32, sig_1497) and "11111111111111111111111111111111");

	-- Behaviour of component 'mux_517' model 'mux'
	mux_517 <=
		(repeat(8, sig_1423) and "11111111") or
		(repeat(8, sig_1425) and pgetc_temp);

	-- Behaviour of component 'mux_507' model 'mux'
	mux_507 <=
		(repeat(32, sig_1008) and and_984) or
		(repeat(32, sig_1345) and and_853) or
		(repeat(32, sig_1497) and and_801);

	-- Behaviour of component 'mux_505' model 'mux'
	mux_505 <=
		(repeat(32, sig_1167) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1197) and decodehuffmcu_s) or
		(repeat(32, sig_1201) and decodehuffman_dc);

	-- Behaviour of component 'mux_501' model 'mux'
	mux_501 <=
		(repeat(32, sig_1355) and or_845) or
		(repeat(32, sig_1489) and sig_1626);

	-- Behaviour of component 'mux_492' model 'mux'
	mux_492 <=
		(repeat(32, sig_1186) and sig_1652) or
		(repeat(32, sig_1514) and "00000000000000000000000000000001") or
		(repeat(32, sig_1544) and huff_make_dhuff_tb_ac_l);

	-- Behaviour of component 'mux_488' model 'mux'
	mux_488 <=
		(repeat(32, sig_1499) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1504) and "00000000000000000000000000000001");

	-- Behaviour of component 'mux_490' model 'mux'
	mux_490 <=
		(repeat(32, sig_1498) and "00000000000000000000000000000001") or
		(repeat(32, sig_1507) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_486' model 'mux'
	mux_486 <=
		(repeat(32, sig_1500) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1544) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_482' model 'mux'
	mux_482 <=
		(repeat(32, sig_1283) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1558) and sig_1635);

	-- Behaviour of component 'mux_484' model 'mux'
	mux_484 <=
		(repeat(32, sig_1023) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1283) and huff_make_dhuff_tb_ac_code(30 downto 0) & '0');

	-- Behaviour of component 'mux_480' model 'mux'
	mux_480 <=
		(repeat(32, sig_1514) and "00000000000000000000000000000001") or
		(repeat(32, sig_1525) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_476' model 'mux'
	mux_476 <=
		(repeat(32, sig_1499) and huff_make_dhuff_tb_ac_i_c0);

	-- Behaviour of component 'mux_478' model 'mux'
	mux_478 <=
		(repeat(9, sig_1511) and huff_make_dhuff_tb_ac_p(8 downto 0));

	-- Behaviour of component 'mux_459' model 'mux'
	mux_459 <=
		(repeat(32, sig_1038) and huff_make_dhuff_tb_dc_l) or
		(repeat(32, sig_1305) and sig_1656) or
		(repeat(32, sig_1542) and "00000000000000000000000000000001");

	-- Behaviour of component 'mux_455' model 'mux'
	mux_455 <=
		(repeat(32, sig_1527) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1533) and "00000000000000000000000000000001");

	-- Behaviour of component 'mux_457' model 'mux'
	mux_457 <=
		(repeat(32, sig_1526) and "00000000000000000000000000000001") or
		(repeat(32, sig_1536) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_453' model 'mux'
	mux_453 <=
		(repeat(32, sig_1038) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1528) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_449' model 'mux'
	mux_449 <=
		(repeat(32, sig_1033) and sig_1633) or
		(repeat(32, sig_1068) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_451' model 'mux'
	mux_451 <=
		(repeat(32, sig_1035) and sig_1609(31 downto 0)) or
		(repeat(32, sig_1068) and huff_make_dhuff_tb_dc_code(30 downto 0) & '0');

	-- Behaviour of component 'mux_447' model 'mux'
	mux_447 <=
		(repeat(32, sig_1542) and "00000000000000000000000000000001") or
		(repeat(32, sig_1563) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_443' model 'mux'
	mux_443 <=
		(repeat(32, sig_1527) and huff_make_dhuff_tb_dc_i_c0);

	-- Behaviour of component 'mux_445' model 'mux'
	mux_445 <=
		(repeat(9, sig_1537) and huff_make_dhuff_tb_dc_p(8 downto 0));

	-- Behaviour of component 'mux_430' model 'mux'
	mux_430 <=
		(repeat(32, sig_1284) and sig_1657);

	-- Behaviour of component 'mux_422' model 'mux'
	mux_422 <=
		(repeat(32, sig_1565) and "00000000000000000000000000000001") or
		(repeat(32, sig_1567) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_424' model 'mux'
	mux_424 <=
		(repeat(32, sig_1565) and "0000000000000000000000000000000" & buf_getb) or
		(repeat(32, sig_1567) and sig_1610(30 downto 0) & buf_getb);

	-- Behaviour of component 'mux_416' model 'mux'
	mux_416 <=
		(repeat(32, sig_1304) and sig_1659);

	-- Behaviour of component 'mux_410' model 'mux'
	mux_410 <=
		(repeat(32, sig_1571) and "0000000000000000000000000000000" & buf_getb) or
		(repeat(32, sig_1574) and sig_1610(30 downto 0) & buf_getb);

	-- Behaviour of component 'mux_408' model 'mux'
	mux_408 <=
		(repeat(32, sig_1571) and "00000000000000000000000000000001") or
		(repeat(32, sig_1574) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_398' model 'mux'
	mux_398 <=
		(repeat(32, sig_1026) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1341) and buf_getv) or
		(repeat(32, sig_1344) and or_854);

	-- Behaviour of component 'mux_400' model 'mux'
	mux_400 <=
		(repeat(32, sig_1030) and and_982) or
		(repeat(32, sig_1342) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1579) and decodehuffman_dc);

	-- Behaviour of component 'mux_392' model 'mux'
	mux_392 <=
		(repeat(32, sig_1454) and "00000000000000000000000000000001") or
		(repeat(32, sig_1466) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1464) and sig_1610(27 downto 0) & decodehuffmcu_k(3 downto 0));

	-- Behaviour of component 'mux_394' model 'mux'
	mux_394 <=
		(repeat(32, sig_1443) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1445) and "00000000000000000000000000000001");

	-- Behaviour of component 'mux_378' model 'mux'
	mux_378 <=
		(repeat(8, sig_1070) and yuvtorgb_r(7 downto 0)) or
		(repeat(8, sig_1234) and yuvtorgb_b(7 downto 0)) or
		(repeat(8, sig_1237) and yuvtorgb_g(7 downto 0));

	-- Behaviour of component 'mux_379' model 'mux'
	mux_379 <=
		(repeat(10, sig_1070) and yuvtorgb_p & "00" & yuvtorgb_i(5 downto 0)) or
		(repeat(10, sig_1234) and yuvtorgb_p & "10" & yuvtorgb_i(5 downto 0)) or
		(repeat(10, sig_1237) and yuvtorgb_p & "01" & yuvtorgb_i(5 downto 0));

	-- Behaviour of component 'mux_375' model 'mux'
	mux_375 <=
		(repeat(2, sig_1020) and write4blocks_i) or
		(repeat(2, sig_1196) and decode_start_i(1 downto 0));

	-- Behaviour of component 'mux_373' model 'mux'
	mux_373 <=
		(repeat(2, sig_1005) and "10") or
		(repeat(2, sig_1004) and "11") or
		(repeat(2, sig_1019) and "01");

	-- Behaviour of component 'mux_365' model 'mux'
	mux_365 <=
		(repeat(32, sig_1005) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1021) and sig_1610(28 downto 0) & write4blocks_hoffs(2 downto 0)) or
		(repeat(32, sig_1196) and sig_1647(28 downto 0) & "000");

	-- Behaviour of component 'mux_367' model 'mux'
	mux_367 <=
		(repeat(32, sig_1005) and sig_1610(28 downto 0) & write4blocks_voffs(2 downto 0)) or
		(repeat(32, sig_1021) and write4blocks_voffs) or
		(repeat(32, sig_1196) and sig_1648(28 downto 0) & "000");

	-- Behaviour of component 'mux_363' model 'mux'
	mux_363 <=
		(repeat(32, sig_1018) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1065) and writeoneblock_voffs);

	-- Behaviour of component 'mux_359' model 'mux'
	mux_359 <=
		(repeat(32, sig_1012) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_361' model 'mux'
	mux_361 <=
		(repeat(32, sig_1017) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1081) and writeoneblock_hoffs);

	-- Behaviour of component 'mux_347' model 'mux'
	mux_347 <=
		(repeat(32, sig_1005) and sig_1610(28 downto 0) & write4blocks_voffs(2 downto 0)) or
		(repeat(32, sig_1194) and sig_1648(28 downto 0) & "000");

	-- Behaviour of component 'mux_345' model 'mux'
	mux_345 <=
		(repeat(32, sig_1005) and sig_1614(31 downto 0)) or
		(repeat(32, sig_1021) and sig_1610(28 downto 0) & write4blocks_hoffs(2 downto 0)) or
		(repeat(32, sig_1194) and sig_1647(28 downto 0) & "000");

	-- Behaviour of component 'mux_341' model 'mux'
	mux_341 <=
		(repeat(3, sig_993) and decode_start_i(2 downto 0));

	-- Behaviour of component 'mux_343' model 'mux'
	mux_343 <=
		(repeat(2, sig_993) and decode_start_i(1 downto 0));

	-- Behaviour of component 'mux_339' model 'mux'
	mux_339 <=
		(repeat(3, sig_993) and "100") or
		(repeat(3, sig_997) and "001");

	-- Behaviour of component 'mux_335' model 'mux'
	mux_335 <=
		(repeat(32, sig_1060) and mux_965) or
		(repeat(32, sig_1217) and sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24) & sig_1611(24 downto 1));

	-- Behaviour of component 'mux_337' model 'mux'
	mux_337 <=
		(repeat(3, sig_993) and "101") or
		(repeat(3, sig_997) and "010");

	-- Behaviour of component 'mux_333' model 'mux'
	mux_333 <=
		(repeat(32, sig_1060) and mux_969) or
		(repeat(32, sig_1217) and sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24) & sig_1610(24 downto 1));

	-- Behaviour of component 'mux_331' model 'mux'
	mux_331 <=
		(repeat(32, sig_1060) and mux_967) or
		(repeat(32, sig_1217) and sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24) & sig_1613(24 downto 1));

	-- Behaviour of component 'mux_323' model 'mux'
	mux_323 <=
		(repeat(6, sig_1345) and buf_getv_p(5 downto 0)) or
		(repeat(6, sig_1355) and sig_1614(5 downto 0));

	-- Behaviour of component 'mux_320' model 'mux'
	mux_320 <=
		(repeat(32, sig_1234) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_322' model 'mux'
	mux_322 <=
		(repeat(32, sig_1345) and current_read_byte) or
		(repeat(32, sig_1355) and "000000000000000000000000" & pgetc);

	-- Behaviour of component 'mux_317' model 'mux'
	mux_317 <=
		(repeat(2, sig_995) and "01") or
		(repeat(2, sig_994) and "10") or
		(repeat(2, sig_1045) and decode_start_i(1 downto 0));

	-- Behaviour of component 'mux_314' model 'mux'
	mux_314 <=
		(repeat(32, sig_1324) and chenidct_a2) or
		(repeat(32, sig_1118) and chenidct_a3) or
		(repeat(32, sig_1217) and yuvtorgb_v(30) & yuvtorgb_v(30 downto 0)) or
		(repeat(32, sig_1349) and chenidct_b3) or
		(repeat(32, sig_1395) and sig_1614(31 downto 0));

	-- Behaviour of component 'mux_315' model 'mux'
	mux_315 <=
		(repeat(32, sig_1349) and "00000000000000000000000000110001") or
		(repeat(32, sig_1101) and p_jinfo_mcuwidth) or
		(repeat(32, sig_1118) and "00000000000000000000000000011001") or
		(repeat(32, sig_1217) and "00000000000000000000000000001011") or
		(repeat(32, sig_1324) and "00000000000000000000000011010101") or
		(repeat(32, sig_1081) and writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12) & writeoneblock_i(12 downto 0)) or
		(repeat(32, sig_1395) and "00000000000000000000000010110101") or
		(repeat(32, sig_1433) and sig_1661);

	-- Behaviour of component 'mux_316' model 'mux'
	mux_316 <=
		(repeat(32, sig_1349) and chenidct_b2) or
		(repeat(32, sig_1101) and p_jinfo_mcuheight) or
		(repeat(32, sig_1118) and chenidct_a0) or
		(repeat(32, sig_1217) and yuvtorgb_u(28) & yuvtorgb_u(28) & yuvtorgb_u(28) & yuvtorgb_u(28 downto 0)) or
		(repeat(32, sig_1324) and chenidct_a1) or
		(repeat(32, sig_1081) and writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12) & writeoneblock_width(12 downto 0)) or
		(repeat(32, sig_1395) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1433) and sig_1639);

	-- Behaviour of component 'mux_313' model 'mux'
	mux_313 <=
		(repeat(9, sig_1324) and "001000111") or
		(repeat(9, sig_1118) and "011111011") or
		(repeat(9, sig_1217) and "001011011") or
		(repeat(9, sig_1349) and "111011001") or
		(repeat(9, sig_1395) and "010110101");

	-- Behaviour of component 'mux_308' model 'mux'
	mux_308 <=
		(repeat(3, sig_994) and "101") or
		(repeat(3, sig_995) and "100") or
		(repeat(3, sig_1046) and decode_start_i(2 downto 0));

	-- Behaviour of component 'mux_306' model 'mux'
	mux_306 <=
		(repeat(41, sig_1451) and "00000000000000000000000000000000000001000") or
		(repeat(41, sig_1299) and "00000000000000000000000000000000010000000") or
		(repeat(41, sig_1308) and "00000000000000000000000000000000000000001") or
		(repeat(41, sig_1324) and chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1) or
		(repeat(41, sig_1355) and buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p) or
		(repeat(41, sig_1217) and sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30) & sig_1627(30 downto 0) & '0') or
		(repeat(41, sig_1161) and "00000000000000000000000000000000000000010") or
		(repeat(41, sig_1118) and chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2) or
		(repeat(41, sig_1470) and "00000000000000000000000000000000011111111") or
		(repeat(41, sig_1463) and chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2);

	-- Behaviour of component 'mux_307' model 'mux'
	mux_307 <=
		(repeat(41, sig_1355) and "00000000000000000000000000000000000000111") or
		(repeat(41, sig_1217) and sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31) & sig_1614(31 downto 0)) or
		(repeat(41, sig_1299) and sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30) & sig_1641(30 downto 0)) or
		(repeat(41, sig_1309) and p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width) or
		(repeat(41, sig_1324) and chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0) or
		(repeat(41, sig_1216) and sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648) or
		(repeat(41, sig_1118) and chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1) or
		(repeat(41, sig_1060) and yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r) or
		(repeat(41, sig_1468) and sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642) or
		(repeat(41, sig_1463) and chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3(31) & chenidct_c3);

	-- Behaviour of component 'mux_302' model 'mux'
	mux_302 <=
		(repeat(41, sig_1216) and p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth) or
		(repeat(41, sig_1470) and "00000000000000000000000000000000011111111") or
		(repeat(41, sig_1463) and chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3);

	-- Behaviour of component 'mux_303' model 'mux'
	mux_303 <=
		(repeat(41, sig_1216) and sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647) or
		(repeat(41, sig_1060) and yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r) or
		(repeat(41, sig_1471) and sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642) or
		(repeat(41, sig_1463) and chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0);

	-- Behaviour of component 'mux_294' model 'mux'
	mux_294 <=
		(repeat(2, sig_995) and "01") or
		(repeat(2, sig_994) and "10") or
		(repeat(2, sig_1045) and decode_start_i(1 downto 0));

	-- Behaviour of component 'mux_290' model 'mux'
	mux_290 <=
		(repeat(41, sig_1395) and chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1) or
		(repeat(41, sig_1376) and "00000000000000000000000000000000000000111") or
		(repeat(41, sig_1363) and chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2) or
		(repeat(41, sig_1355) and buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5) & buf_getv_p(5 downto 0)) or
		(repeat(41, sig_1349) and sig_1627) or
		(repeat(41, sig_1534) and sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660) or
		(repeat(41, sig_1324) and sig_1667(38 downto 0) & "00") or
		(repeat(41, sig_1318) and "00000000000000000000000000000000000000010") or
		(repeat(41, sig_1313) and get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count) or
		(repeat(41, sig_1300) and "00000000000000000000000000000000000010001") or
		(repeat(41, sig_1299) and "00000000000000000000000000000000010000000") or
		(repeat(41, sig_1292) and sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654(8) & sig_1654) or
		(repeat(41, sig_1289) and sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650(8) & sig_1650) or
		(repeat(41, sig_1280) and chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0) or
		(repeat(41, sig_1550) and "00000000000000000000000000000000000010000") or
		(repeat(41, sig_1569) and sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651) or
		(repeat(41, sig_1576) and sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655) or
		(repeat(41, sig_1505) and sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658) or
		(repeat(41, sig_1491) and "00000000000000000000000000000000000001000") or
		(repeat(41, sig_1486) and "00000000000000000000000000000000000010111") or
		(repeat(41, sig_1485) and "00000000000000000000000000000000000000001") or
		(repeat(41, sig_1440) and chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1) or
		(repeat(41, sig_1434) and "00000000000000000000000000000000000111111") or
		(repeat(41, sig_1244) and "000000000000000000000000000000000" & p_jinfo_num_components) or
		(repeat(41, sig_1241) and "000000000000000000000000000000000" & get_sos_num_comp) or
		(repeat(41, sig_1240) and "00000000000000000000000000000000001000000") or
		(repeat(41, sig_1229) and chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3) or
		(repeat(41, sig_1217) and sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28) & sig_1628(28 downto 0) & "000") or
		(repeat(41, sig_1216) and p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth(31) & p_jinfo_mcuwidth) or
		(repeat(41, sig_1165) and buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p) or
		(repeat(41, sig_1118) and sig_1627(39 downto 0) & '0') or
		(repeat(41, sig_1089) and "00000000000000000000000000000000001000001") or
		(repeat(41, sig_1077) and "00000000000000000000000000000000100000000") or
		(repeat(41, sig_1049) and "00000000000000000000000000000000000000011") or
		(repeat(41, sig_1048) and p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu(31) & p_jinfo_nummcu) or
		(repeat(41, sig_1032) and read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position) or
		(repeat(41, sig_1015) and writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width(31) & writeoneblock_width) or
		(repeat(41, sig_1014) and sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28 downto 0) & writeoneblock_hoffs(2 downto 0)) or
		(repeat(41, sig_1010) and writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height(31) & writeoneblock_height) or
		(repeat(41, sig_1009) and sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28 downto 0) & writeoneblock_voffs(2 downto 0));

	-- Behaviour of component 'mux_291' model 'mux'
	mux_291 <=
		(repeat(41, sig_1468) and sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642) or
		(repeat(41, sig_1505) and huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j) or
		(repeat(41, sig_1502) and huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0) or
		(repeat(41, sig_1492) and buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p(31) & buf_getv_p) or
		(repeat(41, sig_1487) and read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position) or
		(repeat(41, sig_1420) and chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i) or
		(repeat(41, sig_1569) and decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code(31) & decodehuffman_ac_code) or
		(repeat(41, sig_1395) and chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2) or
		(repeat(41, sig_1393) and chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2) or
		(repeat(41, sig_1363) and chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1) or
		(repeat(41, sig_1355) and "00000000000000000000000000000000000001000") or
		(repeat(41, sig_1349) and sig_1628(38 downto 0) & "00") or
		(repeat(41, sig_1342) and decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s(31) & decodehuffmcu_s) or
		(repeat(41, sig_1336) and get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i) or
		(repeat(41, sig_1324) and sig_1666(39 downto 0) & '0') or
		(repeat(41, sig_1576) and decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code(31) & decodehuffman_dc_code) or
		(repeat(41, sig_1446) and decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i) or
		(repeat(41, sig_1455) and decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k) or
		(repeat(41, sig_1551) and huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l) or
		(repeat(41, sig_1534) and huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j) or
		(repeat(41, sig_1531) and huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0) or
		(repeat(41, sig_1518) and huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l) or
		(repeat(41, sig_1463) and chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0) or
		(repeat(41, sig_1435) and izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i) or
		(repeat(41, sig_1322) and get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length(31) & get_dqt_length) or
		(repeat(41, sig_1319) and read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word(15) & read_word) or
		(repeat(41, sig_1314) and get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length(31) & get_dht_length) or
		(repeat(41, sig_1309) and p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height) or
		(repeat(41, sig_1303) and get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i) or
		(repeat(41, sig_1299) and sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640(31) & sig_1640) or
		(repeat(41, sig_1291) and sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8) & sig_1610(8 downto 0)) or
		(repeat(41, sig_1287) and get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j(31) & get_sos_j) or
		(repeat(41, sig_1280) and chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3) or
		(repeat(41, sig_1245) and get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci) or
		(repeat(41, sig_1241) and get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i) or
		(repeat(41, sig_1229) and chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0) or
		(repeat(41, sig_1217) and yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y & "00000000") or
		(repeat(41, sig_1216) and sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647) or
		(repeat(41, sig_1209) and get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci) or
		(repeat(41, sig_1168) and buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n(31) & buf_getv_n) or
		(repeat(41, sig_1118) and sig_1628(37) & sig_1628(37 downto 0) & "00") or
		(repeat(41, sig_1078) and huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p) or
		(repeat(41, sig_1060) and yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r(31) & yuvtorgb_r) or
		(repeat(41, sig_1051) and decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i) or
		(repeat(41, sig_1048) and decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu) or
		(repeat(41, sig_1037) and huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p) or
		(repeat(41, sig_1025) and yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i) or
		(repeat(41, sig_1016) and writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e) or
		(repeat(41, sig_1011) and writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i) or
		(repeat(41, sig_1005) and write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs);

	-- Behaviour of component 'mux_292' model 'mux'
	mux_292 <=
		(repeat(32, sig_1294) and sig_1613) or
		(repeat(32, sig_1427) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_286' model 'mux'
	mux_286 <=
		(repeat(32, sig_1047) and sig_1610(31 downto 0)) or
		(repeat(32, sig_1052) and sig_1610(29 downto 0) & decode_start_currentmcu(1 downto 0));

	-- Behaviour of component 'mux_275' model 'mux'
	mux_275 <=
		(repeat(32, sig_1396) and chenidct_i) or
		(repeat(32, sig_1118) and chenidct_a1) or
		(repeat(32, sig_1217) and sig_1609(23) & sig_1609(23) & sig_1609(23) & sig_1609(23) & sig_1609(23) & sig_1609(23) & sig_1609(23) & sig_1609(23 downto 0) & sig_1666(6)) or
		(repeat(32, sig_1294) and decode_start_i) or
		(repeat(32, sig_1309) and sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16) & sig_1624(16 downto 3)) or
		(repeat(32, sig_1451) and sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31 downto 4)) or
		(repeat(32, sig_1463) and chenidct_c3);

	-- Behaviour of component 'mux_272' model 'mux'
	mux_272 <=
		(repeat(39, sig_1217) and yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y) or
		(repeat(39, sig_1118) and sig_1600(40 downto 2)) or
		(repeat(39, sig_1309) and sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16) & sig_1613(16 downto 0)) or
		(repeat(39, sig_1449) and chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i(31) & chenidct_i);

	-- Behaviour of component 'mux_274' model 'mux'
	mux_274 <=
		(repeat(32, sig_1397) and "00000000000000000000000000000001") or
		(repeat(32, sig_1118) and chenidct_a2) or
		(repeat(32, sig_1309) and "0000000000000000000000000000000" & and_864) or
		(repeat(32, sig_1451) and "0000000000000000000000000000000" & and_789) or
		(repeat(32, sig_1463) and chenidct_c2);

	-- Behaviour of component 'mux_271' model 'mux'
	mux_271 <=
		(repeat(39, sig_1118) and sig_1668(38 downto 0)) or
		(repeat(39, sig_1217) and sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31) & sig_1667(31 downto 8)) or
		(repeat(39, sig_1448) and "000000000000000000000000000000000000001");

	-- Behaviour of component 'mux_266' model 'mux'
	mux_266 <=
		(repeat(39, sig_1463) and chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0) or
		(repeat(39, sig_1574) and decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30) & decodehuffman_dc_code(30 downto 0)) or
		(repeat(39, sig_1567) and decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30) & decodehuffman_ac_code(30 downto 0)) or
		(repeat(39, sig_1563) and huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l(31) & huff_make_dhuff_tb_dc_l) or
		(repeat(39, sig_1561) and sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655(31) & sig_1655) or
		(repeat(39, sig_1556) and sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31 downto 7)) or
		(repeat(39, sig_1479) and get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i(31) & get_dht_i) or
		(repeat(39, sig_1548) and huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p(31) & huff_make_dhuff_tb_ac_p) or
		(repeat(39, sig_1536) and huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0(31) & huff_make_dhuff_tb_dc_i_c0) or
		(repeat(39, sig_1529) and huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p(31) & huff_make_dhuff_tb_dc_p) or
		(repeat(39, sig_1525) and huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l(31) & huff_make_dhuff_tb_ac_l) or
		(repeat(39, sig_1523) and sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651(31) & sig_1651) or
		(repeat(39, sig_1507) and huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0(31) & huff_make_dhuff_tb_ac_i_c0) or
		(repeat(39, sig_1493) and read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31) & read_position(31 downto 3)) or
		(repeat(39, sig_1412) and "0000000" & chenidct_aidx) or
		(repeat(39, sig_1477) and "0000000000" & chenidct_aidx(31 downto 3)) or
		(repeat(39, sig_1466) and decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k) or
		(repeat(39, sig_1464) and decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31) & decodehuffmcu_k(31 downto 4)) or
		(repeat(39, sig_1451) and sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31) & sig_1642(31 downto 3)) or
		(repeat(39, sig_1443) and decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i(31) & decodehuffmcu_i) or
		(repeat(39, sig_1441) and "0000000" & curhuffreadbuf_idx) or
		(repeat(39, sig_1582) and "0000000000" & chenidct_i(31 downto 3)) or
		(repeat(39, sig_1436) and izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i(31) & izigzagmatrix_i) or
		(repeat(39, sig_1427) and decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i(31) & decode_start_i) or
		(repeat(39, sig_1395) and chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1(31) & chenidct_a1) or
		(repeat(39, sig_1350) and "0000000" & chenidct_i(28 downto 0) & "001") or
		(repeat(39, sig_1335) and get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num(1) & get_dqt_num) or
		(repeat(39, sig_1309) and sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16) & sig_1614(16 downto 3)) or
		(repeat(39, sig_1297) and sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647) or
		(repeat(39, sig_1292) and sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653(8) & sig_1653) or
		(repeat(39, sig_1289) and sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649(8) & sig_1649) or
		(repeat(39, sig_1283) and huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size(31) & huff_make_dhuff_tb_ac_size) or
		(repeat(39, sig_1264) and chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2(31) & chenidct_b2) or
		(repeat(39, sig_1257) and sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643) or
		(repeat(39, sig_1254) and get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci(31) & get_sos_ci) or
		(repeat(39, sig_1251) and get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i(31) & get_sos_i) or
		(repeat(39, sig_1247) and "0000000" & readbuf_idx) or
		(repeat(39, sig_1234) and yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i(31) & yuvtorgb_i) or
		(repeat(39, sig_1217) and sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31) & sig_1624(31 downto 7)) or
		(repeat(39, sig_1211) and get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci(31) & get_sof_ci) or
		(repeat(39, sig_1163) and sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31) & sig_1647(31 downto 1)) or
		(repeat(39, sig_1118) and sig_1666(39 downto 1)) or
		(repeat(39, sig_1112) and chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1) or
		(repeat(39, sig_1110) and chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3(31) & chenidct_b3) or
		(repeat(39, sig_1073) and "0000000000000000000000000000000000000" & sig_1663) or
		(repeat(39, sig_1068) and huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size(31) & huff_make_dhuff_tb_dc_size) or
		(repeat(39, sig_1059) and "0000000" & jpeg2bmp_main_j) or
		(repeat(39, sig_1056) and "0000000" & jpeg2bmp_main_i) or
		(repeat(39, sig_1052) and decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31 downto 2)) or
		(repeat(39, sig_1047) and decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu(31) & decode_start_currentmcu) or
		(repeat(39, sig_1026) and decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff(31) & decodehuffmcu_diff) or
		(repeat(39, sig_1021) and write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31) & write4blocks_hoffs(31 downto 3)) or
		(repeat(39, sig_1018) and writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i(31) & writeoneblock_i) or
		(repeat(39, sig_1017) and writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e(31) & writeoneblock_e) or
		(repeat(39, sig_1014) and writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31) & writeoneblock_hoffs(31 downto 3)) or
		(repeat(39, sig_1012) and writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff(12) & writeoneblock_diff) or
		(repeat(39, sig_1009) and writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31) & writeoneblock_voffs(31 downto 3)) or
		(repeat(39, sig_1005) and write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31) & write4blocks_voffs(31 downto 3));

	-- Behaviour of component 'mux_265' model 'mux'
	mux_265 <=
		(repeat(39, sig_1112) and chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2(31) & chenidct_c2) or
		(repeat(39, sig_1110) and chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0(31) & chenidct_a0) or
		(repeat(39, sig_1027) and sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643(31) & sig_1643) or
		(repeat(39, sig_1012) and writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12) & writeoneblock_e(12 downto 0)) or
		(repeat(39, sig_1006) and "00000000000" & decodehuffmcu_n) or
		(repeat(39, sig_1309) and "00000000000000000000000000000000000000" & and_862) or
		(repeat(39, sig_1395) and chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2(31) & chenidct_a2) or
		(repeat(39, sig_1546) and "111111111111111111111111111111111111111") or
		(repeat(39, sig_1556) and "000000000000000000000000000000000000001") or
		(repeat(39, sig_1292) and decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8) & decodehuffman_dc_code(8 downto 0)) or
		(repeat(39, sig_1289) and decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8) & decodehuffman_ac_code(8 downto 0)) or
		(repeat(39, sig_1264) and chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1) or
		(repeat(39, sig_1118) and sig_1667(37) & sig_1667(37 downto 0)) or
		(repeat(39, sig_1463) and chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3) or
		(repeat(39, sig_1426) and "000000000000000000000000000000000000001");

	-- Behaviour of component 'mux_260' model 'mux'
	mux_260 <=
		(repeat(39, sig_1458) and "0000000000000000000000000000000" & read_byte) or
		(repeat(39, sig_1324) and sig_1627(38 downto 0)) or
		(repeat(39, sig_1395) and chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3(31) & chenidct_a3) or
		(repeat(39, sig_1544) and sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658(31) & sig_1658) or
		(repeat(39, sig_1451) and "00000000000000000000000000000000000000" & and_785) or
		(repeat(39, sig_1217) and sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30) & sig_1666(30 downto 7)) or
		(repeat(39, sig_1118) and chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1(31) & chenidct_b1) or
		(repeat(39, sig_1038) and sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660(31) & sig_1660) or
		(repeat(39, sig_1463) and chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1(31) & chenidct_c1) or
		(repeat(39, sig_1438) and "000000000000000000000000000000000000001");

	-- Behaviour of component 'mux_261' model 'mux'
	mux_261 <=
		(repeat(39, sig_1458) and get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count(31) & get_dht_count) or
		(repeat(39, sig_1324) and sig_1628(39 downto 1)) or
		(repeat(39, sig_1309) and sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16) & sig_1610(16 downto 0)) or
		(repeat(39, sig_1297) and sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648) or
		(repeat(39, sig_1262) and "0000000" & chenidct_aidx) or
		(repeat(39, sig_1217) and yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y(23) & yuvtorgb_y) or
		(repeat(39, sig_1546) and sig_1610(31) & sig_1610(31) & sig_1610(31) & sig_1610(31) & sig_1610(31) & sig_1610(31) & sig_1610(31) & sig_1610(31 downto 0)) or
		(repeat(39, sig_1163) and sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31) & sig_1648(31 downto 1)) or
		(repeat(39, sig_1111) and "0000000000" & chenidct_aidx(31 downto 3)) or
		(repeat(39, sig_1035) and huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code(31) & huff_make_dhuff_tb_dc_code) or
		(repeat(39, sig_1023) and huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code(31) & huff_make_dhuff_tb_ac_code) or
		(repeat(39, sig_1012) and "0000000" & writeoneblock_inidx) or
		(repeat(39, sig_1567) and decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l(31) & decodehuffman_ac_l) or
		(repeat(39, sig_1574) and decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l(31) & decodehuffman_dc_l) or
		(repeat(39, sig_1451) and sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28) & sig_1610(28 downto 1)) or
		(repeat(39, sig_1527) and huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j(31) & huff_make_dhuff_tb_dc_j) or
		(repeat(39, sig_1499) and huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j(31) & huff_make_dhuff_tb_ac_j) or
		(repeat(39, sig_1398) and chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0(31) & chenidct_b0) or
		(repeat(39, sig_1335) and get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i(31) & get_dqt_i) or
		(repeat(39, sig_1463) and chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0(31) & chenidct_c0) or
		(repeat(39, sig_1436) and izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx(31) & izigzagmatrix_out_idx);

	-- Behaviour of component 'mux_262' model 'mux'
	mux_262 <=
		(repeat(32, sig_1056) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_257' model 'mux'
	mux_257 <=
		(repeat(32, sig_1059) and sig_1610(31 downto 0));

	-- Behaviour of component 'nand_786' model 'nand'
	nand_786 <= not (
		sig_1605 and
		sig_1606
	);

	-- Behaviour of component 'or_845' model 'or'
	or_845 <=
		sig_1629 or
		buf_getv_rv;

	-- Behaviour of component 'or_854' model 'or'
	or_854 <=
		sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638 or
		buf_getv;

	-- Behaviour of component 'or_866' model 'or'
	or_866 <=
		sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638(20) & sig_1638 or
		sig_1643;

	-- Behaviour of component 'and_785' model 'and'
	and_785 <=
		nand_786 and
		sig_1610(28);

	-- Behaviour of component 'and_801' model 'and'
	and_801 <=
		sig_1636 and
		current_read_byte;

	-- Behaviour of component 'mux_761' model 'mux'
	mux_761 <=
		(repeat(9, sig_1118) and "000011001") or
		(repeat(9, sig_1217) and "101100111") or
		(repeat(9, sig_1324) and "001000111");

	-- Behaviour of component 'mux_782' model 'mux'
	mux_782 <=
		(repeat(32, sig_1607) and sig_1660) or
		(repeat(32, sig_1608) and "000000000000000000000000" & read_byte);

	-- Behaviour of component 'or_802' model 'or'
	or_802 <=
		current_read_byte(23 downto 0) or
		"000000000000000000000000";

	-- Behaviour of component 'and_803' model 'and'
	and_803 <=
		sig_1637 and
		current_read_byte;

	-- Behaviour of component 'mux_822' model 'mux'
	mux_822 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_823' model 'mux'
	mux_823 <=
		(repeat(32, sig_1617) and mux_824);

	-- Behaviour of component 'mux_776' model 'mux'
	mux_776 <=
		(repeat(32, sig_1617) and mux_777);

	-- Behaviour of component 'mux_820' model 'mux'
	mux_820 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_824' model 'mux'
	mux_824 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_825' model 'mux'
	mux_825 <=
		(repeat(32, sig_1617) and mux_826);

	-- Behaviour of component 'mux_760' model 'mux'
	mux_760 <=
		(repeat(32, sig_1118) and chenidct_a0) or
		(repeat(32, sig_1217) and yuvtorgb_u(30) & yuvtorgb_u) or
		(repeat(32, sig_1324) and chenidct_a2);

	-- Behaviour of component 'and_789' model 'and'
	and_789 <=
		sig_1604 and
		sig_1624(31);

	-- Behaviour of component 'mux_759' model 'mux'
	mux_759 <=
		(repeat(6, sig_1118) and "111011") or
		(repeat(6, sig_1217) and "100011") or
		(repeat(6, sig_1324) and "010101");

	-- Behaviour of component 'mux_768' model 'mux'
	mux_768 <=
		(repeat(32, sig_1436) and sig_1610(31 downto 0));

	-- Behaviour of component 'mux_757' model 'mux'
	mux_757 <=
		(repeat(8, sig_1057) and sig_1646) or
		(repeat(8, sig_1062) and outdata_image_height);

	-- Behaviour of component 'mux_773' model 'mux'
	mux_773 <=
		(repeat(8, sig_1179) and outdata_image_height) or
		(repeat(8, sig_1180) and outdata_image_width) or
		(repeat(8, sig_1181) and write8_u8);

	-- Behaviour of component 'mux_762' model 'mux'
	mux_762 <=
		(repeat(32, sig_1118) and chenidct_a3) or
		(repeat(32, sig_1217) and yuvtorgb_v) or
		(repeat(32, sig_1324) and chenidct_a1);

	-- Behaviour of component 'mux_766' model 'mux'
	mux_766 <=
		(repeat(32, sig_1436) and sig_1609(31 downto 0));

	-- Behaviour of component 'mux_781' model 'mux'
	mux_781 <=
		(repeat(32, sig_1608) and sig_1657) or
		(repeat(32, sig_1607) and "000000000000000000000000" & read_byte);

	-- Behaviour of component 'mux_797' model 'mux'
	mux_797 <=
		(repeat(32, sig_1617) and mux_798);

	-- Behaviour of component 'mux_821' model 'mux'
	mux_821 <=
		(repeat(32, sig_1617) and mux_822);

	-- Behaviour of component 'mux_826' model 'mux'
	mux_826 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_778' model 'mux'
	mux_778 <=
		(repeat(32, sig_1607) and sig_1659) or
		(repeat(32, sig_1608) and "000000000000000000000000" & read_byte);

	-- Behaviour of component 'mux_827' model 'mux'
	mux_827 <=
		(repeat(32, sig_1617) and mux_828);

	-- Behaviour of component 'mux_815' model 'mux'
	mux_815 <=
		(repeat(32, sig_1617) and mux_816);

	-- Behaviour of component 'mux_798' model 'mux'
	mux_798 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_816' model 'mux'
	mux_816 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_817' model 'mux'
	mux_817 <=
		(repeat(32, sig_1617) and mux_818);

	-- Behaviour of component 'mux_777' model 'mux'
	mux_777 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_819' model 'mux'
	mux_819 <=
		(repeat(32, sig_1617) and mux_820);

	-- Behaviour of component 'mux_783' model 'mux'
	mux_783 <=
		(repeat(32, sig_1608) and sig_1658) or
		(repeat(32, sig_1607) and "000000000000000000000000" & read_byte);

	-- Behaviour of component 'mux_795' model 'mux'
	mux_795 <=
		(repeat(32, sig_1617) and mux_796);

	-- Behaviour of component 'mux_796' model 'mux'
	mux_796 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_805' model 'mux'
	mux_805 <=
		(repeat(32, sig_1617) and mux_806);

	-- Behaviour of component 'mux_806' model 'mux'
	mux_806 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_807' model 'mux'
	mux_807 <=
		(repeat(32, sig_1617) and mux_808);

	-- Behaviour of component 'mux_808' model 'mux'
	mux_808 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_809' model 'mux'
	mux_809 <=
		(repeat(32, sig_1617) and mux_810);

	-- Behaviour of component 'mux_810' model 'mux'
	mux_810 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_811' model 'mux'
	mux_811 <=
		(repeat(32, sig_1617) and mux_812);

	-- Behaviour of component 'mux_812' model 'mux'
	mux_812 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_813' model 'mux'
	mux_813 <=
		(repeat(32, sig_1617) and mux_814);

	-- Behaviour of component 'mux_814' model 'mux'
	mux_814 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_818' model 'mux'
	mux_818 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_828' model 'mux'
	mux_828 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_829' model 'mux'
	mux_829 <=
		(repeat(32, sig_1617) and mux_830);

	-- Behaviour of component 'mux_830' model 'mux'
	mux_830 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_831' model 'mux'
	mux_831 <=
		(repeat(32, sig_1617) and mux_832);

	-- Behaviour of component 'mux_832' model 'mux'
	mux_832 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_836' model 'mux'
	mux_836 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_837' model 'mux'
	mux_837 <=
		(repeat(32, sig_1617) and mux_838);

	-- Behaviour of component 'mux_839' model 'mux'
	mux_839 <=
		(repeat(32, sig_1617) and mux_840);

	-- Behaviour of component 'mux_840' model 'mux'
	mux_840 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_841' model 'mux'
	mux_841 <=
		(repeat(32, sig_1617) and mux_842);

	-- Behaviour of component 'mux_842' model 'mux'
	mux_842 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_843' model 'mux'
	mux_843 <=
		(repeat(32, sig_1617) and mux_844);

	-- Behaviour of component 'mux_856' model 'mux'
	mux_856 <=
		(repeat(32, sig_1617) and mux_857);

	-- Behaviour of component 'and_864' model 'and'
	and_864 <=
		sig_1603 and
		sig_1624(16);

	-- Behaviour of component 'mux_870' model 'mux'
	mux_870 <=
		(repeat(32, sig_1599) and get_dqt_length) or
		(repeat(32, sig_1669) and sig_1614(31 downto 0));

	-- Behaviour of component 'mux_872' model 'mux'
	mux_872 <=
		(repeat(2, sig_1598) and "10");

	-- Behaviour of component 'mux_875' model 'mux'
	mux_875 <=
		(repeat(32, sig_1616) and sig_1647);

	-- Behaviour of component 'mux_891' model 'mux'
	mux_891 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_892' model 'mux'
	mux_892 <=
		(repeat(32, sig_1617) and mux_893);

	-- Behaviour of component 'mux_893' model 'mux'
	mux_893 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_894' model 'mux'
	mux_894 <=
		(repeat(32, sig_1617) and mux_895);

	-- Behaviour of component 'mux_895' model 'mux'
	mux_895 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_896' model 'mux'
	mux_896 <=
		(repeat(32, sig_1623) and sig_1648) or
		(repeat(32, sig_1616) and sig_1624(31 downto 0));

	-- Behaviour of component 'mux_897' model 'mux'
	mux_897 <=
		(repeat(32, sig_1616) and sig_1647);

	-- Behaviour of component 'mux_898' model 'mux'
	mux_898 <=
		(repeat(32, sig_1617) and mux_899);

	-- Behaviour of component 'mux_899' model 'mux'
	mux_899 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_900' model 'mux'
	mux_900 <=
		(repeat(32, sig_1617) and mux_901);

	-- Behaviour of component 'mux_901' model 'mux'
	mux_901 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_902' model 'mux'
	mux_902 <=
		(repeat(32, sig_1617) and mux_903);

	-- Behaviour of component 'mux_903' model 'mux'
	mux_903 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_904' model 'mux'
	mux_904 <=
		(repeat(32, sig_1617) and mux_905);

	-- Behaviour of component 'mux_905' model 'mux'
	mux_905 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_906' model 'mux'
	mux_906 <=
		(repeat(32, sig_1617) and mux_907);

	-- Behaviour of component 'mux_907' model 'mux'
	mux_907 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_908' model 'mux'
	mux_908 <=
		(repeat(32, sig_1617) and mux_909);

	-- Behaviour of component 'mux_917' model 'mux'
	mux_917 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_918' model 'mux'
	mux_918 <=
		(repeat(32, sig_1617) and mux_919);

	-- Behaviour of component 'mux_924' model 'mux'
	mux_924 <=
		(repeat(32, sig_1617) and mux_925);

	-- Behaviour of component 'mux_925' model 'mux'
	mux_925 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_928' model 'mux'
	mux_928 <=
		(repeat(32, sig_1617) and mux_929);

	-- Behaviour of component 'mux_929' model 'mux'
	mux_929 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_931' model 'mux'
	mux_931 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_932' model 'mux'
	mux_932 <=
		(repeat(32, sig_1617) and mux_933);

	-- Behaviour of component 'mux_934' model 'mux'
	mux_934 <=
		(repeat(32, sig_1617) and mux_935);

	-- Behaviour of component 'mux_935' model 'mux'
	mux_935 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_936' model 'mux'
	mux_936 <=
		(repeat(32, sig_1617) and mux_937);

	-- Behaviour of component 'mux_937' model 'mux'
	mux_937 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_938' model 'mux'
	mux_938 <=
		(repeat(32, sig_1617) and mux_939);

	-- Behaviour of component 'mux_939' model 'mux'
	mux_939 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_941' model 'mux'
	mux_941 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_944' model 'mux'
	mux_944 <=
		(repeat(32, sig_1617) and mux_945);

	-- Behaviour of component 'mux_945' model 'mux'
	mux_945 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_946' model 'mux'
	mux_946 <=
		(repeat(32, sig_1617) and mux_947);

	-- Behaviour of component 'mux_833' model 'mux'
	mux_833 <=
		(repeat(32, sig_1617) and mux_834);

	-- Behaviour of component 'mux_834' model 'mux'
	mux_834 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_835' model 'mux'
	mux_835 <=
		(repeat(32, sig_1617) and mux_836);

	-- Behaviour of component 'mux_838' model 'mux'
	mux_838 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_844' model 'mux'
	mux_844 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_857' model 'mux'
	mux_857 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_858' model 'mux'
	mux_858 <=
		(repeat(32, sig_1617) and mux_859);

	-- Behaviour of component 'mux_859' model 'mux'
	mux_859 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_874' model 'mux'
	mux_874 <=
		(repeat(32, sig_1623) and sig_1648) or
		(repeat(32, sig_1616) and sig_1624(31 downto 0));

	-- Behaviour of component 'mux_888' model 'mux'
	mux_888 <=
		(repeat(32, sig_1617) and mux_889);

	-- Behaviour of component 'mux_889' model 'mux'
	mux_889 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_913' model 'mux'
	mux_913 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_914' model 'mux'
	mux_914 <=
		(repeat(32, sig_1617) and mux_915);

	-- Behaviour of component 'mux_915' model 'mux'
	mux_915 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_916' model 'mux'
	mux_916 <=
		(repeat(32, sig_1617) and mux_917);

	-- Behaviour of component 'mux_933' model 'mux'
	mux_933 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_940' model 'mux'
	mux_940 <=
		(repeat(32, sig_1617) and mux_941);

	-- Behaviour of component 'mux_942' model 'mux'
	mux_942 <=
		(repeat(32, sig_1617) and mux_943);

	-- Behaviour of component 'and_867' model 'and'
	and_867 <=
		sig_1670 and
		sig_1594;

	-- Behaviour of component 'mux_909' model 'mux'
	mux_909 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_910' model 'mux'
	mux_910 <=
		(repeat(32, sig_1617) and mux_911);

	-- Behaviour of component 'mux_911' model 'mux'
	mux_911 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_920' model 'mux'
	mux_920 <=
		(repeat(32, sig_1617) and mux_921);

	-- Behaviour of component 'mux_921' model 'mux'
	mux_921 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_926' model 'mux'
	mux_926 <=
		(repeat(32, sig_1617) and mux_927);

	-- Behaviour of component 'mux_927' model 'mux'
	mux_927 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_943' model 'mux'
	mux_943 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_886' model 'mux'
	mux_886 <=
		(sig_1601 and read_byte(0)) or
		(sig_1602 and read_byte(0));

	-- Behaviour of component 'mux_922' model 'mux'
	mux_922 <=
		(repeat(32, sig_1617) and mux_923);

	-- Behaviour of component 'mux_923' model 'mux'
	mux_923 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_930' model 'mux'
	mux_930 <=
		(repeat(32, sig_1617) and mux_931);

	-- Behaviour of component 'mux_987' model 'mux'
	mux_987 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'and_860' model 'and'
	and_860 <=
		sig_1637 and
		buf_getv;

	-- Behaviour of component 'and_881' model 'and'
	and_881 <=
		sig_1637 and
		sig_1643;

	-- Behaviour of component 'and_884' model 'and'
	and_884 <=
		"00000000000000000000000000010000" and
		"000000000000000000000000" & read_byte;

	-- Behaviour of component 'mux_890' model 'mux'
	mux_890 <=
		(repeat(32, sig_1617) and mux_891);

	-- Behaviour of component 'mux_912' model 'mux'
	mux_912 <=
		(repeat(32, sig_1617) and mux_913);

	-- Behaviour of component 'mux_919' model 'mux'
	mux_919 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_948' model 'mux'
	mux_948 <=
		(repeat(32, sig_1617) and mux_949);

	-- Behaviour of component 'mux_949' model 'mux'
	mux_949 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_950' model 'mux'
	mux_950 <=
		(repeat(32, sig_1617) and mux_951);

	-- Behaviour of component 'and_862' model 'and'
	and_862 <=
		sig_1595 and
		sig_1614(16);

	-- Behaviour of component 'mux_953' model 'mux'
	mux_953 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_954' model 'mux'
	mux_954 <=
		(repeat(32, sig_1617) and mux_955);

	-- Behaviour of component 'mux_955' model 'mux'
	mux_955 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_951' model 'mux'
	mux_951 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_952' model 'mux'
	mux_952 <=
		(repeat(32, sig_1617) and mux_953);

	-- Behaviour of component 'mux_959' model 'mux'
	mux_959 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_960' model 'mux'
	mux_960 <=
		(repeat(32, sig_1617) and mux_961);

	-- Behaviour of component 'mux_961' model 'mux'
	mux_961 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_965' model 'mux'
	mux_965 <=
		(repeat(32, sig_1617) and mux_966);

	-- Behaviour of component 'mux_966' model 'mux'
	mux_966 <=
		(repeat(32, sig_1622) and yuvtorgb_r) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'and_876' model 'and'
	and_876 <=
		"00001111" and
		"0000" & read_byte(7 downto 4);

	-- Behaviour of component 'mux_956' model 'mux'
	mux_956 <=
		(repeat(32, sig_1617) and mux_957);

	-- Behaviour of component 'mux_957' model 'mux'
	mux_957 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_947' model 'mux'
	mux_947 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_968' model 'mux'
	mux_968 <=
		(repeat(32, sig_1593) and yuvtorgb_b) or
		(repeat(32, sig_1591) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_969' model 'mux'
	mux_969 <=
		(repeat(32, sig_1597) and mux_970);

	-- Behaviour of component 'mux_970' model 'mux'
	mux_970 <=
		(repeat(32, sig_1586) and yuvtorgb_g) or
		(repeat(32, sig_1589) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_980' model 'mux'
	mux_980 <=
		(repeat(32, sig_1617) and mux_981);

	-- Behaviour of component 'mux_981' model 'mux'
	mux_981 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of component 'mux_958' model 'mux'
	mux_958 <=
		(repeat(32, sig_1617) and mux_959);

	-- Behaviour of component 'and_963' model 'and'
	and_963 <=
		sig_1615 and
		sig_991;

	-- Behaviour of component 'mux_986' model 'mux'
	mux_986 <=
		(repeat(32, sig_1617) and mux_987);

	-- Behaviour of component 'mux_988' model 'mux'
	mux_988 <=
		(repeat(32, sig_1617) and mux_989);

	-- Behaviour of component 'mux_989' model 'mux'
	mux_989 <=
		(repeat(32, sig_1622) and sig_1642) or
		(repeat(32, sig_1625) and "00000000000000000000000011111111");

	-- Behaviour of all components of model 'reg'
	-- Registers with clock = sig_clock and no reset
	process(sig_clock)
	begin
		if rising_edge(sig_clock) then
			if sig_1437 = '1' then
				izigzagmatrix_i <= mux_768;
			end if;
			if sig_1437 = '1' then
				izigzagmatrix_out_idx <= mux_766;
			end if;
			if sig_1072 = '1' then
				iquantize_qidx <= sig_1610(1 downto 0);
			end if;
			if sig_1061 = '1' then
				write8_u8 <= mux_757;
			end if;
			if sig_1206 = '1' then
				p_jinfo_image_height <= read_word;
			end if;
			if sig_1207 = '1' then
				p_jinfo_image_width <= read_word;
			end if;
			if sig_1204 = '1' then
				p_jinfo_num_components <= read_byte;
			end if;
			if sig_1219 = '1' then
				p_jinfo_smp_fact <= mux_872;
			end if;
			if sig_1307 = '1' then
				p_jinfo_mcuwidth <= sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17) & sig_1612(17 downto 0);
			end if;
			if sig_1307 = '1' then
				p_jinfo_mcuheight <= sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17) & sig_1609(17 downto 0);
			end if;
			if sig_1100 = '1' then
				p_jinfo_nummcu <= sig_1628(31 downto 0);
			end if;
			if sig_1273 = '1' then
				i_jinfo_jpeg_data <= readbuf_idx;
			end if;
			if sig_1583 = '1' then
				curhuffreadbuf_idx <= mux_671;
			end if;
			if sig_1042 = '1' then
				outdata_image_width <= p_jinfo_image_width(7 downto 0);
			end if;
			if sig_1042 = '1' then
				outdata_image_height <= p_jinfo_image_height(7 downto 0);
			end if;
			if sig_1340 = '1' then
				readbuf_idx <= mux_648;
			end if;
			if sig_1053 = '1' then
				read_byte <= sig_1644;
			end if;
			if sig_1249 = '1' then
				read_word <= read_word_c & sig_1644;
			end if;
			if sig_1248 = '1' then
				read_word_c <= sig_1644;
			end if;
			if sig_1205 = '1' then
				next_marker <= next_marker_c;
			end if;
			if sig_1203 = '1' then
				next_marker_c <= read_byte;
			end if;
			if sig_1210 = '1' then
				get_sof_ci <= mux_633;
			end if;
			if sig_1208 = '1' then
				get_sof_i_comp_info_id <= get_sof_ci(1 downto 0);
			end if;
			if sig_1208 = '1' then
				get_sof_i_comp_info_h_samp_factor <= get_sof_ci(1 downto 0);
			end if;
			if sig_1208 = '1' then
				get_sof_i_comp_info_quant_tbl_no <= get_sof_ci(1 downto 0);
			end if;
			if sig_1224 = '1' then
				get_sos_num_comp <= read_byte;
			end if;
			if sig_1250 = '1' then
				get_sos_i <= mux_622;
			end if;
			if sig_1272 = '1' then
				get_sos_c <= read_byte(4);
			end if;
			if sig_1242 = '1' then
				get_sos_cc <= read_byte;
			end if;
			if sig_1253 = '1' then
				get_sos_ci <= mux_616;
			end if;
			if sig_1286 = '1' then
				get_sos_j <= mux_614;
			end if;
			if sig_1243 = '1' then
				get_sos_i_comp_info_dc_tbl_no <= get_sos_ci(1 downto 0);
			end if;
			if sig_1312 = '1' then
				get_dht_length <= sig_1614(31 downto 0);
			end if;
			if sig_1199 = '1' then
				get_dht_index <= mux_886;
			end if;
			if sig_1541 = '1' then
				get_dht_i <= mux_602;
			end if;
			if sig_1456 = '1' then
				get_dht_count <= mux_600;
			end if;
			if sig_1199 = '1' then
				get_dht_is_ac <= sig_1602;
			end if;
			if sig_1316 = '1' then
				get_dqt_length <= mux_593;
			end if;
			if sig_1302 = '1' then
				get_dqt_prec <= read_byte(7 downto 4);
			end if;
			if sig_1302 = '1' then
				get_dqt_num <= read_byte(1 downto 0);
			end if;
			if sig_1333 = '1' then
				get_dqt_i <= mux_587;
			end if;
			if sig_1338 = '1' then
				get_dqt_tmp <= mux_585;
			end if;
			if sig_1347 = '1' then
				read_markers_unread_marker <= mux_580;
			end if;
			if sig_1352 = '1' then
				read_markers_sow_soi <= sig_1353;
			end if;
			if sig_1447 = '1' then
				chenidct_i <= mux_555;
			end if;
			if sig_1476 = '1' then
				chenidct_aidx <= mux_553;
			end if;
			if sig_1462 = '1' then
				chenidct_a0 <= mux_551;
			end if;
			if sig_1439 = '1' then
				chenidct_a1 <= mux_549;
			end if;
			if sig_1584 = '1' then
				chenidct_a2 <= mux_547;
			end if;
			if sig_1465 = '1' then
				chenidct_a3 <= mux_545;
			end if;
			if sig_1459 = '1' then
				chenidct_b0 <= mux_543;
			end if;
			if sig_1356 = '1' then
				chenidct_b1 <= mux_541;
			end if;
			if sig_1478 = '1' then
				chenidct_b2 <= mux_539;
			end if;
			if sig_1460 = '1' then
				chenidct_b3 <= mux_537;
			end if;
			if sig_1461 = '1' then
				chenidct_c0 <= mux_535;
			end if;
			if sig_1394 = '1' then
				chenidct_c1 <= mux_533;
			end if;
			if sig_1394 = '1' then
				chenidct_c2 <= mux_531;
			end if;
			if sig_1461 = '1' then
				chenidct_c3 <= mux_529;
			end if;
			if sig_1494 = '1' then
				current_read_byte <= mux_521;
			end if;
			if sig_1424 = '1' then
				pgetc <= mux_517;
			end if;
			if sig_1442 = '1' then
				pgetc_temp <= sig_1644;
			end if;
			if sig_1483 = '1' then
				buf_getb <= sig_1481;
			end if;
			if sig_1495 = '1' then
				buf_getv <= mux_507;
			end if;
			if sig_1200 = '1' then
				buf_getv_n <= mux_505;
			end if;
			if sig_1490 = '1' then
				buf_getv_p <= sig_1614(31 downto 0);
			end if;
			if sig_1488 = '1' then
				buf_getv_rv <= mux_501;
			end if;
			if sig_1521 = '1' then
				huff_make_dhuff_tb_ac <= huff_make_dhuff_tb_ac_p_dhtbl_ml;
			end if;
			if sig_1187 = '1' then
				huff_make_dhuff_tb_ac_tbl_no <= sig_1184;
			end if;
			if sig_1543 = '1' then
				huff_make_dhuff_tb_ac_p_dhtbl_ml <= mux_492;
			end if;
			if sig_1506 = '1' then
				huff_make_dhuff_tb_ac_i_c0 <= mux_490;
			end if;
			if sig_1503 = '1' then
				huff_make_dhuff_tb_ac_j <= mux_488;
			end if;
			if sig_1545 = '1' then
				huff_make_dhuff_tb_ac_p <= mux_486;
			end if;
			if sig_1282 = '1' then
				huff_make_dhuff_tb_ac_code <= mux_484;
			end if;
			if sig_1557 = '1' then
				huff_make_dhuff_tb_ac_size <= mux_482;
			end if;
			if sig_1524 = '1' then
				huff_make_dhuff_tb_ac_l <= mux_480;
			end if;
			if sig_1559 = '1' then
				huff_make_dhuff_tb_dc <= huff_make_dhuff_tb_dc_p_dhtbl_ml;
			end if;
			if sig_1306 = '1' then
				huff_make_dhuff_tb_dc_tbl_no <= sig_1189;
			end if;
			if sig_1539 = '1' then
				huff_make_dhuff_tb_dc_p_dhtbl_ml <= mux_459;
			end if;
			if sig_1535 = '1' then
				huff_make_dhuff_tb_dc_i_c0 <= mux_457;
			end if;
			if sig_1532 = '1' then
				huff_make_dhuff_tb_dc_j <= mux_455;
			end if;
			if sig_1538 = '1' then
				huff_make_dhuff_tb_dc_p <= mux_453;
			end if;
			if sig_1067 = '1' then
				huff_make_dhuff_tb_dc_code <= mux_451;
			end if;
			if sig_1069 = '1' then
				huff_make_dhuff_tb_dc_size <= mux_449;
			end if;
			if sig_1562 = '1' then
				huff_make_dhuff_tb_dc_l <= mux_447;
			end if;
			if sig_1572 = '1' then
				decodehuffman_ac <= mux_430;
			end if;
			if sig_1452 = '1' then
				decodehuffman_ac_tbl_no <= decodehuffmcu_tbl_no;
			end if;
			if sig_1452 = '1' then
				decodehuffman_ac_dhuff_ml <= sig_1652(5 downto 0);
			end if;
			if sig_1566 = '1' then
				decodehuffman_ac_code <= mux_424;
			end if;
			if sig_1566 = '1' then
				decodehuffman_ac_l <= mux_422;
			end if;
			if sig_1288 = '1' then
				decodehuffman_ac_p <= sig_1614(8 downto 0);
			end if;
			if sig_1580 = '1' then
				decodehuffman_dc <= mux_416;
			end if;
			if sig_1509 = '1' then
				decodehuffman_dc_tbl_no <= sig_1662;
			end if;
			if sig_1169 = '1' then
				decodehuffman_dc_dhuff_ml <= sig_1656(5 downto 0);
			end if;
			if sig_1573 = '1' then
				decodehuffman_dc_code <= mux_410;
			end if;
			if sig_1573 = '1' then
				decodehuffman_dc_l <= mux_408;
			end if;
			if sig_1290 = '1' then
				decodehuffman_dc_p <= sig_1614(8 downto 0);
			end if;
			if sig_1509 = '1' then
				decodehuffmcu_bufdim1 <= decode_block_in_buf_idx;
			end if;
			if sig_1578 = '1' then
				decodehuffmcu_s <= mux_400;
			end if;
			if sig_1343 = '1' then
				decodehuffmcu_diff <= mux_398;
			end if;
			if sig_1509 = '1' then
				decodehuffmcu_tbl_no <= sig_1662;
			end if;
			if sig_1444 = '1' then
				decodehuffmcu_i <= mux_394;
			end if;
			if sig_1467 = '1' then
				decodehuffmcu_k <= mux_392;
			end if;
			if sig_1029 = '1' then
				decodehuffmcu_n <= and_983;
			end if;
			if sig_1195 = '1' then
				writeoneblock_outidx <= mux_375;
			end if;
			if sig_1195 = '1' then
				writeoneblock_indim1 <= mux_373;
			end if;
			if sig_1195 = '1' then
				writeoneblock_width <= p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width(15) & p_jinfo_image_width;
			end if;
			if sig_1195 = '1' then
				writeoneblock_height <= p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height(15) & p_jinfo_image_height;
			end if;
			if sig_1195 = '1' then
				writeoneblock_voffs <= mux_367;
			end if;
			if sig_1195 = '1' then
				writeoneblock_hoffs <= mux_365;
			end if;
			if sig_1064 = '1' then
				writeoneblock_i <= mux_363;
			end if;
			if sig_1080 = '1' then
				writeoneblock_e <= mux_361;
			end if;
			if sig_1066 = '1' then
				writeoneblock_inidx <= mux_359;
			end if;
			if sig_1082 = '1' then
				writeoneblock_diff <= sig_1628(12 downto 0);
			end if;
			if sig_996 = '1' then
				writeblock_i <= decode_start_i(1 downto 0);
			end if;
			if sig_1191 = '1' then
				write4blocks_i <= decode_start_i(1 downto 0);
			end if;
			if sig_1193 = '1' then
				write4blocks_voffs <= mux_347;
			end if;
			if sig_1192 = '1' then
				write4blocks_hoffs <= mux_345;
			end if;
			if sig_998 = '1' then
				yuvtorgb_p <= mux_343;
			end if;
			if sig_998 = '1' then
				yuvtorgb_yidx <= mux_341;
			end if;
			if sig_998 = '1' then
				yuvtorgb_uidx <= mux_339;
			end if;
			if sig_998 = '1' then
				yuvtorgb_vidx <= mux_337;
			end if;
			if sig_1218 = '1' then
				yuvtorgb_r <= mux_335;
			end if;
			if sig_1218 = '1' then
				yuvtorgb_g <= mux_333;
			end if;
			if sig_1218 = '1' then
				yuvtorgb_b <= mux_331;
			end if;
			if sig_1298 = '1' then
				yuvtorgb_y <= sig_1642(23 downto 0);
			end if;
			if sig_1298 = '1' then
				yuvtorgb_u <= sig_1624(30 downto 0);
			end if;
			if sig_1298 = '1' then
				yuvtorgb_v <= sig_1614(31 downto 0);
			end if;
			if sig_1233 = '1' then
				yuvtorgb_i <= mux_320;
			end if;
			if sig_1044 = '1' then
				decode_block_comp_no <= mux_317;
			end if;
			if sig_1044 = '1' then
				decode_block_out_buf_idx <= mux_308;
			end if;
			if sig_1044 = '1' then
				decode_block_in_buf_idx <= mux_294;
			end if;
			if sig_1430 = '1' then
				decode_start_i <= mux_292;
			end if;
			if sig_1182 = '1' then
				decode_start_currentmcu <= mux_286;
			end if;
			if sig_1055 = '1' then
				jpeg2bmp_main_i <= mux_262;
			end if;
			if sig_1058 = '1' then
				jpeg2bmp_main_j <= mux_257;
			end if;
			if sig_1178 = '1' then
				read8_ret0_195 <= stdin_data;
			end if;
		end if;
	end process;
	-- Registers with clock = sig_clock and reset = sig_reset active '1'
	process(sig_clock, sig_reset)
	begin
		if sig_reset = '1' then
			read_position <= "11111111111111111111111111111111";
		else
			if rising_edge(sig_clock) then
				if sig_1496 = '1' then
					read_position <= mux_519;
				end if;
			end if;
		end if;
	end process;

	-- Remaining signal assignments
	-- Those who are not assigned by component instantiation

	sig_clock <= clock;
	sig_reset <= reset;
	augh_test_159 <= sig_1615;
	augh_test_26 <= sig_1616;
	augh_test_49 <= sig_1616;
	augh_test_52 <= sig_1616;
	augh_test_53 <= and_867;
	augh_test_67 <= sig_1618;
	augh_test_72 <= sig_1615;
	augh_test_77 <= sig_1616;
	augh_test_83 <= sig_1618;
	augh_test_89 <= sig_1615;
	augh_test_90 <= sig_1669;
	augh_test_105 <= sig_1615;
	augh_test_106 <= sig_1615;
	augh_test_107 <= sig_1615;
	augh_test_111 <= sig_1616;
	augh_test_114 <= sig_1618;
	augh_test_115 <= sig_1618;
	augh_test_119 <= sig_1615;
	augh_test_120 <= sig_1615;
	augh_test_122 <= and_963;
	augh_test_125 <= sig_1615;
	augh_test_127 <= sig_1615;
	augh_test_128 <= sig_1615;
	augh_test_130 <= and_976;
	augh_test_133 <= sig_1615;
	augh_test_136 <= sig_1618;
	augh_test_138 <= sig_1616;
	augh_test_142 <= sig_1618;
	augh_test_144 <= sig_1616;
	augh_test_151 <= sig_1615;
	augh_test_152 <= sig_1615;
	augh_test_155 <= sig_1618;
	augh_test_165 <= sig_1616;
	augh_test_166 <= sig_1616;
	augh_test_167 <= sig_1616;
	augh_test_168 <= sig_1616;
	sig_start <= start;
	augh_test_171 <= sig_1615;
	augh_test_178 <= sig_1615;
	augh_test_179 <= sig_1615;
	augh_test_182 <= sig_1616;
	augh_test_183 <= sig_1615;
	augh_test_184 <= sig_1615;
	augh_test_186 <= sig_1616;
	augh_test_187 <= sig_1615;
	augh_test_188 <= sig_1615;
	augh_test_189 <= sig_1615;
	sig_1671 <= "000000000000000000000000" & pgetc_temp;
	sig_1672 <= "000000000000000000000000000000" & p_jinfo_smp_fact;
	sig_1673 <= yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g;
	sig_1674 <= yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g;
	sig_1675 <= yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b;
	sig_1676 <= yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b;
	sig_1677 <= "000000000000000000000000" & next_marker_c;
	sig_1678 <= "000000000000000000000000" & pgetc_temp;
	sig_1679 <= yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g(31) & yuvtorgb_g;
	sig_1680 <= "000000000000000000000000" & read_byte;
	sig_1681 <= "000000000000000000000000" & next_marker_c;
	sig_1682 <= "0000000000000000000000000000" & get_dqt_prec;
	sig_1683 <= "000000000000000000000000" & read_markers_unread_marker;
	sig_1684 <= "000000000000000000000000" & read_markers_unread_marker;
	sig_1685 <= "0000000000000000000000000000000" & get_dht_is_ac;
	sig_1686 <= "0000000000000000000000000000000" & get_dht_is_ac;
	sig_1687 <= "0000" & decodehuffmcu_n;
	sig_1688 <= sig_1612(23 downto 0) & sig_1667(7);
	sig_1689 <= yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b(31) & yuvtorgb_b;
	sig_1690 <= "00000000000000000000000" & mux_313;
	sig_1691 <= writeoneblock_indim1 & writeoneblock_outidx & writeoneblock_inidx(5 downto 0);
	sig_1692 <= yuvtorgb_uidx & yuvtorgb_i(5 downto 0);
	sig_1693 <= jpeg2bmp_main_i(1 downto 0) & jpeg2bmp_main_j(12 downto 0);
	sig_1694 <= writeoneblock_outidx & sig_1610(12 downto 0);
	sig_1695 <= decodehuffman_ac_tbl_no & decodehuffman_ac_l(5 downto 0);
	sig_1696 <= huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_l(5 downto 0);
	sig_1697 <= decodehuffman_ac_tbl_no & decodehuffman_ac_l(5 downto 0);
	sig_1698 <= huff_make_dhuff_tb_ac_tbl_no & huff_make_dhuff_tb_ac_l(5 downto 0);
	sig_1699 <= decodehuffman_dc_tbl_no & decodehuffman_dc_l(5 downto 0);
	sig_1700 <= huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_l(5 downto 0);
	sig_1701 <= decodehuffman_dc_tbl_no & decodehuffman_dc_l(5 downto 0);
	sig_1702 <= huff_make_dhuff_tb_dc_tbl_no & huff_make_dhuff_tb_dc_l(5 downto 0);
	sig_1703 <= get_dht_index & get_dht_i(8 downto 0);
	sig_1704 <= get_dht_index & get_dht_i(5 downto 0);
	sig_1705 <= get_dht_index & get_dht_i(8 downto 0);
	sig_1706 <= get_dht_index & get_dht_i(5 downto 0);
	sig_1707 <= sig_1610(1 downto 0) & sig_1645;
	sig_1708 <= "0000000000000000" & get_dqt_tmp;
	sig_1709 <= "000000000000000000000000" & read_markers_unread_marker;
	sig_1710 <= "000000000000000000000000" & read_markers_unread_marker;
	sig_1711 <= "000000000000000000000000" & read_markers_unread_marker;
	sig_1712 <= "00000000000000000000000011" & mux_759;
	sig_1713 <= "00000000000000000000000" & mux_761;
	sig_1714 <= "0000000000000000000000000000000" & read_markers_sow_soi;
	sig_1715 <= "0000000000000000000000000000" & get_dqt_prec;
	sig_1716 <= "000000000000000000000000" & read_markers_unread_marker;

	-- Remaining top-level ports assignments
	-- Those who are not assigned by component instantiation

	stdout_data <= mux_773;
	stdin_rdy <= sig_1178;

end architecture;
