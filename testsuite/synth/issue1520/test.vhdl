library ieee;
use ieee.std_logic_1164.all;

entity test is
	port (
		slv : in std_logic_vector(7 downto 0);
		sl  : in std_logic;
		int : in natural;

		vec_scal_and  : out std_logic_vector(7 downto 0);
		vec_scal_nand : out std_logic_vector(7 downto 0);
		vec_scal_or   : out std_logic_vector(7 downto 0);
		vec_scal_nor  : out std_logic_vector(7 downto 0);
		vec_scal_xor  : out std_logic_vector(7 downto 0);
		vec_scal_xnor : out std_logic_vector(7 downto 0);

		scal_vec_and  : out std_logic_vector(7 downto 0);
		scal_vec_nand : out std_logic_vector(7 downto 0);
		scal_vec_or   : out std_logic_vector(7 downto 0);
		scal_vec_nor  : out std_logic_vector(7 downto 0);
		scal_vec_xor  : out std_logic_vector(7 downto 0);
		scal_vec_xnor : out std_logic_vector(7 downto 0);

		slv_sll : out std_logic_vector(7 downto 0);
		slv_srl : out std_logic_vector(7 downto 0)
	);
end entity;

architecture arch of test is
begin
	vec_scal_and  <= slv and sl;
	vec_scal_nand <= slv nand sl;
	vec_scal_or   <= slv or sl;
	vec_scal_nor  <= slv nor sl;
	vec_scal_xor  <= slv xor sl;
	vec_scal_xnor <= slv xnor sl;

	scal_vec_and  <= sl and slv;
	scal_vec_nand <= sl nand slv;
	scal_vec_or   <= sl or slv;
	scal_vec_nor  <= sl nor slv;
	scal_vec_xor  <= sl xor slv;
	scal_vec_xnor <= sl xnor slv;

	slv_sll <= slv sll int;
	slv_srl <= slv srl int;
end architecture;
