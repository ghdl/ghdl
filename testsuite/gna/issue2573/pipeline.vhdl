library ieee;
use ieee.std_logic_1164.all;

entity pipeline_8 is
	generic (
  width_ppline : positive := 8
		);
	port (
		clk : in std_ulogic;
		data_in : in std_ulogic_vector(width_ppline - 1 downto 0);
		data_out : out std_ulogic_vector(width_ppline - 1 downto 0)
		);
end entity pipeline_8;

architecture structural of pipeline_8 is
	type data_int is array (1 to 3) of std_ulogic_vector(7 downto 0);
	signal data_int_a : data_int := (others => (others => '0'));
	
	component dff_8 is
		port (
			clk : in std_ulogic;
			d : in std_ulogic_vector(7 downto 0);
			q : out std_ulogic_vector(7 downto 0)
		);
	end component dff_8;

	begin
		
		pip: for i in 1 to 4 generate
			i1 : if (i = 1) generate
--				for all : dff_8 use entity work.dff_n(rtl);
--				begin
				u : dff_8 port map (
					clk => clk,
					d => data_in,
					q => data_int_a(i));
			end generate i1;
			
			i2 : if (i > 1 and i < 4) generate
--				for all : dff_8 use entity work.dff_n(rtl);
--				begin
				u : dff_8 port map (
					clk => clk,
					d => data_int_a(i - 1),
					q => data_int_a(i));
			end generate i2;
			
			i3 : if (i = 4) generate
--				for all : dff_8 use entity work.dff_n(rtl);
--				begin
				u : dff_8 port map (
					clk => clk,
					d => data_int_a(i - 1),
					q => data_out);
				end generate i3;
		end generate pip;
		
end architecture structural;

configuration config_pip of pipeline_8 is
	for structural
		for pip
			for i1
				for all dff_8 use entity work.(dff_n(rtl));
				end for;
			end for;
			for i2
				for all dff_8 use entity work.(dff_n(rtl));
				end for;
			end for;
			for i3
				for all dff_8 use entity work.(dff_n(rtl));
				end for;
			end for;
		end for;
	end for;
end configuration config_pip;
