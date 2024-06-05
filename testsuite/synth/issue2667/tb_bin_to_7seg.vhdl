library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_bin_to_7seg is
end;

architecture tb of tb_bin_to_7seg is
	signal clk: std_logic;
	signal bin: unsigned(3 downto 0);

        subtype seg_vec is std_logic_vector(6 downto 0);

	signal segs: seg_vec;
begin
	clk_pr: process
	begin
          for i in 1 to 16 loop
		clk <= '0';
		wait for 5 ns;
		clk <= '1';
		wait for 5 ns;
          end loop;
          wait;
	end process;
	
        bin <= to_unsigned(1, bin);

	dut: entity work.bin_to_7seg
		port map(
			clk_in => clk,
			bin_in => bin,
			segs_out => segs);

        process (clk)
          type res_arr is array (natural range <>) of seg_vec;
          constant res : res_arr (0 to 15) :=
            ("UUUUUUU", 7x"00", 7x"00", 7x"00", 7x"00", 7x"00", 7x"10", 7x"20",
             7x"00", 7x"00", 7x"00", 7x"00", 7x"00", 7x"10", 7x"20", 7x"00");
          variable idx : natural := 0;
        begin
          if rising_edge(clk) then
            assert segs = res (idx) report "segs=" & to_bstring(segs)
              severity failure;
            idx := idx + 1;
          end if;
        end process;

end;
