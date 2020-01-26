library ieee;
use ieee.std_logic_1164.all;

entity ent1_disp is
	port (
		clk : in std_logic;
		i : in std_logic_vector(31 downto 0);
		o : out std_logic_vector(7 downto 0)
	);
end;

architecture a of ent1_disp is
	function switch_endianness(x : std_logic_vector(31 downto 0)) return std_logic_vector is
	begin
		return x(7 downto 0) & x(15 downto 8) & x(23 downto 16) & x(31 downto 24);
	end function;

        procedure disp (v : std_logic_vector) is
        begin
          report "left: " & natural'image (v'left);
          if v'ascending then
            report "to";
          else
            report "downto";
          end if;
          report "right: " & natural'image (v'right);
        end disp;
begin
	process
	begin
          disp (switch_endianness(i));
          wait;
	end process;
end;
