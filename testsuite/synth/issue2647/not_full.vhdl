library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity case_statement_not_full is
	port(
        x : in  std_logic_vector(1 downto 0);
        y : out std_logic_vector(1 downto 0)
	);
end entity;

architecture arch of case_statement_not_full is
begin

    process (all) is
    begin
        case x is
            when "00" => y <= "00";
            when "01" => y <= "01";
            when "10" => y <= "10";
            when "11" => y <= "11";
            when others => null;
        end case;
    end process;

end architecture;
