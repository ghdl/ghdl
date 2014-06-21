library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-- library std;
use std.textio.all;

entity morten is
end entity;

architecture foo of morten is
    
    signal clk: std_logic := '0';
    signal rst: std_logic := '1';
    signal cnt_1: unsigned (7 downto 0);
    signal cnt_3: unsigned (7 downto 0);
    
    function to_bstring(sl : std_logic) return string is
    begin
      return "" & std_logic'image(sl)(2);  -- "" & character to get string
    end function;

    function to_bstring(slv : std_logic_vector) return string is
      alias slv_norm : std_logic_vector(1 to slv'length) is slv;
    begin
      if slv_norm'length = 0 then
        return "";
      elsif slv_norm'length = 1 then
        return to_bstring(slv_norm(1));
      else  -- slv_norm'length > 0
        return to_bstring(slv_norm(1)) & to_bstring(slv_norm(2 to slv_norm'length));
      end if;
    end function;
    
begin


PRINT:        
    process (clk) is
        variable line_v   : line;
        file     out_file : text open write_mode is "out.txt";
    begin
        if rising_edge(clk) then
            write(line_v, to_bstring(rst) & " " & 
                          to_bstring(std_logic_vector(cnt_1)) & " " & 
                          to_bstring(std_logic_vector(cnt_3))
                 );
            writeline(out_file, line_v);
      end if;
    end process;
    
COUNTER1:
    process (clk,rst)
    begin
        if rst = '1' then
            cnt_1 <= (others => '0');
        elsif rising_edge(clk) then
            cnt_1 <= cnt_1 + 1;
        end if;
    end process;

COUNTER3: 
    process (clk,rst)
    begin
        if rst = '1' then
            cnt_3 <= (others => '0');
        elsif rising_edge(clk) then
            cnt_3 <= cnt_3 + 3;
        end if;
    end process;

RESET:
    process
    begin
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        rst <= '0';
        wait;
    end process;

CLOCK:
    process 
    begin
        wait for 10 ns;
        clk <= not clk;
        if Now > 210 ns then
            wait;
        end if;
    end process;

end architecture;