-------------------------------------------------------
-- Design Name : lfsr
-- File Name   : lfsr_updown.vhd
-- Function    : Linear feedback shift register
-- Coder       : Deepak Kumar Tala (Verilog)
-- Translator  : Alexander H Pham (VHDL)
-------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity lfsr_updown is
    generic (
        WIDTH :integer := 8
    );
    port (
        clk       :in  std_logic;                       -- Clock input
        reset     :in  std_logic;                       -- Reset input
        enable    :in  std_logic;                       -- Enable input
        up_down   :in  std_logic;                       -- Up Down input
        count     :out std_logic_vector (WIDTH-1 downto 0);   -- Count output
        overflow  :out std_logic                        -- Overflow output
    );
end entity;

architecture rtl of lfsr_updown is
    signal cnt :std_logic_vector (WIDTH-1 downto 0);
begin

    process (up_down, cnt) begin
        if (((up_down = '1') and (cnt(WIDTH-1) = '1')) or
            ((up_down = '0') and ((cnt(WIDTH-1) = '1') and 
	    (cnt(WIDTH-2 downto 0) = "0")))) then
            overflow <= '1';
        else
            overflow <= '0';
        end if;
    end process;
    
    process (clk, reset, cnt, enable, up_down)
        variable temp_a :std_logic_vector (WIDTH-1 downto 0);
        variable temp_b :std_logic :='1';
    begin
    
        temp_a := cnt and "01100011";
        temp_b :='1';
        for i in 0 to WIDTH-1 loop
            temp_b := temp_a(i) xnor temp_b;
        end loop;

        if (rising_edge(clk)) then
            if (reset = '1') then
                cnt <= (others=>'0');
            elsif (enable = '1') then
                if (up_down = '1') then
                    cnt <= (temp_b & cnt(WIDTH-1 downto 1));
                else
                    cnt <= (cnt(WIDTH-2 downto 0) & temp_b);
                end if;
            end if;
        end if;
    end process;
    count <= cnt;
    
end architecture;
