library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity config_example is
port(
    reset:  in  std_logic;
    mclk:   in  std_logic;			
    sdat:   out std_logic;
    sclk:   out std_logic);
end entity;

architecture rtl of config_example is
signal mcnt,mnct1: std_logic_vector(6 downto 0);
signal bcnt: std_logic_vector(3 downto 0);	
signal wdata: std_logic_vector(8 downto 0);	
signal part: std_logic_vector(1 downto 0);	
signal mm_reg: std_logic_vector(4 downto 0);
signal mm: std_logic_vector(4 downto 0);	
signal addr: std_logic_vector(7 downto 0);	
signal data: std_logic_vector(7 downto 0);
constant    wr       :   std_logic                       := '0';
constant    device   :   std_logic_vector(7 downto 0)    := x"55";
type        config_data_type is array (7 downto 1) of std_logic_vector(7 downto 0);
constant    data_reg :   config_data_type:=(X"03",X"02",X"08",X"11",X"33",X"12",X"18");
type   WRSTATE is (T0,T1,T2);	
signal  config_st: WRSTATE;	
begin

fenp:process(reset,mclk)
begin
    if(reset = '1')then
        mcnt <= (others => '0');
    elsif(mclk'event and mclk = '1')then
        mcnt <= mcnt + '1';
    end if;
end process;
	mm_reg <= mm;
process(reset,mclk)             --
begin
    if(reset = '1')then
	    wdata <= (others => '0');
	    bcnt <= (others => '0');
    	part <= "00";
    	mm <= "10000";
	    config_st <= T0;
    	data <= x"00";
    	addr <= x"08";
    elsif(rising_edge(mclk))then
        if mcnt = "1111111" then
	        case config_st is
            when T0 => config_st <= T1;bcnt <= (others => '0');
		        data <= x"00";
		        case part is
        		    when "00" => addr <= x"08"; mm <= "10000";	
		            when "01" => addr <= x"40"; mm <= "11000";	
		            when "10" => addr <= x"0e"; mm <= "00110"; data <= data_reg(6);--
    		        when others => addr <= x"08"; mm <= "00001"; data <=data_reg(7);              
		        end case;
		        wdata <= device(6 downto 0) & wr & '0';		
	        when T1 => 			
		        if bcnt = "1000" then  
			        bcnt <= (others => '0');
			        wdata <= addr & '0';			
			        config_st <= T2;			
		        else 
			        bcnt <= bcnt + '1';			
		        end if;
	        when T2 =>						
		        if bcnt = "1000" then				
			        bcnt <= (others => '0');
			        wdata <= data & '0';			
			        config_st <= T0;			
		        else 
			        bcnt <= bcnt + '1';
			        config_st <= T0;
					config_st <= T1;							-----error1
		        end if;
	       -- when others => config_st <= T0;					-----error2	
            end case;
        end if;
    end if;
end process;

sdat <= '0';													-----error3	
process(reset,mclk)												-----error4	
begin
    if(reset ='1')then
	    sclk <= '0';
	    sdat <= '0';
    end if;
end process;
process(reset,mclk)	
--process(reset,mclk,config_st)									-----error5
--process(mclk)													-----error6	
begin
    if(reset ='1')then
	    sclk <= '0';
	    sdat <= '0';
    elsif(rising_edge(mclk))then	 
	    sclk <= mcnt(6);		
        case config_st is
        when T0 =>		                        
            if mcnt < "1100000" then            
                sdat <= '1';
            else
	        sdat <= '0';
            end if;
        when T1 => sclk <= '0'; sdat <= '0';	
        when others =>				
	        if mcnt= "0100000" then	
		        sdat <= wdata(8-conv_integer(bcnt));
	        end if;
        end case;
    end if;
end process;

end rtl;
