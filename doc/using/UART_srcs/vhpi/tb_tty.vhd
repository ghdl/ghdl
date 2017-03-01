--tb_tty.vhd
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
use work.tty_pkg.all;

ENTITY tb_tty IS
END tb_tty;
 
ARCHITECTURE behavior OF tb_tty IS 
  	
signal clk          : std_logic := '0';
signal reset        : std_logic;

signal data_in     	: std_logic_vector(7 downto 0);
signal wdata     	: std_logic_vector(7 downto 0);
signal wr_en        : std_logic:='1';
signal wr           : std_logic;
signal rd_en        : std_logic;
signal rd           : std_logic;
signal a            : integer;
signal c            : integer;


component capitalisation is
  port(
    clk        : in  std_logic;
    reset      : in  std_logic;
    --in
    rdata      : in  std_logic_vector(7 downto 0);
    rd_en      : in  std_logic;
    rd         : out std_logic;
    --out
    wdata      : out std_logic_vector(7 downto 0);
    wr_en      : in  std_logic;
    wr         : out std_logic
    );
end component;

   -- Clock period definitions
   constant clk_period : time := 10 ns;
 
subtype by_te is character;
type f_byte is file of by_te;

BEGIN

--file open
  process
    begin c<=tty_open(0);
    wait;
  end process;

--read 
process (clk)

variable b: integer;
begin
  if rising_edge(CLK) then
    a<= read_enable(0);
    if a=1 then
      data_in<=std_logic_vector(to_unsigned(read_data(0),8));
      rd_en<='1';
    else
      rd_en<='0';
    end if;
  end if;  
 
end process;

--write 
process (clk)
variable b: integer;

  begin 
    if rising_edge(CLK) then     
      if reset='0' then
        if wr='1' then
          b:=to_integer(unsigned(wdata));
          write_data(b);         
        end if;
      end if;
    end if;
  end process;
  
   stim_proc : process
     begin
       reset <= '1';

      wait for 50 ns;
         reset <='0'; 
      wait;
    end process;

  clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;

engine: capitalisation 
  port map(
    clk        => clk,
    reset      => reset,
    --in
    rdata      => data_in,
    rd_en      => rd_en,
    rd         => rd,
    --out
    wdata      => wdata,
    wr_en      => wr_en,
    wr         => wr
    );
END;
