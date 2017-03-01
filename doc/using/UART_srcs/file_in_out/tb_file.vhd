--tb_file.vhd
------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY tb_file IS
END tb_file;
--vhdl no seek or rewind function in VHDL
 
ARCHITECTURE behavior OF tb_file IS 
 
signal clk          : std_logic := '0';
signal reset        : std_logic;

signal rdata     	: std_logic_vector(7 downto 0);
signal rd_en        : std_logic;
signal rd           : std_logic;

signal wdata        : std_logic_vector(7 downto 0);
signal wr_en        : std_logic:='1';
signal wr           : std_logic;

    -- Component Declaration for the Unit Under Test (UUT)
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

process(clk)
begin
  if rising_edge (clk) then
    if reset='0' then
      wr_en<='1';
    else
      wr_en<='0';
    end if;
  end if;
end process;

--read a file
process (reset,clk)
constant file_name: string:="test.txt";
file in_file: f_byte open read_mode is file_name;

variable a:character;
    
begin
  if reset='1' then
    rd_en<='0';
  else
  
    if rising_edge(clk) then
      if rd_en='0' or rd='1' then    
        if not endfile (in_file) then
          read(in_file,a);
          rdata<=std_logic_vector(to_unsigned(character'pos(a),8));
          --very tricky the conversation
          rd_en<='1';
        else
          rd_en<='0';
        end if;
      end if;
    end if;
       --wait until rising_edge(CLK) and rd='1';
  end if;  
end process;



--write a file
process (clk)
constant file_name: string:="test1.txt";
file out_file: f_byte open write_mode is file_name;

----variable in_line,out_line: line;
variable b:character;

  begin 
    if rising_edge(CLK) then
      if reset='0' then
        if wr='1' then
          b:=character'val(to_integer(unsigned(wdata)));
          write(out_file,b);
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
     
    rdata      => rdata,
    rd_en      => rd_en,
    rd         => rd,
    
    wdata      => wdata,
    wr_en      => wr_en,
    wr         => wr 
     );


END;
