library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity line_delay is
    Port ( clk : in STD_LOGIC;
           -------------------------------
           -- VGA data recovered from HDMI
           -------------------------------
           in_blank  : in std_logic;
           in_hsync  : in std_logic;
           in_vsync  : in std_logic;
           in_red    : in std_logic_vector(7 downto 0);
           in_green  : in std_logic_vector(7 downto 0);
           in_blue   : in std_logic_vector(7 downto 0);
     
           -----------------------------------
           -- VGA data to be converted to HDMI
           -----------------------------------
           out_blank : out std_logic;
           out_hsync : out std_logic;
           out_vsync : out std_logic;
           out_red   : out std_logic_vector(7 downto 0);
           out_green : out std_logic_vector(7 downto 0);
           out_blue  : out std_logic_vector(7 downto 0));
end line_delay;

architecture Behavioral of line_delay is
    type mem_block is array (0 to 511) of std_logic_vector(26 downto 0);
    signal mem_4 : mem_block := (others => (others => '0'));
    signal wr_addr    : unsigned(8 downto 0) := (others =>'1');
    signal mid_3      : std_logic_vector(26 downto 0) := (others =>'0');
begin

process(clk)
    variable mem_4_out : std_logic_vector(26 downto 0);
    begin
        if rising_edge(clk) then
            mem_4_out := mem_4(to_integer(wr_addr));
            out_green <= mem_4_out(18 downto 11);
            out_blue  <= mem_4_out(10 downto  3);
            mem_4(to_integer(wr_addr)) <= mid_3;
        end if;
    end process;

end Behavioral;
