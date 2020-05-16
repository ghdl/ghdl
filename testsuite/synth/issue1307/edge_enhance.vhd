library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity edge_enhance is
    Port (  clk            : in STD_LOGIC;
            enable_feature : in std_logic;
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
end edge_enhance;

architecture Behavioral of edge_enhance is
    component line_delay is
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
    end component;
    type a_bits is array(0 to 8) of std_logic;
    type a_component is array(0 to 8) of std_logic_vector(7 downto 0);
    signal blanks : a_bits;
    signal hsyncs : a_bits;
    signal vsyncs : a_bits;
    signal reds   : a_component;
    signal greens : a_component;
    signal blues  : a_component;

    signal sobel_1_blue   : unsigned(12 downto 0) := (others => '0');
    signal sobel_1_blue_x  : unsigned(11 downto 0) := (others => '0');
    signal sobel_1_blue_y  : unsigned(11 downto 0) := (others => '0');

    signal sobel_3_blue   : unsigned(12 downto 0) := (others => '0');
    signal sobel_2_blue_x  : unsigned(11 downto 0) := (others => '0');
    signal sobel_2_blue_y  : unsigned(11 downto 0) := (others => '0');

    signal sobel_1_green_left   : unsigned(11 downto 0) := (others => '0');
    signal sobel_1_green_right  : unsigned(11 downto 0) := (others => '0');
    signal sobel_1_blue_left    : unsigned(11 downto 0) := (others => '0');
    signal sobel_1_blue_right   : unsigned(11 downto 0) := (others => '0');

    signal test : std_logic_vector(7 downto 0) := (others => '0');
    signal test2 : std_logic_vector(7 downto 0) := (others => '0');
begin

i_line_delay_2: line_delay  Port map ( 
        clk       => clk,
        in_blank  => blanks(3),
        in_hsync  => hsyncs(3),
        in_vsync  => vsyncs(3),
        in_red    => reds(3),
        in_green  => greens(3),
        in_blue   => blues(3),

        out_blank => blanks(6), 
        out_hsync => hsyncs(6),
        out_vsync => vsyncs(6), 
        out_red   => reds(6),
        out_green => test2, 
        out_blue  => test  
    );

process(clk)
    begin
        if rising_edge(clk) then
		out_green <= test2;
		out_blue <= test;
        end if;
    end process;

end Behavioral;
