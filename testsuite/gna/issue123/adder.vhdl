library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ADDER is 
  generic( WIDTH : positive := 8 );
  port( CIN  : in  std_logic;
        A    : in  std_logic_vector(WIDTH-1 downto 0);
        B    : in  std_logic_vector(WIDTH-1 downto 0);
        F    : out std_logic_vector(WIDTH-1 downto 0);
        COUT : out std_logic);
end entity ADDER;

-- Ripple Carry Adder
architecture RCA of ADDER is 
  signal CIN0 : unsigned(0 downto 0);
  signal FIN  : unsigned(WIDTH downto 0);
begin
  CIN0(0) <= CIN;
  FIN <= resize(unsigned(A), WIDTH+1) + resize(unsigned(B), WIDTH+1) + CIN0; -- yes, I know it's not a ripple carry adder
  F    <= std_logic_vector(FIN(WIDTH-1 downto 0));
  COUT <= FIN(WIDTH);
end architecture RCA;

-- Carry Select Adder
architecture CSA of ADDER is
  component ADDER is 
    generic( WIDTH : positive );
    port( CIN  : in  std_logic;
          A    : in  std_logic_vector(WIDTH-1 downto 0);
          B    : in  std_logic_vector(WIDTH-1 downto 0);
          F    : out std_logic_vector(WIDTH-1 downto 0);
          COUT : out std_logic);
  end component ADDER;
  signal F0, F1       : std_logic_vector(WIDTH-1 downto 0);
  signal COUT0, COUT1 : std_logic;
begin
  ADD0: ADDER generic map( WIDTH => WIDTH)
    port map ( 
          CIN  => '0'  ,
          A    => A    ,
          B    => B    ,
          F    => F0    ,
          COUT => COUT0 );
  ADD1: ADDER generic map( WIDTH => WIDTH)
    port map ( 
          CIN  => '1'  ,
          A    => A    ,
          B    => B    ,
          F    => F1    ,
          COUT => COUT1 );
  COUT <= COUT1 when CIN = '1' else COUT0;
  F    <= F1    when CIN = '1' else F0;
end architecture CSA;

-- here's the configuration
configuration CSAC of ADDER is
  for CSA
    for all: ADDER
      use entity work.ADDER(RCA);
    end for;
  end for;
end configuration CSAC;
