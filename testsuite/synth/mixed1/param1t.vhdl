library ieee; 
use ieee.std_logic_1164.all;

entity param1t is
  port(
    a : in std_logic_vector(3 downto 0);
    res : out std_logic_vector(3 downto 0)
  ); 
end param1t;

architecture structure of param1t is
  signal s0 : std_logic_vector(3 downto 0);
  
  -- define Verilog component 
  component param1b is
    generic (
      v : integer
    );
    port(
      x : in std_logic_vector(3 downto 0);
      r : out std_logic_vector(3 downto 0)
    );
  end component;

begin
  vlg : param1b
    generic map (v => 1)
    port map (x => a, r => s0);

  res <= s0 or "1000";
end structure;  
