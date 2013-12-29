library ieee;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;
entity twoscompliment is
      generic
      (
              Nbits : positive := 8 
       );
 port 
( 
           --Inputs
           A : in std_logic_vector (Nbits-1 downto 0);
           --Outputs
           Y : out std_logic_vector (Nbits downto 0)
);
end twoscompliment;

architecture twoscompliment_v1 of twoscompliment is
    constant ONE:   UNSIGNED(Y'RANGE) := (0 => '1', others => '0');
begin
     Y <= std_logic_vector(unsigned (not A) + ONE);
end twoscompliment_v1;

architecture twoscompliment_v2 of twoscompliment is
signal temp : std_logic_vector(Nbits-1 downto 0);
begin
  temp <= not A;
  Y    <= std_logic_vector(unsigned(temp) + 1);
end twoscompliment_v2;

library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture foo of test is
    -- counts on default value for Nbits in DUT = 8)
    signal A:   std_logic_vector (7 downto 0) := (0=>'1', others => '0');  -- ONE
    signal Y:   std_logic_vector ( 8 downto 0);
begin
    DUT: entity work.twoscompliment(twoscompliment_v2)
    port map (
        A => A,
        Y => Y
    );
        
end architecture;