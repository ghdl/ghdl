library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity Adder is
	
generic(
    N : positive := 4
    );
    
port(
    A    : in  std_logic_vector(N-1 downto 0);
    B    : in  std_logic_vector(N-1 downto 0);
    Cin  : in  std_logic;
    Sum  : out std_logic_vector(N-1 downto 0);
    Cout : out std_logic
    );
    
end Adder;

architecture RTL of Adder is
	signal cout_sum: std_logic_vector(Sum'length downto 0);
begin

--	This works fine:
--  cout_sum <= ("0" & A) + B + Cin;
--	Cout <= cout_sum(Sum'length);
--	Sum <= cout_sum(Sum'length-1 downto 0);

-- This crashes GHDL:
	(Cout, Sum) <= ("0" & A) + B + Cin;

end RTL; 
