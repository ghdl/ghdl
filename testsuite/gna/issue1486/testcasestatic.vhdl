library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

entity TestcaseStatic is
  port (
  SEL    : in std_logic_vector(1 downto 0);
  DATA0  : in std_logic_vector(7 downto 0);
  DATA1  : in std_logic_vector(7 downto 0);
  DATA2  : in std_logic_vector(7 downto 0);
  DATA3  : in std_logic_vector(7 downto 0);
  RESULT : out std_logic_vector(7 downto 0)
  );
end;

architecture test of TestcaseStatic is

  constant SEL_ZERO : std_logic_vector(1 downto 0) := "00"; 

begin

  process(SEL, DATA0, DATA1, DATA2, DATA3)
    begin
      case (SEL) is
	when SEL_ZERO => 
          RESULT <= DATA0;
	when std_logic_vector(unsigned(SEL_ZERO)+1) => 
          RESULT <= DATA1;
	when std_logic_vector(unsigned(SEL_ZERO)+2) => 
          RESULT <= DATA2;
	when std_logic_vector(unsigned(SEL_ZERO)+3) => 
          RESULT <= DATA3;
        when others =>
          RESULT <= (others => 'X');
      end case;
    end process;

end test;
