library ieee;
use ieee.std_logic_1164.all;

package repro is
  type SPIrecGeneric is record
    BIT : positive;
  end record;	
    
  type SPI_Rec_input is record
    SPI_Data_in : std_logic_vector (SPIrecGeneric.BIT-1 downto 0);
  end record;					
    
  type SPI_Rec_Output is record
    SPI_Data_out : std_logic_vector (SPIrecGeneric.BIT-1 downto 0);	
  end record;	
end repro;
