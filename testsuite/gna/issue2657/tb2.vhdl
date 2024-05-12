entity tb2 is
end tb2;

architecture sim of tb2 is
    
    type SlvArray_t is array(integer range<>) of bit_vector;
    type UnsignedArray_t is array(integer range<>) of bit_vector;

    constant a : UnsignedArray_t(0 to 0)(0 downto 0) := (others => (others => '0'));

    procedure p is
          constant b : SlvArray_t := SlvArray_t(a);
    begin
      report "p";
    end p;
    
begin
    
    process
    begin
      p;
      wait;
    end process;
    
end sim;
