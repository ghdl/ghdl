package repro1 is
  function return_true return boolean;
end repro1;

package body repro1 is
  function slv_ones(constant width : in integer) return bit_vector is
  begin
    return (1 to width => '1');
  end function;
  
  function return_true return boolean is
    constant ones_c : bit_vector(31 downto 0) := (others => '1');
  begin
    return ones_c = slv_ones(32);
  end function;
end repro1;
