package repro is
  function return_true return boolean;
end repro;

package body repro is
  function slv_ones(constant width : in integer) return bit_vector is
  begin
    return (1 to width => '1');
  end function;
  
  function return_true return boolean is
    constant ones_c : bit_vector(31 downto 0) := slv_ones(32);
  begin
    return true;
  end function;
end repro;
