package repro2 is
  procedure return_true (res : out boolean);
end repro2;

package body repro2 is
  function slv_ones(constant width : in integer) return bit_vector is
  begin
    return (1 to width => '1');
  end function;

  procedure return_true (res : out boolean) is
    constant ones_c : bit_vector(31 downto 0) := (others => '1');
    constant two_c : bit_vector := slv_ones(32);
  begin
    wait for 1 ns;
    res := ones_c = two_c;
  end;
end repro2;
