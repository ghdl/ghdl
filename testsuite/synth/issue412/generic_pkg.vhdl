-- package containing a type-generic D Flip Flop
-- may not be 100% valid VHDL code, contact ktbarrett on gitter
-- non-generic version does synthesize correctly
package generic_pkg is

  procedure generic_FF
    generic (
      constant T: type)
    paramater (
      signal   q    : out T;
      signal   d    : in T;
      signal   clk  : in std_logic;
      signal   rst  : in std_logic;
      constant INIT : in T;
      signal   en   : in std_logic := '1');

end package generic_pkg;
  
package body generic_pkg is
  
  procedure generic_FF
    generic (
      constant T: type)
    paramater (
      signal   q    : out T;
      signal   d    : in T;
      signal   clk  : in std_logic;
      signal   rst  : in std_logic;
      constant INIT : in T;
      signal   en   : in std_logic := '1')
    is
  begin
    if (rising_edge(clk)) then
      if (rst /= '0') then
        q <= INIT;
      elsif (en = '1') then
        q <= d;
      end if;
    end if;
  end procedure generic_FF;
      
end package body generic_pkg; 
