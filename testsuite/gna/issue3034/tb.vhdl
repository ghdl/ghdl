library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_pkg.all;
entity tb is
end;

architecture arch of tb is
  type nrec_axis_mosi_t is record
    valid : std_logic;
    data : std_logic_vector;
    strb : std_logic_vector; -- STROBE COMES BEFORE KEEP IN DEFINTION
    keep : std_logic_vector;
    last : std_logic;
    id : std_logic_vector;
    dest : std_logic_vector;
    user : std_logic_vector;
  end record;

  type nrec_sfixed_axis_mosi_t is record
    valid : std_logic;
    data  : sfixed;
    strb  : std_logic_vector;   -- STROBE COMES BEFORE KEEP IN DEFINTION
    keep  : std_logic_vector;
    last  : std_logic;
    id    : std_logic_vector;
    dest  : std_logic_vector;
    user  : std_logic_vector;
  end record;

  function to_std_logic_vector(sig : nrec_sfixed_axis_mosi_t)
                               return nrec_axis_mosi_t is
  begin
    return (valid => sig.valid,
            data => to_slv(sig.data),
            keep => sig.keep, -- NOTICE KEEP COMES FIRST IN THE LIST HERE.
            strb => sig.strb,
            last => sig.last,
            id => sig.id,
            dest => sig.dest,
            user => sig.user);
  end function;


  signal b : nrec_sfixed_axis_mosi_t (data (7 downto 0),
                                      strb (1 downto 0),
                                      keep (1 downto 0),
                                      id (3 downto 0),
                                      dest (5 downto 0),
                                      user (7 downto 0));
  signal a : nrec_axis_mosi_t (data (7 downto 0),
                               strb (1 downto 0),
                               keep (1 downto 0),
                               id (3 downto 0),
                               dest (5 downto 0),
                               user (7 downto 0));
begin
  a <= to_std_logic_vector(b);
end;
  
