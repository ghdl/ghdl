library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-------------------------------------------------------------------------------

entity barrel_shifter is

  generic (
    NBITS : positive := 8);

  port (
    din, d : in  std_logic_vector(NBITS-1 downto 0);  -- data in, shift distance
    dout   : out std_logic_vector(NBITS-1 downto 0));  -- data out

end entity barrel_shifter;

-------------------------------------------------------------------------------

architecture dfl of barrel_shifter is
  -- TODO: Calculate the number of required shift stages as a constant.
  constant nshift : natural := natural(log2(real(NBITS)));

  -- custom vector for the shift stages
  type my_vec is array(0 to nshift) of std_logic_vector(NBITS-1 downto 0);
  signal vector : my_vec;
  -- vector of zeros
  constant ZEROS : std_logic_vector(NBITS-1 downto 0) := (others => '0');

begin  -- architecture dfl
    vector(0) <= din;

    gen: for i in 0 to nshift-1 generate
        vector(i+1) <= (vector(i)(NBITS-1-2**i downto 0)
                       & ZEROS(2**i-1 downto 0))
                   when d(i) = '1'
                   else vector(i);
    end generate gen;
    dout <= vector(nshift);

end architecture dfl;
