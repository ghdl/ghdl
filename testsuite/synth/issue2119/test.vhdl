-- Title      : Testcase for unbounded records
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package test_pkg is
  type test_rec is record
    vec_bound   : std_logic_vector(7 downto 0);
    vec_unbound : std_logic_vector;
    single_bit  : std_logic;
  end record test_rec;
end test_pkg;

------------------------------------------------------------------------------------------------------------------------------------------------------
-- Inner module
------------------------------------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

use work.test_pkg.all;

entity test_impl is

  port (
    clk : in std_logic;
    rec_out : out test_rec
    );

end entity test_impl;
architecture str of test_impl is
begin  -- architecture str
end architecture str;

------------------------------------------------------------------------------------------------------------------------------------------------------
-- Outer Wrapper
------------------------------------------------------------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use work.test_pkg.all;
entity test is

  generic (
    unbound_len : natural := 10
    );
  port (
    clk     : in  std_logic;
    rec_out : out test_rec(vec_unbound(unbound_len-1 downto 0)));
end entity test;

architecture str of test is

begin  -- architecture str
  test_impl_1: entity work.test_impl
    port map (
      clk     => clk,                   -- [in  std_logic]
      rec_out => rec_out);              -- [out test_rec]
end architecture str;
