-------------------------------------------------------------------------------
-- Title      : Full-Adder Test Bench
-- Project    :
-------------------------------------------------------------------------------
--! \file     full_adder_tb.vhd
--! \author   Jose Correcher <jose.correcher@gmail.com>
--! \date     Created    : 2021-07-02
--            Last update: 2021-07-02
-- Platform   :
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description:
--! \class    full_adder
--! \details
--!            This is the test bench for full-adder design.
-------------------------------------------------------------------------------
-- Copyright (c) 2021
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2021-07-02  1.0      jcorrecher  Created
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- * libraries
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------
-- * entity
-------------------------------------------------------------------------------

entity full_adder_tb is

end entity full_adder_tb;

-------------------------------------------------------------------------------
-- * architecture body
-------------------------------------------------------------------------------

architecture sim of full_adder_tb is

-- ** record declaration
  type rc_data is record
    a    : std_logic;
    b    : std_logic;
    cin  : std_logic;
    s    : std_logic;
    cout : std_logic;
  end record rc_data;

-- ** truth table declaration
  type fa_array is array (natural range <>) of rc_data;
  constant fa_table : fa_array :=
    (('0', '0', '0', '0', '0'),
     ('0', '0', '1', '1', '0'),
     ('0', '1', '0', '1', '0'),
     ('0', '1', '1', '0', '1'),
     ('1', '0', '0', '1', '0'),
     ('1', '0', '1', '0', '1'),
     ('1', '1', '0', '0', '1'),
     ('1', '1', '1', '1', '1'));

-- ** signal declaration
  signal a, b, cin, s, cout : std_logic;

begin

-------------------------------------------------------------------------------
-- * TB0 : Test
-------------------------------------------------------------------------------

  process
  begin
    --  Check each pattern.
    for i in fa_table'range loop
      --  Set the inputs.
      a   <= fa_table(i).a;
      b   <= fa_table(i).b;
      cin <= fa_table(i).cin;
      --  Wait for the results.
      wait for 1 ns;
      --  Check the outputs.
      assert s = fa_table(i).s
        report "bad sum value" severity error;
      assert cout = fa_table(i).cout
        report "bad carry out value" severity error;
    end loop;
    assert false report "end of test" severity note;
    std.env.stop;
    wait;
  end process;

-------------------------------------------------------------------------------
-- * TB1 : DUV + FM
-------------------------------------------------------------------------------

-- ** instance

  DUV : entity work.full_adder
    port map (
      a    => a,
      b    => b,
      cin  => cin,
      s    => s,
      cout => cout
      );

end architecture sim;
