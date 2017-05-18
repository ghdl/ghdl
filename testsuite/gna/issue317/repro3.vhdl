package repro3_sortnet_tb is
  generic (
    DATA_BITS			: positive
    );

  subtype T_DATA	is bit_vector(DATA_BITS - 1 downto 0);
  type T_DATA_VECTOR	is array(natural range <>) of T_DATA;

  procedure dec (v : inout natural);
end repro3_sortnet_tb;

package body repro3_sortnet_tb is
  procedure dec (v : inout natural) is
  begin
    v := v - 1;
  end dec;
end repro3_sortnet_tb;

entity repro3 is
end repro3;

architecture behav of repro3 is
  package tb is new work.repro3_sortnet_tb generic map (3);
begin
end behav;
