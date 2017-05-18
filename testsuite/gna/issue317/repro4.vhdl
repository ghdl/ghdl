package repro4_gen is
  generic (type t;
           function id (a : t) return t);

  function id2 (p : t) return t;
end repro4_gen;

package body repro4_gen is
  function id2 (p : t) return t
  is
--    constant c : t := p;
  begin
    return id (p);
  end id2;
end repro4_gen;

package repro4_sortnet_tb is
  generic (
    DATA_BITS			: positive;
    LEN : Positive
    );

  subtype T_DATA	is bit_vector(DATA_BITS - 1 downto 0);
  type T_DATA_VECTOR	is array(1 to LEN) of T_DATA;

  function id (a : t_data_vector) return t_data_vector;

  package inst is new work.repro4_gen
                      generic map (t => t_data_vector, id => id);
end repro4_sortnet_tb;

package body repro4_sortnet_tb is
  function id (a : t_data_vector) return t_data_vector is
  begin
    return a;
  end id;
end repro4_sortnet_tb;

entity repro4 is
end repro4;

architecture behav of repro4 is
  package tb is new work.repro4_sortnet_tb generic map (3, 4);
begin
end behav;
