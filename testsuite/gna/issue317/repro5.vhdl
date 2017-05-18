package repro5_gen is
  generic (type t;
           function id (a : t) return t);

  function id2 (p : t) return t;

  type prot is protected
     procedure Set (V : natural);
     impure function Get return Natural;
  end protected prot;
end repro5_gen;

package body repro5_gen is
  function id2 (p : t) return t
  is
--    constant c : t := p;
  begin
    return id (p);
  end id2;
  
  type prot is protected body
     variable val : Natural;
     procedure Set (V : natural) is
     begin
       val := v;
     end Set;
  
    impure function Get return Natural is
    begin
      return val;
    end Get;
  end protected body prot;
end repro5_gen;

package repro5_sortnet_tb is
  generic (
    DATA_BITS			: positive;
    LEN : Positive
    );

  subtype T_DATA	is bit_vector(DATA_BITS - 1 downto 0);
  type T_DATA_VECTOR	is array(1 to LEN) of T_DATA;

  function id (a : t_data_vector) return t_data_vector;

  package inst is new work.repro5_gen
                      generic map (t => t_data_vector, id => id);
end repro5_sortnet_tb;

package body repro5_sortnet_tb is
  function id (a : t_data_vector) return t_data_vector is
  begin
    return a;
  end id;
end repro5_sortnet_tb;

entity repro5 is
end repro5;

architecture behav of repro5 is
  package tb is new work.repro5_sortnet_tb generic map (3, 4);
begin
end behav;
