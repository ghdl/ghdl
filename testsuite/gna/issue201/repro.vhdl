package gen is
  generic (type t);
end gen;

entity e is
end entity;

architecture a of e is
  subtype T_DATA		is bit_vector(31 downto 0);
  type T_DATA_VECTOR		is array(natural range <>) of T_DATA;

  package pkg is new work.gen generic map (t => t_data_vector (31 downto 0));
begin
end architecture;
