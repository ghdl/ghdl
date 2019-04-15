package pkg_c is

  type byte_vector_access_t is access string;
  type extbuf_access_t is access string(1 to integer'high);

  impure function
  get_addr(
    id : integer
  ) return extbuf_access_t;
  attribute foreign of get_addr : function is "VHPIDIRECT get_addr";

  impure function
  get_baddr(
    id : integer
  ) return byte_vector_access_t;
  attribute foreign of get_baddr : function is "VHPIDIRECT get_baddr";
  
  procedure
  set(
    index : natural;
    value : natural
  );
  
  impure function
  get(
    index : natural
  ) return natural;
end pkg_c;

package body pkg_c is
  impure function
  get_addr(
    id : integer
  ) return extbuf_access_t is begin
    assert false report "VHPI get_addr" severity failure;
  end;
  
  impure function
  get_baddr(
    id : integer
  ) return byte_vector_access_t is begin
    assert false report "VHPI get_baddr" severity failure;
  end;
  
  procedure
  set(
    index : natural;
    value : natural
  ) is
    variable a : extbuf_access_t := get_addr(0);
    variable b : byte_vector_access_t := get_baddr(0);
    variable c : byte_vector_access_t(1 to integer'high) := get_baddr(0);
  begin
    a(index+1) := character'val(value);
    --b(index+1) := character'val(value);
    c(index+1) := character'val(value);
  end;
  
  impure function
  get(
    index : natural
  ) return natural is
    variable a : extbuf_access_t := get_addr(0);
    variable b : byte_vector_access_t := get_baddr(0);
    variable c : byte_vector_access_t(1 to integer'high) := get_baddr(0);
  begin
    return character'pos(a(index+1));
    --return character'pos(b(index+1));
    return character'pos(c(index+1));
  end;
end pkg_c;
