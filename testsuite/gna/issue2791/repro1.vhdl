package repro1_pkg is
    type blk_t;
    type blk_ptr_t is access blk_t;
    type blk_t is record
        addr         : bit_vector;
        data         : bit_vector;
        next_blk_ptr : blk_ptr_t;
    end record;

    impure function build(addr     : in  bit_vector;
                   data     : in  bit_vector) return blk_ptr_t;
end;

package body repro1_pkg is
  impure function build(addr     : in  bit_vector;
                   data     : in  bit_vector) return blk_ptr_t
  is
    constant BLK_ADDR_WIDTH : integer := addr'length;
    constant DATA_WIDTH     : integer := data'length;
    variable t : blk_t(addr(BLK_ADDR_WIDTH-1 downto 0),
                       data(DATA_WIDTH-1 downto 0));

    variable res : blk_ptr_t;
  begin
    res := new blk_t(addr(BLK_ADDR_WIDTH-1 downto 0),
                     data(DATA_WIDTH-1 downto 0));
    res.addr            := addr;
    res.data := data;

    return res;
  end build;
end;

