library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--library vunit_lib;
--context vunit_lib.vunit_context;
package mbox_mem_model_pkg is

type mem_blk_list_t is protected
    procedure mem_write(blk_addr : in  std_logic_vector;
                        addr     : in  std_logic_vector;
                        data     : in  std_logic_vector); 
end protected;

end mbox_mem_model_pkg;

package body mbox_mem_model_pkg is

type mem_blk_list_t is protected body

    type data_arr_t is array(integer range <>) of std_logic_vector;
    type blk_t;
    type blk_ptr_t is access blk_t;
    type blk_t is record
        addr         : std_logic_vector;
        data         : data_arr_t;
        next_blk_ptr : blk_ptr_t;
    end record;

    variable root_blk_ptr : blk_ptr_t;

    -- This procedure writes an entry to the memory.
    -- If the target location is in a block that does not exist that block is created.
    -- If the target location in in an existing block then the array element within that block is update.
    -- Everytime a new entry block is written the block the entry belongs to is moved to the front of the list.
    -- This hopefully will improve performance due to the fact most memory access have locality.
    procedure mem_write(blk_addr   : in  std_logic_vector;
                        addr       : in  std_logic_vector;
                        data       : in  std_logic_vector) is
        variable new_blk      : blk_ptr_t;
        variable found_blk    : boolean := false;
        constant BLK_RANGE      : integer := blk_addr'high-blk_addr'low+1;
        constant BLK_ADDR_WIDTH : integer := maximum(1,BLK_RANGE);
        constant BLK_SIZE       : integer := 2**(addr'high-addr'low+1);
        constant DATA_WIDTH     : integer := data'high-data'low+1;
        variable blk_addr_v     : std_logic_vector(BLK_ADDR_WIDTH-1 downto 0);
    begin

        -- Set block address.
        for i in 0 to BLK_ADDR_WIDTH-1 loop
            blk_addr_v(i) := '0';
        end loop; 
        if (BLK_RANGE >= 1) then
            blk_addr_v := blk_addr;
        end if;

        -- Check and see if the block is in the linked list.  If so, move it to the root.
        --found_blk := find_blk(blk_addr_v);

        -- If the block is not found create a new one and move it to the head.
        if (not found_blk) then
            
            -- Create new block and assign address and undefine data.
            new_blk              := new blk_t(addr(BLK_ADDR_WIDTH-1 downto 0),data(0 to BLK_SIZE-1)(DATA_WIDTH-1 downto 0));
            new_blk.next_blk_ptr := root_blk_ptr;
            new_blk.addr         := blk_addr_v;
            root_blk_ptr         := new_blk;

            -- Set data to undefined.
            for i in 0 to BLK_SIZE-1 loop
                for j in 0 to DATA_WIDTH-1 loop
                    new_blk.data(i)(j) := 'U';
                end loop;
            end loop;

        end if;

        -- Assing data.
        root_blk_ptr.data(to_integer(unsigned(addr))) := data;

    end procedure;

    -- This procedure reads an entry from memory.
    -- If the target location is in a block that does not exist, undefined results are returned.
    -- If the target location is in an existing block then the array element within that block is returned.
    -- Everytime a block is read that block is moved to the front of the list.
    -- This hopefully will improve performance due to the fact most memory access have locality.
    
end protected body;

end mbox_mem_model_pkg;

