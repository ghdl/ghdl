library ieee;
  use ieee.std_logic_1164.all;


package QueueP is

  generic (
    type     QUEUE_TYPE;
    function to_string(d : in QUEUE_TYPE) return string
  );

  -- simple queue interface
  type t_simple_queue is protected
    procedure push (data : in  QUEUE_TYPE);
    procedure pop  (data : out QUEUE_TYPE);
    impure function is_empty  return boolean;
    impure function is_full   return boolean;
  end protected t_simple_queue;

end package QueueP;


package body QueueP is

  -- simple queue implementation
  -- inspired by noasic article http://noasic.com/blog/a-simple-fifo-using-vhdl-protected-types/
  type t_simple_queue is protected body

    constant C_QUEUE_DEPTH : natural := 64;

    type t_queue_array is array (0 to C_QUEUE_DEPTH-1) of QUEUE_TYPE;

    variable v_queue : t_queue_array;
    variable v_count : natural range 0 to t_queue_array'length := 0;
    variable v_head  : natural range 0 to t_queue_array'high   := 0;
    variable v_tail  : natural range 0 to t_queue_array'high   := 0;

    -- write one entry into queue
    procedure push (data : in QUEUE_TYPE) is
    begin
      assert not(is_full)
        report "push into full queue -> discarded"
        severity failure;
      v_queue(v_head) := data;
      v_head  := (v_head + 1) mod t_queue_array'length;
      v_count := v_count + 1;
    end procedure push;

    -- read one entry from queue
    procedure pop (data : out QUEUE_TYPE) is
    begin
      assert not(is_empty)
        report "pop from empty queue -> discarded"
        severity failure;
      data := v_queue(v_tail);
      v_tail  := (v_tail + 1) mod t_queue_array'length;
      v_count := v_count - 1;
    end procedure pop;

    -- returns true if queue is empty, false otherwise
    impure function is_empty return boolean is
    begin
      return v_count = 0;
    end function is_empty;

    -- returns true if queue is full, false otherwise
    impure function is_full return boolean is
    begin
      return v_count = t_queue_array'length;
    end function is_full;

  end protected body t_simple_queue;

end package body QueueP;
