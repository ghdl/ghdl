library ieee;
use ieee.std_logic_1164.all;



package corelib_Dict is

--  generic (
--    type     KEY_TYPE;
--    type     VALUE_TYPE;
--    function to_hash(d : in KEY_TYPE, size : positive) return natural;
--    INIT_SIZE : natural := 128
--  );


  -- REMOVE when using package generics
  constant INIT_SIZE : positive := 128;
  alias to_hash is "mod" [integer, integer return integer];
  subtype KEY_TYPE   is integer;
  subtype VALUE_TYPE is std_logic_vector;


  type PT_DICT is protected

    procedure Set (constant key : in KEY_TYPE; constant data : in VALUE_TYPE);
    procedure Get (constant key : in KEY_TYPE; data : out VALUE_TYPE);
    impure function Get (constant key : KEY_TYPE) return VALUE_TYPE;
    procedure Del (constant key : in KEY_TYPE);
    procedure Clear;
    impure function HasKey (constant key : KEY_TYPE) return boolean;
    impure function Count return natural;

  end protected PT_DICT;

  procedure Merge(d0 : inout PT_DICT; d1 : inout PT_DICT; dout : inout PT_DICT);


end package corelib_Dict;



package body corelib_Dict is


  type t_key_ptr  is access KEY_TYPE;
  type t_data_ptr is access VALUE_TYPE;


  type PT_DICT is protected body


    type t_entry;
    type t_entry_ptr is access t_entry;

    type t_entry is record
      key        : t_key_ptr;
      data       : t_data_ptr;
      last_entry : t_entry_ptr;
      next_entry : t_entry_ptr;
    end record t_entry;

    type t_entry_array is array (0 to INIT_SIZE-1) of t_entry_ptr;

    variable head  : t_entry_array := (others => null);

    variable entry_count : integer_vector(0 to INIT_SIZE-1) := (others => 0);


    -- Private method to find entry stored in dictionary
    impure function Find (constant key : KEY_TYPE) return t_entry_ptr;

    impure function Find (constant key : KEY_TYPE) return t_entry_ptr is
      variable entry : t_entry_ptr := head(to_hash(key, INIT_SIZE));
    begin
      while (entry /= null) loop
        if (entry.key.all = key) then
          return entry;
        end if;
        entry := entry.last_entry;
      end loop;
      return null;
    end function Find;


    procedure Set (constant key : in KEY_TYPE; constant data : in VALUE_TYPE) is
      variable addr  : natural := 0;
      variable entry : t_entry_ptr := Find(key);
    begin
      if (entry = null) then
        addr := to_hash(key, INIT_SIZE);
        if (head(addr) /= null) then
          entry                            := new t_entry;
          entry.key                        := new KEY_TYPE'(key);
          entry.data                       := new VALUE_TYPE'(data);
          entry.last_entry                 := head(addr);
          entry.next_entry                 := null;
          head(addr)                       := entry;
          head(addr).last_entry.next_entry := head(addr);
        else
          head(addr)            := new t_entry;
          head(addr).key        := new KEY_TYPE'(key);
          head(addr).data       := new VALUE_TYPE'(data);
          head(addr).last_entry := null;
          head(addr).next_entry := null;
        end if;
        entry_count(addr) := entry_count(addr) + 1;
      else
        entry.data.all := data;
      end if;
    end procedure Set;

    procedure Get (constant key : in KEY_TYPE; data : out VALUE_TYPE) is
      variable entry : t_entry_ptr := Find(key);
    begin
      assert entry /= null
        report PT_DICT'instance_name & ": ERROR: key " & to_string(key) & " not found"
        severity failure;
      data := entry.data.all;
    end procedure Get;

    impure function Get (constant key : KEY_TYPE) return VALUE_TYPE is
      variable entry : t_entry_ptr := Find(key);
    begin
      assert entry /= null
        report PT_DICT'instance_name & ": ERROR: key " & to_string(key) & " not found"
        severity failure;
      return entry.data.all;
    end function Get;

    procedure Del (constant key : in KEY_TYPE) is
      variable entry : t_entry_ptr := Find(key);
      variable addr  : natural := 0;
    begin
      if (entry /= null) then
        addr := to_hash(key, INIT_SIZE);
        -- remove head entry
        if(entry.next_entry = null and entry.last_entry /= null) then
          entry.last_entry.next_entry := null;
          head(addr)                  := entry.last_entry;
        -- remove start entry
        elsif(entry.next_entry /= null and entry.last_entry = null) then
          entry.next_entry.last_entry := null;
        -- remove from between
        elsif(entry.next_entry /= null and entry.last_entry /= null) then
          entry.last_entry.next_entry := entry.next_entry;
          entry.next_entry.last_entry := entry.last_entry;
        else
          head(addr) := null;
        end if;
        deallocate(entry.key);
        deallocate(entry.data);
        deallocate(entry);
        entry_count(addr) := entry_count(addr) - 1;
      end if;
    end procedure Del;

    procedure Clear  is
      variable entry   : t_entry_ptr;
      variable entry_d : t_entry_ptr;
    begin
      for i in t_entry_array'range loop
        entry := head(i);
        while (entry /= null) loop
          entry_d := entry;
          Del(entry_d.key.all);
          entry := entry.last_entry;
        end loop;
      end loop;
    end procedure Clear;

    impure function HasKey (constant key : KEY_TYPE) return boolean is
    begin
      return Find(key) /= null;
    end function HasKey;

    impure function Count return natural is
      variable value : natural := 0;
    begin
      for i in entry_count'range loop
        value := value + entry_count(i);
      end loop;
      return value;
    end function Count;


  end protected body PT_DICT;


  procedure Merge(d0 : inout PT_DICT; d1 : inout PT_DICT; dout : inout PT_DICT) is
  begin
  end procedure Merge;


end package body corelib_Dict;
