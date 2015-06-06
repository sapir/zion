library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity alu is
    Port ( op      : in Alu_Op_Type;
           a       : in Logic_Word;
           b       : in Logic_Word;
           neg     : in std_logic;  -- whether to negate b in add
           sgnd    : in std_logic;  -- whether to treat inputs as signed in slt
           res     : out Logic_Word);
end alu;

architecture Behavioral of alu is
    signal add_res          : unsigned(16 downto 0);
    signal sll_res, srl_res : u16;
    signal slt_res          : std_logic;
    signal exts_res         : s16;
    signal tmp_res          : Logic_Word;
begin

    add_proc : process(a, b, neg, sgnd)
        variable ext17_a, ext17_b : std_logic_vector(16 downto 0);
        variable unsigned_c       : unsigned(0 downto 0);
    begin
        ext17_a := (sgnd and a(15)) & a;
        ext17_b := (sgnd and b(15)) & b;

        if neg = '1' then
            ext17_b := not ext17_b; -- we then add 1 using unsigned_c to get -b
        end if;

        unsigned_c := (0 => neg);
        add_res <= unsigned(ext17_a) + unsigned(ext17_b) + unsigned_c;
    end process;


    shift_proc : process(a, b)
        variable shift_amt : integer;
    begin
        shift_amt := to_integer(unsigned(a(3 downto 0)));

        if a(15 downto 4) = X"000" then
            sll_res <= shift_left( u16(b), shift_amt);
            srl_res <= shift_right(u16(b), shift_amt);
        else
            -- shift overflow
            sll_res <= (others => '0');
            srl_res <= (others => '0');
        end if;
    end process;


    slt_res <= add_res(16);      -- a < b if result is negative

    exts_res <= resize(signed(b(7 downto 0)), 16);


    with op select tmp_res <=
        Logic_Word(add_res(15 downto 0)) when aluop_add,
        a and b                          when aluop_and,
        a or b                           when aluop_or,
        a nor b                          when aluop_nor,
        a xor b                          when aluop_xor,
        Logic_Word(sll_res)              when aluop_sll,
        Logic_Word(srl_res)              when aluop_srl,
        (0 => slt_res, others => '0')    when aluop_slt,
        Logic_Word(exts_res)             when aluop_exts,
        (others => '0')                  when others;

    res <= tmp_res;

end Behavioral;
