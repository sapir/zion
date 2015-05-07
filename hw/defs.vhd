library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;


package defs is
    -- single-bit vector
    subtype lvbit is std_logic_vector(0 downto 0);

    subtype Logic_Byte is std_logic_vector(7 downto 0);
    subtype Logic_Word is std_logic_vector(15 downto 0);
    subtype MemWordAddr is std_logic_vector(12 downto 0);
    subtype DataByteAddr is std_logic_vector(10 downto 0);

    subtype Instr_Type is std_logic_vector(17 downto 0);
    subtype Reg_Index is std_logic_vector(3 downto 0);

    -- index of $ra
    constant ra_reg_idx : Reg_Index := "1111";

    subtype u16 is unsigned(15 downto 0);
    subtype u8 is unsigned(7 downto 0);
    subtype u4 is unsigned(3 downto 0);

    type Opcode_Type is (
        opc_add, opc_sub, opc_slt, opc_sltu,
        opc_lb, opc_lw, opc_sb, opc_sw,
        opc_li8, opc_lui, opc_addi, opc_ori,
        opc_b, opc_bal, opc_beqz, opc_bnez,
        opc_sll, opc_srl, opc_slli, opc_srli,
        opc_and, opc_or, opc_nor, opc_xor,
        opc_jr, opc_jalr,
        opc_exts, opc_break
        );

    type Alu_Op_Type is (
        aluop_add,
        aluop_sub,
        aluop_and,
        aluop_or,
        aluop_nor,
        aluop_xor,
        aluop_sll,
        aluop_srl,
        aluop_exts,
        aluop_slt,
        aluop_sltu
        );


    -- types used for pipeline stage interfaces

    -- value forwarded from hazard control
    type FwdValue is
        record
            -- whether to use the forwarded value
            use_fwd : std_logic;

            -- the value itself
            fwd_val : Logic_Word;
        end record;

    type ImmOrReg_Type is
        record
            imm     : Logic_Word;
            reg_val : Logic_Word;
            -- if '1', use register. if '0', use immediate value.
            use_reg : std_logic;
        end record;

    type Branch_Type is (b_none, b_always_imm, b_always_reg, b_eqz, b_nez);
    type Write_Type is (wr_none, wr_alu_to_reg, wr_memb_to_reg, wr_memw_to_reg,
        wr_reg_to_memb, wr_reg_to_memw, wr_pc_plus_2_to_ra);

    type Stage_1_2_Interface is
        record
            -- inputs to stage 2

            -- if '1', this instruction shouldn't be performed, i.e. writes and
            -- branches should be cancelled
            invalid_flag : std_logic;

            alu_op      : Alu_Op_Type;

            value1      : ImmOrReg_Type;    -- associated register is in rs
            value2      : ImmOrReg_Type;    -- associated register is in rt

            -- type of [conditional] branch to be performed. register to be
            -- tested, if any, will be in value1. (we're ignoring the ALU's
            -- output so it doesn't really matter if registers get input to it)
            branch_type : Branch_Type;
            -- pc after branch (except for b_always_reg, in which case reg2 is
            -- used). not calculated using ALU to avoid data hazard detection
            -- on ALU inputs, when in fact we already know the correct values.
            branch_dest : MemWordAddr;

            -- inputs to stages 3 & 4
            wr_type     : Write_Type;
            wr_reg_idx  : Reg_Index;
            pc_plus_2   : MemWordAddr;
        end record;

    constant Stage_1_2_Interface_zero : Stage_1_2_Interface
        := (invalid_flag => '0',
            alu_op      => aluop_add,
            value1      => (imm => (others => '0'),
                            reg_val => (others => '0'),
                            use_reg => '0'),
            value2      => (imm => (others => '0'),
                            reg_val => (others => '0'),
                            use_reg => '0'),
            branch_type => b_none,
            wr_type     => wr_none,
            others      => (others => '0'));


    type Stage_2_3_Interface is
        record
            alu_res     : Logic_Word;

            -- copied from stage 1
            wr_type     : Write_Type;   -- rather than forward invalid_flag to
                                            -- stage 3, this just gets set to
                                            -- wr_none
            wr_reg_idx  : Reg_Index;
            reg2_val    : Logic_Word;   -- not necessarily copied from stage 1
                                            -- due to data hazard handling
            pc_plus_2   : MemWordAddr;
        end record;


    ------------------------------------------
    -- Memory-mapped I/O addresses (MSB is 1)
    ------------------------------------------
    -- LEDs bitmask. byte-only and write-only.
    constant iomem_addr_leds : Logic_Word := "1000000000000000";

    function word_to_mem_addr(signal w : in Logic_Word) return MemWordAddr;

    function mem_addr_to_word(signal ma : in MemWordAddr) return Logic_Word;

end defs;

package body defs is

    function word_to_mem_addr(signal w : in Logic_Word) return MemWordAddr is
    begin
        -- word addresses always have lsb = 0, so ignore lsb
        return w(13 downto 1);
    end word_to_mem_addr;

    function mem_addr_to_word(signal ma : in MemWordAddr) return Logic_Word is
    begin
        -- inverse of word_to_mem_addr
        return "00" & ma & "0";
    end mem_addr_to_word;

end defs;
