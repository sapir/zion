library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage2 is
    Port (
        clk             : in std_logic;

        -- inputs from stage 1
        st2in           : in Stage_1_2_Interface;

        -- inputs from hazard control
        reg1_fwd        : in FwdValue;
        reg2_fwd        : in FwdValue;

        -- outputs back to stage 1
        branch_flag     : out std_logic;
        branch_dest     : out MemWordAddr;

        -- outputs to stage 3
        st2out          : out Stage_2_3_Interface;

        -- outputs to IRAM
        iram_en         : out std_logic;
        iram_we         : out lvbit;
        iram_addr       : out MemWordAddr;
        iram_din        : out Instr_Type;

        -- outputs to DRAM
        dram_ena        : out std_logic;
        dram_wea        : out lvbit;
        dram_addra      : out DataByteAddr;
        dram_dina       : out Logic_Byte;
        dram_enb        : out std_logic;
        dram_web        : out lvbit;
        dram_addrb      : out DataByteAddr;
        dram_dinb       : out Logic_Byte;

        -- outputs to I/O register
        io_reg_we       : out lvbit;
        io_reg_inp      : out Logic_Byte);
end pl_stage2;


architecture Behavioral of pl_stage2 is

    COMPONENT alu
    PORT (
        op  : in Alu_Op_Type;
        a   : in Logic_Word;
        b   : in Logic_Word;
        res : out Logic_Word);
    END COMPONENT;

    -- value of $rs and $rt to actually use. may be different from
    -- st2in.value1/2.reg_val due to data hazards and forwarding.
    signal final_reg1_val : Logic_Word;
    signal final_reg2_val : Logic_Word;
    -- inputs for 'main_alu' instance
    signal alu_inp1 : Logic_Word;
    signal alu_inp2 : Logic_Word;

    signal alu_res : Logic_Word;

    signal cur_memobj : MemObject_Type;

begin

    final_reg1_val <= reg1_fwd.fwd_val when reg1_fwd.use_fwd = '1'
        else st2in.value1.reg_val;

    final_reg2_val <= reg2_fwd.fwd_val when reg2_fwd.use_fwd = '1'
        else st2in.value2.reg_val;


    alu_inp1 <= final_reg1_val when st2in.value1.use_reg = '1'
        else st2in.value1.imm;

    alu_inp2 <= final_reg2_val when st2in.value2.use_reg = '1'
        else st2in.value2.imm;


    -- copy values from stage 1 (with a few overrides)
    st2out.wr_type      <= wr_none when st2in.invalid_flag = '1'
                                else st2in.wr_type;
    st2out.wr_reg_idx   <= st2in.wr_reg_idx;
    st2out.pc_plus_2    <= st2in.pc_plus_2;


    main_alu : alu PORT MAP (
        op  => st2in.alu_op,
        a   => alu_inp1,    -- decided in st2_alu_proc
        b   => alu_inp2,    -- decided in st2_alu_proc
        res => alu_res);


    branch_proc : process(st2in.branch_type, st2in.branch_dest, final_reg2_val,
        st2in.invalid_flag)
    begin
        -- default
        branch_flag <= '0';
        branch_dest <= (others => '-');

        -- ignore branches for invalidated instructions
        if st2in.invalid_flag = '0' then

            case st2in.branch_type is
                when b_always_imm =>
                    branch_flag <= '1';
                    branch_dest <= st2in.branch_dest;

                when b_always_reg =>
                    branch_flag <= '1';
                    branch_dest <= word_to_mem_addr(final_reg2_val);

                when b_eqz =>
                    if final_reg2_val = "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when b_nez =>
                    if final_reg2_val /= "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when others =>
                    -- no branch, use default
            end case;

        end if;
    end process;


    st3_mem_inps_proc : process(alu_res, final_reg2_val, st2in.wr_type)
        -- 16-bit address we're reading/writing
        constant mem_addr   : Logic_Word    := alu_res;

        -- value to be written to mem_addr (when writing).
        -- we only ever write $rt, never alu_res
        constant wr_val     : Logic_Word    := final_reg2_val;

        -- hi & lo bytes of wr_val
        constant wr_val_hi  : Logic_Byte    := wr_val(15 downto 8);
        constant wr_val_lo  : Logic_Byte    := wr_val(7 downto 0);

        variable tmp_dram_addra, tmp_dram_addrb : DataByteAddr;
    begin
        -- default values:

        tmp_dram_addra := mem_addr(10 downto 0);
        tmp_dram_addrb := std_logic_vector(unsigned(tmp_dram_addra) + 1);

        dram_ena        <= '0';
        dram_wea        <= "0";
        dram_addra      <= tmp_dram_addra;
        dram_dina       <= (others => '-');
        dram_enb        <= '0';
        dram_web        <= "0";
        dram_addrb      <= tmp_dram_addrb;
        dram_dinb       <= (others => '-');
        iram_en         <= '0';
        iram_we         <= "0";
        iram_addr       <= mem_addr(13 downto 1);   -- ignore lsb
        iram_din        <= "00" & wr_val; -- TODO
        io_reg_we       <= "0";
        io_reg_inp      <= (others => '-');


        if mem_addr(15) = '1' then
            -- Memory-mapped IO
            cur_memobj <= mo_io;

            -- only byte writes are supported
            if mem_addr = iomem_addr_leds and st2in.wr_type = wr_reg_to_memb then
                io_reg_we <= "1";
                io_reg_inp <= wr_val_lo;
            end if;

        elsif mem_addr(14) = '1' then
            -- Instruction memory
            cur_memobj <= mo_iram;

            -- only word read/writes are supported
            case st2in.wr_type is
                when wr_reg_to_memw =>
                    iram_en    <= '1';
                    iram_we    <= "1";

                when wr_memw_to_reg =>
                    iram_en    <= '1';

                when others =>
                    -- use defaults
            end case;

        else
            -- Regular memory
            cur_memobj <= mo_dram;

            case st2in.wr_type is
                when wr_reg_to_memb =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= wr_val_lo;

                when wr_reg_to_memw =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= wr_val_hi;
                    dram_enb    <= '1';
                    dram_web    <= "1";
                    dram_dinb   <= wr_val_lo;

                when wr_memb_to_reg =>
                    dram_ena    <= '1';

                when wr_memw_to_reg =>
                    dram_ena    <= '1';
                    dram_enb    <= '1';

                when others =>
                    -- use defaults
            end case;
        end if;
    end process;

    st2out.alu_res <= alu_res;
    st2out.cur_memobj <= cur_memobj;

end Behavioral;
