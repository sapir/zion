library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


-- Basically, we have to worry about the following data hazards:
-- 1. instr. in stage 1 preparing to use value (in stage 2) that is being
--      modified by instr. currently in stage 2
-- 2. instr. in stage 1 preparing to use value (in stage 2) that is being
--      modified by instr. currently in stage 3
--
-- Case 2 is simply handled by the register file returning the modified value
-- in the same clock cycle.
--
-- Note that any data hazard encountered by stage 2 will already have been
-- handled when instruction was in stage 1.
--
-- In addition, we have a control hazard on the instruction following a taken
-- branch. Those are currently handled by stage 1 itself, by invalidating that
-- instruction while taking the branch.


entity hazardctl is
    Port (
        clk                 : in std_logic;

        -- registers that stage 1 is reading
        st1_reg1_idx        : in Reg_Index;
        st1_reg2_idx        : in Reg_Index;

        -- register that stage 2 is writing (and how)
        st2_wr_type         : in Write_Type;
        st2_wr_reg_idx      : in Reg_Index;

        -- values that just arrived at stage 3
        st3_alu_res         : in Logic_Word;
        st3_pc_plus_2       : in MemWordAddr;

        -- outputs:
        -- tell stage 2 whether to override its inputs (due to data
        -- hazard detected in previous instruction's stage 1)
        st2_reg1_fwd        : out FwdValue;
        st2_reg2_fwd        : out FwdValue;
        -- tell stage 1 whether to stall
        st1_stall_flag      : out std_logic);
end hazardctl;


architecture Behavioral of hazardctl is

    -- source of register values to use in stage 2
    -- (from perspective of instruction running in stage 2, though this value
    -- is set in stage 1)
    type RegValue_Src is (
        -- use register value read in stage 1 (no forwarding)
        rvs_st1,
        -- use ALU res passed to stage 3 from previous instruction's stage 2
        rvs_st3_alu,
        -- use $pc + 2 value passed to stage 3
        rvs_st3_pc_plus_2);

    signal st1_reg1_src, st1_reg2_src : RegValue_Src;
    signal st2_reg1_src, st2_reg2_src : RegValue_Src;

begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            -- set values for stage 2 from values decided for previous cycle's
            -- stage 1
            st2_reg1_src <= st1_reg1_src;
            st2_reg2_src <= st1_reg2_src;
        end if;
    end process;

    with st2_reg1_src select st2_reg1_fwd <=
        (use_fwd => '1', fwd_val => st3_alu_res)        when rvs_st3_alu,
        (use_fwd => '1', fwd_val => mem_addr_to_word(st3_pc_plus_2))
                                                        when rvs_st3_pc_plus_2,
        (use_fwd => '0', fwd_val => (others => '-'))    when rvs_st1,
        (use_fwd => '0', fwd_val => (others => '-'))    when others;

    with st2_reg2_src select st2_reg2_fwd <=
        (use_fwd => '1', fwd_val => st3_alu_res)        when rvs_st3_alu,
        (use_fwd => '1', fwd_val => mem_addr_to_word(st3_pc_plus_2))
                                                        when rvs_st3_pc_plus_2,
        (use_fwd => '0', fwd_val => (others => '-'))    when rvs_st1,
        (use_fwd => '0', fwd_val => (others => '-'))    when others;


    st1_data_hazard_proc : process(st1_reg1_idx, st1_reg2_idx,
        st2_wr_type, st2_wr_reg_idx)
    begin
        -- default: use values from stage 1 unless there's a data hazard,
        -- and don't stall.
        st1_reg1_src <= rvs_st1;
        st1_reg2_src <= rvs_st1;
        st1_stall_flag <= '0';

        -- forward values from stage 2
        case st2_wr_type is
            when wr_alu_to_reg =>
                -- if stage 2 is calculating value to be written with ALU,
                -- then instruction in stage 1 should use that value once it
                -- (the instr.) reaches stage 2 and the value reaches stage 3.
                if st2_wr_reg_idx = st1_reg1_idx then
                    st1_reg1_src <= rvs_st3_alu;
                end if;

                if st2_wr_reg_idx = st1_reg2_idx then
                    st1_reg2_src <= rvs_st3_alu;
                end if;

            when wr_memb_to_reg | wr_memw_to_reg =>
                -- if stage 2 wants to read value from memory, no way we can
                -- handle it in time. stage 1 will have to wait one cycle.
                st1_stall_flag <= '1';

            when wr_pc_plus_2_to_ra =>
                -- like case of writing ALU result, but using $pc+2 instead
                if ra_reg_idx = st1_reg1_idx then
                    st1_reg1_src <= rvs_st3_pc_plus_2;
                end if;

                if ra_reg_idx = st1_reg2_idx then
                    st1_reg2_src <= rvs_st3_pc_plus_2;
                end if;

            when others =>
                -- no data hazard
        end case;
    end process;

end Behavioral;
