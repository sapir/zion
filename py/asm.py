#!/usr/bin/python
from collections import namedtuple
from pyparsing import *
from ml import *


class StringObject(str):
    def __repr__(self):
        return '{0}({1})'.format(
            self.__class__.__name__,
            str.__repr__(self))

class Label(StringObject): pass
class Opcode(StringObject): pass
class Register(StringObject): pass

RegOffset = namedtuple('RegOffset', 'reg ofs')
OpcodeStmt = namedtuple('OpcodeStmt', 'opcode operands')


MACRO_OPCODES = 'nop li move'.split()


def oneOfKeywords(words):
    if isinstance(words, basestring):
        words = words.split()

    return reduce(operator.or_, map(Keyword, words))


grLabelName = Word('_' + alphas, '_' + alphanums)
grLabelName.setParseAction(lambda s,loc,toks: Label(toks[0]))

grLabel = grLabelName + Suppress(':') - Empty()
# grLabel.setParseAction(lambda s,loc,toks: toks[0])

grOpcode = oneOfKeywords(OPCODES.keys() + MACRO_OPCODES)
grOpcode.setParseAction(lambda s,loc,toks: Opcode(toks[0]))

grRegister = oneOfKeywords(REGISTER_NAMES.keys())
grRegister.setParseAction(lambda s,loc,toks: Register(toks[0]))

grDecNumber = Word(nums, asKeyword=True)
grHexNumber = Combine('0x' - Word(hexnums))
grNumber = grDecNumber | grHexNumber
grNumber.setParseAction(lambda s,loc,toks: int(toks[0], 0))

grImmediate = grNumber

grRegOffset = grNumber("ofs") + "(" - grRegister("reg") - ")"
grRegOffset.setParseAction(
    lambda s,loc,toks: RegOffset(toks['reg'], toks['ofs']))

grOperand = (
    grRegOffset("regofs")
    | grRegister("reg")
    | grImmediate("imm")
    | grLabelName("label"))
grOperand.setParseAction(lambda s,loc,toks: toks[0])

grOpcodeStmt = (grOpcode
    - Optional(delimitedList(grOperand)))
grOpcodeStmt.setParseAction(
    lambda s,loc,toks: OpcodeStmt(toks[0], toks[1:]))

grStatement = grLabel | grOpcodeStmt

grProgram = OneOrMore(grStatement)


def _encodeOperand(value, typeName, curStmtIndex, labels):
    if typeName == 'reg':
        assert isinstance(value, Register)
        return REGISTER_NAMES[value]

    elif typeName == 'regofs':
        assert isinstance(value, RegOffset)
        regNum = _encodeOperand(value.reg, 'reg', curStmtIndex, labels)

        ofs = value.ofs
        assert -8 <= ofs < 8
        ofs &= 0xf      # cast to unsigned

        return (ofs << 4) | regNum

    elif typeName == 'imm8':
        assert 0x00 <= value <= 0xff
        return value

    elif typeName == 'imm4':
        assert 0x0 <= value <= 0xf
        return value

    elif typeName.startswith('addr'):
        assert isinstance(value, Label)

        nextStmtIndex = curStmtIndex + 1
        labelIndex = labels[value]
        ofs = labelIndex - nextStmtIndex

        if typeName == 'addr7':
            assert -64 <= ofs < 64
            return ofs & 0x7f
        else:
            assert typeName == 'addr11'
            assert -1024 <= ofs < 1024
            return ofs & 0x7ff

def expandMacros(stmt):
    if stmt.opcode in ['add', 'sub', 'slt', 'sltu'] and len(stmt.operands) == 2:

        rd, rt = stmt.operands
        return [stmt.replace(operands=[rd, rt, rt])]

    elif stmt.opcode == 'nop':
        return [OpcodeStmt('add', [0, 0, 0])]

    elif stmt.opcode == 'li':
        reg, value = stmt.operands

        # cast to unsigned
        value &= 0xffff

        upperByte = value >> 8
        lowerByte = value & 0xff

        if upperByte == 0:
            # can be done with lower
            return [OpcodeStmt('li8', [reg, lowerByte])]
        elif lowerByte == 0:
            # can be done with upper
            return [OpcodeStmt('lui', [reg, upperByte])]
        else:
            # both upper and lower are needed
            return [
                OpcodeStmt('lui', [reg, upperByte]),
                OpcodeStmt('ori', [reg, lowerByte]),
                ]

    elif stmt.opcode == 'move':
        rd, rt = stmt.operands
        return [OpcodeStmt('add', [rd, Register('$zero'), rt])]

    return [stmt]

def assemble(code):
    allStmts = grProgram.parseString(testProg, parseAll=True)

    # first pass: find labels, expand macros
    labels = {}
    notLabelStmts = []
    for stmt in allStmts:
        if isinstance(stmt, Label):
            curStmtIndex = len(notLabelStmts)
            labels[stmt] = curStmtIndex
        else:
            notLabelStmts += expandMacros(stmt)

    # second pass: execute statements
    assembledWords = []
    for stmtIndex, stmt in enumerate(notLabelStmts):
        assert isinstance(stmt, OpcodeStmt)

        opcodeInfo = OPCODES[stmt.opcode]
        opndTypes = opcodeInfo.ifmt.operandTypes.split()

        assert len(stmt.operands) == len(opndTypes)
        operands = [
            _encodeOperand(opnd, opndType, stmtIndex, labels)
            for (opnd, opndType)
            in zip(stmt.operands, opndTypes)]

        fields = [opcodeInfo.id] + operands
        fields = permute(fields, opcodeInfo.ifmt.operandOrder)
        word = joinBitFields(fields, opcodeInfo.ifmt.fieldWidths)
        assembledWords.append(word)

    return assembledWords


if __name__ == '__main__':
    testProg = open('test.s', 'r').read()
    words = assemble(testProg)

    from emu import ZionEmulator
    emu = ZionEmulator()
    emu.setMem(*words)
    emu.run()

    print(emu.regs)
