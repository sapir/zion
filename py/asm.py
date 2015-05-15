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

grDecNumber = Combine(Optional('-') + Word(nums, asKeyword=True))
grHexNumber = Combine(Optional('-') + '0x' - Word(hexnums))
grNumber = grDecNumber | grHexNumber
grNumber.setParseAction(lambda s,loc,toks: int(toks[0], 0))

grImmediate = grNumber

grRegOffset = grNumber("ofs") + "(" - grRegister("reg") - ")"
grRegOffset.setParseAction(
    lambda s,loc,toks: RegOffset(toks['reg'], toks['ofs']))

grOperand = ~FollowedBy(grOpcode) + (
    grRegOffset("regofs")
    | grRegister("reg")
    | grImmediate("imm")
    | grLabelName("label"))
grOperand.setParseAction(lambda s,loc,toks: toks[0])

grOpcodeStmt = (grOpcode
    - Optional(delimitedList(grOperand) + ~FollowedBy(':')))
grOpcodeStmt.setParseAction(
    lambda s,loc,toks: OpcodeStmt(toks[0], toks[1:]))

grStatement = grLabel | grOpcodeStmt

grComment = ';' + restOfLine

grProgram = OneOrMore(grStatement)
grProgram.ignore(grComment)


def _encodeOperand(fieldName, value, curStmtIndex, labels):
    if fieldName in ('rd', 'rs', 'rt'):
        assert isinstance(value, Register)
        return REGISTER_NAMES[value]

    elif fieldName == 'regofs':
        assert isinstance(value, RegOffset)
        regNum = _encodeOperand('rs', value.reg, curStmtIndex, labels)

        ofs = value.ofs
        assert -8 <= ofs < 8
        ofs &= 0xf      # cast to unsigned

        return (ofs << 4) | regNum

    elif fieldName == 'imm8':
        assert 0x00 <= value <= 0xff, \
            "{!r} out of range for imm8".format(value)

        return value

    elif fieldName == 'imm4':
        assert 0x0 <= value <= 0xf, \
            "{!r} out of range for imm4".format(value)

        return value

    elif fieldName.startswith('addr'):
        assert isinstance(value, Label)

        nextStmtIndex = curStmtIndex + 1
        labelIndex = labels[value]
        ofs = labelIndex - nextStmtIndex

        if fieldName == 'addr8':
            assert -128 <= ofs < 128, \
                "{!r} out of range for addr8".format(ofs)

            return ofs & 0xff

        else:
            assert fieldName == 'addr12'
            assert -2048 <= ofs < 2048, \
                "{!r} out of range for addr12".format(ofs)

            return ofs & 0xfff

    else:
        raise ValueError("unknown field {!r}".format(fieldName))

def expandMacros(stmt):
    if stmt.opcode in ['add', 'sub', 'slt', 'sltu'] and len(stmt.operands) == 2:

        rd, rt = stmt.operands
        return [stmt.replace(operands=[rd, rt, rt])]

    elif stmt.opcode == 'nop':
        return [OpcodeStmt('add', [Register('$zero')] * 3)]

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
    allStmts = grProgram.parseString(code, parseAll=True)

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
        opndFieldNames = opcodeInfo.ifmt.operandFields.split()

        assert len(stmt.operands) == len(opndFieldNames)
        operands = [
            _encodeOperand(name, val, stmtIndex, labels)
            for (name, val)
            in zip(opndFieldNames, stmt.operands)]

        assembledWords.append(encodeML(stmt.opcode, operands))

    return assembledWords


if __name__ == '__main__':
    import sys
    from struct import pack


    inpFilename, outFilename = sys.argv[1:]

    code = open(inpFilename).read()

    words = assemble(code)

    out = open(outFilename, 'wb')
    for w in words:
        out.write(pack('>BH', w >> 16, w & 0xffff))
