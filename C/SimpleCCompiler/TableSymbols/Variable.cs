using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

namespace SimpleCCompiler.TableSymbols
{
    public class Variable : TableSymbol
    {
        public LocalVariableInfo localVariableInfo;
        public Variable(Token token, LocalVariableInfo localVariableInfo) : base(token.line, token.column, token.value)
        {
            this.localVariableInfo = localVariableInfo;
        }

        public override string ToString()
        {
            StringBuilder s = new StringBuilder();
            s.AppendFormat("line {0}, column {1}: {2} - {3} localvartype={4} localindex={5}", line, column, value, GetType(), localVariableInfo.LocalType, localVariableInfo.LocalIndex);
            return s.ToString();
        }
    }
}
