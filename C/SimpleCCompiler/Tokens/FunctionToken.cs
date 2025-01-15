using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCCompiler.Tokens
{
    public class FunctionToken : Token
    {
        public FunctionToken(int line, int column, string value) : base(line, column, value)
        {
            this.value = value;
        }

        public override string ToString()
        {
            StringBuilder s = new StringBuilder();
            s.AppendFormat("line {0}, column {1}: {2} - {3}", line, column, value, GetType());
            return s.ToString();
        }
    }
}
