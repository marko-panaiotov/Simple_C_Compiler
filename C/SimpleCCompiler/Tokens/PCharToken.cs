using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleCCompiler.Tokens
{
    public class PCharToken : LiteralToken
    {
        public char value;

        public PCharToken(int line, int column, char value) : base(line, column)
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
