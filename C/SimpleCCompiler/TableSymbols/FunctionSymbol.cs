using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

namespace SimpleCCompiler.TableSymbols
{
    public class FunctionSymbol : TableSymbol
    {
        public Type returnType;
        public List<FormalParamSymbol> formalParams;
        public MethodInfo methodInfo;

        public FunctionSymbol(IdentToken token, Type returnType, List<FormalParamSymbol> formalParams, MethodInfo methodInfo) : base(token.line, token.column, token.value)
        {
            this.returnType = returnType;
            this.formalParams = formalParams;
            this.methodInfo = methodInfo;
        }

        public override string ToString()
        {
            StringBuilder s = new StringBuilder();
            s.AppendFormat("line {0}, column {1}: {2} - {3} methodsignature={4} {5}(", line, column, value, GetType(), returnType, value);
            foreach (FormalParamSymbol param in formalParams)
            {
                s.AppendFormat("{0} {1}, ", param.paramType, param.value);
            }
            if (formalParams.Count != 0) s.Remove(s.Length - 2, 2);
            s.Append(")");
            return s.ToString();
        }
    }
}
