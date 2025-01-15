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
        public FormalParamSymbol[] formalParams;
        public MethodInfo methodInfo;

        public FunctionSymbol(Token token, Type returnType, FormalParamSymbol[] formalParams, MethodInfo methodInfo)
        : base(token.line, token.column, token.value)
        {
            this.returnType = returnType;
            this.formalParams = formalParams;
            this.methodInfo = methodInfo;
        }
    }
}
