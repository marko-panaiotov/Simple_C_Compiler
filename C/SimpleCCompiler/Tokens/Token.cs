
namespace SimpleCCompiler
{
	public abstract class Token
	{
		public int line;
		public int column;
        public string value;

        public Token(int line, int column)
		{
			this.line = line;
			this.column = column;
		}

        public Token(int line, int column, string value)
        {
            this.line = line;
            this.column = column;
            this.value = value;
        }

    }
}
