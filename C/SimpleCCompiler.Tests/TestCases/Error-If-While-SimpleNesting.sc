﻿using System;

class Program
{	
	int main() //entry point
	{
		int i;
		i = 3;
		if (i>2) {
			while (i<10) {
				System.Console.WriteLine(i);
				i++
			} //expectederror "Очаквам специален символ ';'"
		}
			
		return 0;
	}
} //expectederror "Очаквам специален символ '}'"