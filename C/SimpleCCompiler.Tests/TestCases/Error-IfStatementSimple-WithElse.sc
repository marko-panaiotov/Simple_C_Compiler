﻿using System;

class Program
{	
	int main() //entry point
	{
		int i;
		i = 0;
		if (i>2)
			System.Console.Write("i is more than two");
		else
			System.Console.Write("i is less than two")
		return 0; //expectederror "Очаквам специален символ ';'"
	} //expectederror "Типа на резултата трябва да е съвместим с типа на метода" expectederror "Очаквам специален символ ';'"
} //expectederror "Очаквам специален символ '}'"